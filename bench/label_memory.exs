# Run with: mix run bench/label_memory.exs
# Finite isolated-worker scenarios compare latest-only updates with deliberately
# retained graph history, both directly and through minimal GenServer wrappers.
# Explicit GC is a diagnostic control, not a production library policy. Samples
# are checkpoints, not peaks/RSS; process memory excludes off-heap binary bytes.
# Binary totals deduplicate backing addresses within each process only.
defmodule Graph.Bench.LabelMemory.Worker do
  def graph(kind), do: Graph.new() |> Graph.add_vertex(:vertex, initial(kind))
  def initial(:scalar), do: 0
  def initial(_), do: %{"id" => 1}

  def replacement(:scalar, _old, iteration), do: rem(iteration, 2)
  def replacement(:map, old, _iteration), do: Map.put(old, "foo", true)

  def replacement(kind, old, iteration) do
    # Runtime bytes prevent a compiler literal from masking binary sharing.
    binary =
      :binary.copy(<<rem(iteration, 251)>>, if(kind == :subbinary, do: 1_048_576, else: 131_072))

    payload =
      if kind == :subbinary, do: :binary.part(binary, rem(iteration, 128), 131_072), else: binary

    old |> Map.put("foo", true) |> Map.put("payload", payload)
  end

  def replace(graph, label) do
    graph |> Graph.remove_vertex_labels(:vertex) |> Graph.label_vertex(:vertex, label)
  end

  def summary(graph, history_count) do
    [label] = Graph.vertex_labels(graph, :vertex)
    payload = if is_map(label), do: Map.get(label, "payload", <<>>), else: <<>>

    %{
      vertices: map_size(graph.vertices),
      label_entries: map_size(graph.vertex_labels),
      labels:
        Enum.reduce(graph.vertex_labels, 0, fn {_, labels}, acc -> acc + length(labels) end),
      history: history_count,
      payload_bytes: byte_size(payload),
      referenced_payload_bytes: :binary.referenced_byte_size(payload)
    }
  end

  def loop(graph, kind, iteration, history, retain?) do
    receive do
      {:run, parent, count} ->
        {graph, history} =
          Enum.reduce(1..count, {graph, history}, fn step, {graph, history} ->
            [old] = Graph.vertex_labels(graph, :vertex)
            label = replacement(kind, old, iteration + step)
            updated = replace(graph, label)
            {updated, if(retain?, do: [graph | history], else: history)}
          end)

        send(parent, {:done, self()})
        loop(graph, kind, iteration + count, history, retain?)

      {:summary, parent} ->
        send(parent, {:summary, self(), summary(graph, length(history))})
        loop(graph, kind, iteration, history, retain?)

      :stop ->
        :ok
    end
  end
end

defmodule Graph.Bench.LabelMemory.Server do
  use GenServer
  def init(kind), do: {:ok, Graph.Bench.LabelMemory.Worker.graph(kind)}

  def handle_call({:get_by_label, id}, _from, graph) do
    vertices =
      Enum.filter(Graph.vertices(graph), fn vertex ->
        case Graph.vertex_labels(graph, vertex) do
          [%{"id" => ^id}] -> true
          _ -> false
        end
      end)

    {:reply, vertices, graph}
  end

  def handle_call({:get_label, vertex}, _from, graph),
    do: {:reply, Graph.vertex_labels(graph, vertex), graph}

  def handle_call({:remove, vertex}, _from, graph),
    do: {:reply, :ok, Graph.remove_vertex_labels(graph, vertex)}

  def handle_call({:add, vertex, label}, _from, graph),
    do: {:reply, :ok, Graph.label_vertex(graph, vertex, label)}

  def handle_call(:summary, _from, graph),
    do: {:reply, Graph.Bench.LabelMemory.Worker.summary(graph, 0), graph}
end

defmodule Graph.Bench.LabelMemory do
  def run do
    IO.puts(
      "Elixir #{System.version()}, OTP #{System.otp_release()}, ERTS #{:erlang.system_info(:version)}"
    )

    IO.puts(
      "mode,kind,iterations,phase,memory,binary_count,binary_bytes,mailbox,minor_gcs,vertices,label_entries,labels,history,payload_bytes,referenced_payload_bytes"
    )

    for mode <- [:direct, :genserver], kind <- [:map, :owned, :subbinary] do
      scenario(mode, kind, false, [0, 100, 1000, 5000])
    end

    scenario(:direct, :scalar, false, [0, 100, 1000, 10000])
    scenario(:direct, :owned, true, [0, 25, 50, 100])
    scenario(:direct, :subbinary, true, [0, 25, 50, 100])
  end

  def scenario(:direct, kind, retain?, checkpoints) do
    parent = self()

    pid =
      spawn_link(fn ->
        Graph.Bench.LabelMemory.Worker.loop(
          Graph.Bench.LabelMemory.Worker.graph(kind),
          kind,
          0,
          [],
          retain?
        )
      end)

    mode = if retain?, do: :history, else: :direct

    Enum.reduce(checkpoints, 0, fn checkpoint, previous ->
      if checkpoint > previous do
        send(pid, {:run, parent, checkpoint - previous})

        receive do
          {:done, ^pid} -> :ok
        end
      end

      send(pid, {:summary, parent})

      summary =
        receive do
          {:summary, ^pid, summary} -> summary
        end

      measure(mode, kind, checkpoint, pid, summary)
      checkpoint
    end)

    send(pid, :stop)
  end

  def scenario(:genserver, kind, false, checkpoints) do
    {:ok, pid} = GenServer.start_link(Graph.Bench.LabelMemory.Server, kind)

    Enum.reduce(checkpoints, 0, fn checkpoint, previous ->
      if checkpoint > previous, do: run_server_batch(pid, kind, previous, checkpoint)
      summary = GenServer.call(pid, :summary)
      measure(:genserver, kind, checkpoint, pid, summary)
      checkpoint
    end)

    GenServer.stop(pid)
  end

  # This call frame disappears before sampling, so caller temporaries do not retain labels.
  def run_server_batch(pid, kind, previous, checkpoint) do
    Enum.each((previous + 1)..checkpoint, fn iteration ->
      [vertex] = GenServer.call(pid, {:get_by_label, 1})
      [old] = GenServer.call(pid, {:get_label, vertex})
      label = Graph.Bench.LabelMemory.Worker.replacement(kind, old, iteration)
      :ok = GenServer.call(pid, {:remove, vertex})
      :ok = GenServer.call(pid, {:add, vertex, label})
    end)
  end

  def measure(mode, kind, checkpoint, pid, summary) do
    print(mode, kind, checkpoint, :before_gc, pid, summary)
    :erlang.garbage_collect(pid)
    print(mode, kind, checkpoint, :after_gc, pid, summary)
  end

  def print(mode, kind, checkpoint, phase, pid, summary) do
    info = Map.new(Process.info(pid, [:memory, :binary, :message_queue_len, :garbage_collection]))
    binaries = Map.new(info.binary, fn {id, bytes, _refs} -> {id, bytes} end)

    values =
      [
        mode,
        kind,
        checkpoint,
        phase,
        info.memory,
        map_size(binaries),
        Enum.sum(Map.values(binaries)),
        info.message_queue_len,
        info.garbage_collection[:minor_gcs]
      ] ++
        Enum.map(
          [
            :vertices,
            :label_entries,
            :labels,
            :history,
            :payload_bytes,
            :referenced_payload_bytes
          ],
          &Map.fetch!(summary, &1)
        )

    IO.puts(Enum.join(values, ","))
  end
end

Graph.Bench.LabelMemory.run()
