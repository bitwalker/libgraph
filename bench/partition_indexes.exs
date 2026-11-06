# Run the identical script against baseline and optimized dependencies.
# Compilation/setup and explicit GC are outside timing. No concurrent runs.
# Example: ERL_FLAGS='+S 4:4' mix run bench/partition_indexes.exs 128 512 2048 8192
# Query loops hold selected degree at one while unrelated history grows.
# A single relabel starts from the same immutable graph on every repetition.
# consume_all changes n distinct edges and retains the resulting history.
defmodule Graph.Bench.PartitionIndexes do

  @queries 500

  def run(sizes) do
    IO.puts(
      "operation,n,trial,iterations,microseconds,reductions,post_gc_bytes,result_flat_words"
    )

    for n <- sizes do
      graph = build(n)

      jobs = [
        {"incoming_partition", @queries,
         fn -> repeat(@queries, fn -> Graph.in_edges(graph, :hub, by: :fan_in) end) end},
        {"outgoing_partition", @queries,
         fn -> repeat(@queries, fn -> Graph.out_edges(graph, :hub, by: :flow) end) end},
        {"incident_partition", @queries,
         fn -> repeat(@queries, fn -> Graph.edges(graph, :hub, by: :flow) end) end},
        {"single_relabel", @queries,
         fn ->
           repeat(@queries, fn ->
             Graph.update_labelled_edge(graph, {:pending, 1}, :hub, :runnable, label: :ran)
           end)
         end},
        {"consume_all", n,
         fn ->
           Enum.reduce(1..n, graph, fn i, acc ->
             Graph.update_labelled_edge(acc, {:pending, i}, :hub, :runnable, label: :ran)
           end)
         end}
      ]

      for {name, iterations, work} <- jobs do
        isolated(work)

        for trial <- 1..3 do
          {time, reductions, memory, words} = isolated(work)
          IO.puts(Enum.join([name, n, trial, iterations, time, reductions, memory, words], ","))
        end
      end
    end
  end

  defp build(n) do
    Enum.reduce(1..n, Graph.new(multigraph: true), fn i, g ->
      g
      |> Graph.add_edge({:history, i}, :hub, label: :ran)
      |> Graph.add_edge(:hub, {:output, i}, label: :produced)
      |> Graph.add_edge({:pending, i}, :hub, label: :runnable)
    end)
    |> Graph.add_edge(:source, :hub, label: :fan_in)
    |> Graph.add_edge(:hub, :sink, label: :flow)
  end

  defp repeat(n, work), do: Enum.reduce(1..n, nil, fn _, _ -> work.() end)

  defp isolated(work) do
    caller = self()
    ref = make_ref()

    {pid, monitor} =
      spawn_monitor(fn ->
        :erlang.garbage_collect()
        {:reductions, before} = Process.info(self(), :reductions)
        {time, result} = :timer.tc(work)
        {:reductions, after_work} = Process.info(self(), :reductions)
        :erlang.garbage_collect()
        {:memory, memory} = Process.info(self(), :memory)
        words = :erts_debug.flat_size(result)
        send(caller, {ref, time, after_work - before, memory, words})
      end)

    receive do
      {^ref, time, reductions, memory, words} ->
        Process.demonitor(monitor, [:flush])
        {time, reductions, memory, words}

      {:DOWN, ^monitor, :process, ^pid, reason} ->
        raise "probe failed: #{inspect(reason)}"
    after
      120_000 ->
        Process.exit(pid, :kill)
        raise "probe exceeded 120 seconds"
    end
  end
end

sizes =
  case System.argv() do
    [] -> [128, 512, 2048, 8192]
    args -> Enum.map(args, &String.to_integer/1)
  end

Graph.Bench.PartitionIndexes.run(sizes)
