# Run with: mix run bench/all_paths.exs
# Fixed, bounded fixtures separate graph size from path count. Each search runs
# in an isolated worker with a 10s deadline. Sampled memory peaks are lower bounds,
# not allocation peaks/RSS; unshared result size excludes sharing optimizations.
defmodule Graph.Bench.AllPaths do
  import Bitwise, only: [bsl: 2]

  def run do
    IO.puts("# Elixir #{System.version()}, OTP #{System.otp_release()}")

    IO.puts(
      "fixture,vertices,edges,max_depth_vertices,paths,us,reductions,start_bytes,end_bytes,sampled_peak_bytes,unshared_result_bytes"
    )

    fixtures =
      Enum.map([1000, 4000, 11_000], &{"chain_#{&1}", fn -> chain(&1) end}) ++
        Enum.map([8, 12, 16], &{"layered_2x#{&1}", fn -> layered(&1, true) end}) ++
        [
          {"unreachable_2x16", fn -> layered(16, false) end},
          {"binary_tree", &tree/0},
          {"cyclic_chain", &cyclic_chain/0}
        ]

    for {name, fixture} <- fixtures do
      {graph, source, target, depth} = fixture.()
      parent = self()

      {pid, monitor} =
        spawn_monitor(fn ->
          :erlang.garbage_collect()
          {:memory, start_bytes} = Process.info(self(), :memory)
          {:reductions, before} = Process.info(self(), :reductions)
          {us, result} = :timer.tc(fn -> Graph.get_paths(graph, source, target) end)
          {:reductions, after_search} = Process.info(self(), :reductions)
          {:memory, end_bytes} = Process.info(self(), :memory)
          result_bytes = :erts_debug.flat_size(result) * :erlang.system_info(:wordsize)

          send(
            parent,
            {self(),
             [length(result), us, after_search - before, start_bytes, end_bytes, result_bytes]}
          )
        end)

      metrics = await(pid, monitor, 0, System.monotonic_time(:millisecond) + 10_000)

      IO.puts(
        Enum.join([name, Graph.num_vertices(graph), Graph.num_edges(graph), depth | metrics], ",")
      )
    end
  end

  defp await(pid, monitor, peak, deadline) do
    receive do
      {^pid, [count, us, reductions, start_bytes, end_bytes, result_bytes]} ->
        Process.demonitor(monitor, [:flush])

        [
          count,
          us,
          reductions,
          start_bytes,
          end_bytes,
          max(peak, max(start_bytes, end_bytes)),
          result_bytes
        ]

      {:DOWN, ^monitor, :process, ^pid, reason} ->
        raise "path benchmark worker exited: #{inspect(reason)}"
    after
      1 ->
        if System.monotonic_time(:millisecond) >= deadline do
          Process.exit(pid, :kill)
          Process.demonitor(monitor, [:flush])
          raise "path benchmark exceeded its 10s search deadline"
        end

        memory =
          case Process.info(pid, :memory) do
            {:memory, bytes} -> bytes
            nil -> 0
          end

        await(pid, monitor, max(peak, memory), deadline)
    end
  end

  defp graph(vertices, edges) do
    Graph.new(vertex_identifier: fn v -> v end)
    |> Graph.add_vertices(vertices)
    |> Graph.add_edges(edges)
  end

  defp chain(count) do
    {graph(Enum.to_list(1..count), Enum.map(1..(count - 1), &{&1, &1 + 1})), 1, count, count}
  end

  defp layered(layers, reachable?) do
    target = 2 * layers + 2
    groups = [[1]] ++ Enum.map(0..(layers - 1), &Enum.to_list((2 + 2 * &1)..(3 + 2 * &1)))
    groups = if reachable?, do: groups ++ [[target]], else: groups

    edges =
      groups
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.flat_map(fn [froms, tos] -> for from <- froms, to <- tos, do: {from, to} end)

    {graph(Enum.to_list(1..target), edges), 1, target, layers + if(reachable?, do: 2, else: 1)}
  end

  defp tree do
    count = bsl(1, 13) - 1
    edges = Enum.flat_map(1..div(count, 2), fn v -> [{v, 2 * v}, {v, 2 * v + 1}] end)
    {graph(Enum.to_list(1..count), edges), 1, count, 13}
  end

  defp cyclic_chain do
    {graph, source, target, depth} = chain(1000)
    {Graph.add_edges(graph, Enum.map(2..1000, &{&1, &1 - 1})), source, target, depth}
  end
end

Graph.Bench.AllPaths.run()
