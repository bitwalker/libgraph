# Run with: mix run bench/vertex_identity.exs --vertices 10000 --samples 5
# Each sample builds a directed chain in an isolated process. Vertex construction
# is outside build timing, except for rebuilt source endpoints. Lookups construct
# fresh equal vertices. Memory is worker process memory after a full GC with only
# the graph retained; it includes heap capacity and process overhead. These inputs
# contain no binary payloads, so off-heap binary memory is not part of this metric.
defmodule Graph.Bench.VertexIdentity do
  def run do
    {opts, args, invalid} =
      OptionParser.parse(System.argv(), strict: [vertices: :integer, samples: :integer])

    count = Keyword.get(opts, :vertices, 10_000)
    samples = Keyword.get(opts, :samples, 5)

    unless args == [] and invalid == [] and count >= 2 and samples >= 1 do
      raise ArgumentError, "expected --vertices >= 2 and --samples >= 1"
    end

    IO.puts("# Elixir #{System.version()}, OTP #{System.otp_release()}")
    IO.puts("# #{:erlang.system_info(:system_architecture)}, #{samples} samples per case")
    IO.puts("# Median timings in microseconds; worker memory in bytes after GC")

    IO.puts(
      "input,endpoints,identifier,requested,vertices,edges,hits,build_us,lookup_us,memory_bytes"
    )

    inputs = [
      {"integers", fn id -> id end, nil},
      {"small_maps", fn id -> %{id: id} end, &Map.fetch!(&1, :id)},
      {"structured_maps", fn id -> %{id: id, fields: Enum.map(1..32, &{&1, id + &1})} end,
       &Map.fetch!(&1, :id)}
    ]

    defaults = [{"phash2", &Graph.Utils.vertex_id/1}, {"identity", fn vertex -> vertex end}]

    for {name, vertex, application_id} <- inputs, endpoints <- [:reused, :rebuilt] do
      identifiers =
        if application_id,
          do: defaults ++ [{"application_id", application_id}],
          else: defaults

      # Warm both implementations, then alternate their order between samples.
      for {_, identifier} <- identifiers, do: run_sample(vertex, identifier, endpoints, count)

      results =
        for index <- 1..samples,
            {name, identifier} <-
              if(rem(index, 2) == 0, do: Enum.reverse(identifiers), else: identifiers) do
          {name, run_sample(vertex, identifier, endpoints, count)}
        end

      for {identifier, _} <- identifiers do
        measurements = for {^identifier, result} <- results, do: result

        metrics =
          for key <- [:vertices, :edges, :hits, :build_us, :lookup_us, :memory_bytes],
              do: median(measurements, key)

        IO.puts(Enum.join([name, endpoints, identifier, count | metrics], ","))
      end
    end
  end

  defp run_sample(vertex, identifier, endpoints, count) do
    Task.async(fn -> sample(vertex, identifier, endpoints, count) end)
    |> Task.await(:infinity)
  end

  defp sample(vertex, identifier, endpoints, count) do
    vertices = Enum.map(1..count, vertex)

    {build_us, {graph, _, _}} =
      :timer.tc(fn ->
        Enum.reduce(vertices, {Graph.new(vertex_identifier: identifier), nil, 1}, fn
          value, {graph, _, 1} ->
            {Graph.add_vertex(graph, value), value, 2}

          value, {graph, previous, index} ->
            source = if endpoints == :rebuilt, do: vertex.(index - 1), else: previous
            {Graph.add_edge(graph, source, value), value, index + 1}
        end)
      end)

    {lookup_us, hits} =
      :timer.tc(fn -> Enum.count(1..count, &Graph.has_vertex?(graph, vertex.(&1))) end)

    measure(graph, build_us, lookup_us, hits)
  end

  defp measure(graph, build_us, lookup_us, hits) do
    :erlang.garbage_collect()
    {:memory, memory_bytes} = Process.info(self(), :memory)

    %{
      vertices: Graph.num_vertices(graph),
      edges: Graph.num_edges(graph),
      hits: hits,
      build_us: build_us,
      lookup_us: lookup_us,
      memory_bytes: memory_bytes
    }
  end

  defp median(measurements, key) do
    sorted = measurements |> Enum.map(&Map.fetch!(&1, key)) |> Enum.sort()
    middle = div(length(sorted), 2)

    if rem(length(sorted), 2) == 0 do
      (Enum.at(sorted, middle - 1) + Enum.at(sorted, middle)) / 2
    else
      Enum.at(sorted, middle)
    end
  end
end

Graph.Bench.VertexIdentity.run()
