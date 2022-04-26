opts = [
  time: 10,
  inputs: %{
    "1M vertices, 500k edges" => 1_000_000,
    "100k vertices, 50k edges" => 100_000,
    "10k vertices, 5k edges" => 10_000
  }
]

Benchee.run(
  %{
    "digraph" => fn size ->
      dg = :digraph.new
      for i <- 1..size, do: :digraph.add_vertex(dg, %{num: i})
      for i <- 1..size, rem(i, 2) == 0, do: :digraph.add_edge(dg, %{num: i}, %{num: i - 1})
    end,
    "libgraph" => fn size ->
      g = Graph.new
      ga = Enum.reduce(1..size, g, fn i, g -> Graph.add_vertex(g, %{num: i}) end)
      Enum.reduce(1..size, ga, fn
        i, g when rem(i, 2) == 0 ->
          Graph.add_edge(g, %{num: i}, %{num: i-1})
        _, g ->
          g
      end)
    end
  },
  opts
)
