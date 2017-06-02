dg = :digraph.new
for i <- 1..10_000, do: :digraph.add_vertex(dg, i)
for i <- 10_000..2, j = div(i, 2), do: :digraph.add_edge(dg, i, j)

g = Graph.new
g = Enum.reduce(1..10_000, g, fn i, g -> Graph.add_vertex(g, i) end)
g = Enum.reduce(10_000..2, g, fn i, g -> Graph.add_edge(g, i, div(i, 2)) end)

Benchee.run(%{time: 10}, %{
      "digraph (get_short_path)" => fn ->
              :digraph.get_short_path(dg, 10_000, 1)
            end,
      "libgraph (get_shortest_path)" => fn ->
        Graph.get_shortest_path(g, 10_000, 1)
      end
})
