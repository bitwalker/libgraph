Benchee.run(%{time: 10}, %{
      "digraph (build)" => fn ->
        dg = :digraph.new
        for i <- 1..1_000_000, do: :digraph.add_vertex(dg, %{num: i})
        for i <- 1..1_000_000, rem(i, 2) == 0, do: :digraph.add_edge(dg, %{num: i}, %{num: i - 1})
      end,
      "libgraph (build)" => fn ->
        g = Graph.new
        ga = Enum.reduce(1..1_000_000, g, fn i, g -> Graph.add_vertex(g, %{num: i}) end)
        Enum.reduce(1..1_000_000, ga, fn
          i, g when rem(i, 2) == 0 ->
            Graph.add_edge(g, %{num: i}, %{num: i-1})
          _, g ->
            g
        end)
      end
})
