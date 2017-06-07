Code.require_file(Path.join([__DIR__, "support", "generators.ex"]))

alias Graph.Bench.Generators

g = Generators.dag(10_000)
dg = Generators.libgraph_to_digraph(g)

Benchee.run(%{time: 10}, %{
      "digraph (topsort)" => fn ->
        :digraph_utils.topsort(dg)
      end,
      "libgraph (topsort)" => fn ->
        Graph.topsort(g)
      end
})
