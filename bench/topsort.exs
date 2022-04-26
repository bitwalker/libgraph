Code.require_file(Path.join([__DIR__, "..", "test", "support", "generators.ex"]))

alias Graph.Test.Generators

g = Generators.dag(10_000)
dg = Generators.libgraph_to_digraph(g)

opts = [
  time: 10,
  inputs: %{
    "10k vertices, #{map_size(g.out_edges)} edges" => {g, dg}
  }
]

Benchee.run(
  %{
      "digraph (topsort)" => fn {_, dg}->
        :digraph_utils.topsort(dg)
      end,
      "libgraph (topsort)" => fn {g, _} ->
        Graph.topsort(g)
      end
  },
  opts
)
