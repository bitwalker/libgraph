Code.require_file(Path.join([__DIR__, "..", "test", "support", "generators.ex"]))

alias Graph.Test.Generators

g = Generators.dag(10_000)
dg = Generators.libgraph_to_digraph(g)

g2 = Generators.biased_dag(10_000)
dg2 = Generators.libgraph_to_digraph(g2)

opts = %{time: 10, inputs: %{"unbiased" => {g, dg}, "biased" => {g2, dg2}}}

Benchee.run(opts, %{
      "digraph (get_short_path)" => fn {_, dg} ->
        length(:digraph.get_short_path(dg, 1, 10_000)) != 0
      end,
      "libgraph (dijkstra)" => fn {g, _} ->
        length(Graph.dijkstra(g, 1, 10_000)) != 0
      end,
      "libgraph (a_star)" => fn {g, _} ->
        length(Graph.a_star(g, 1, 10_000, fn v ->
              case rem(v, 2) do
                0 -> 0
                _ -> 10_000+v
              end
            end)) != 0
      end,
})
