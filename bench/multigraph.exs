defmodule MultigraphBench.Helpers do
  def build_graphs(num_vertices, num_edges, num_labels) do
    labels = Enum.map(1..num_labels, fn i -> :"label_#{i}" end)

    edges =
      for _ <- 1..num_edges do
        v1 = :rand.uniform(num_vertices)
        v2 = :rand.uniform(num_vertices)
        label = Enum.random(labels)
        {v1, v2, label: label, weight: :rand.uniform(100)}
      end

    plain =
      Enum.reduce(edges, Graph.new(), fn {v1, v2, opts}, g ->
        Graph.add_edge(g, v1, v2, opts)
      end)

    multi =
      Enum.reduce(edges, Graph.new(multigraph: true), fn {v1, v2, opts}, g ->
        Graph.add_edge(g, v1, v2, opts)
      end)

    target_label = Enum.random(labels)
    some_vertex = :rand.uniform(num_vertices)

    {plain, multi, target_label, some_vertex}
  end
end

alias MultigraphBench.Helpers

Benchee.run(
  %{
    "scan all edges + filter (no multigraph)" => fn {g_plain, _g_multi, target_label, _v} ->
      g_plain |> Graph.edges() |> Enum.filter(fn e -> e.label == target_label end)
    end,
    "indexed lookup (multigraph by:)" => fn {_g_plain, g_multi, target_label, _v} ->
      Graph.edges(g_multi, by: target_label)
    end,
    "scan out_edges + filter (no multigraph)" => fn {g_plain, _g_multi, target_label, v} ->
      g_plain |> Graph.out_edges(v) |> Enum.filter(fn e -> e.label == target_label end)
    end,
    "indexed out_edges (multigraph by:)" => fn {_g_plain, g_multi, target_label, v} ->
      Graph.out_edges(g_multi, v, by: target_label)
    end
  },
  inputs: %{
    "1k vertices, 5k edges, 10 labels" => Helpers.build_graphs(1_000, 5_000, 10),
    "10k vertices, 50k edges, 50 labels" => Helpers.build_graphs(10_000, 50_000, 50)
  },
  time: 10,
  memory_time: 5
)
