defmodule MultigraphMemoryBench.Helpers do
  def build_graphs(num_edges, num_labels) do
    num_vertices = div(num_edges, 5)
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

    {plain, multi}
  end
end

alias MultigraphMemoryBench.Helpers

IO.puts("Multigraph Memory Overhead Report")
IO.puts("==================================\n")

for {name, {size, labels}} <- [
      {"1k edges / 5 labels", {1_000, 5}},
      {"10k edges / 10 labels", {10_000, 10}},
      {"10k edges / 100 labels", {10_000, 100}},
      {"100k edges / 50 labels", {100_000, 50}}
    ] do
  {plain, multi} = Helpers.build_graphs(size, labels)

  plain_info = Graph.info(plain)
  multi_info = Graph.info(multi)

  ratio = multi_info.size_in_bytes / plain_info.size_in_bytes

  IO.puts(
    "#{name}: plain=#{plain_info.size_in_bytes}B, multi=#{multi_info.size_in_bytes}B, ratio=#{Float.round(ratio, 2)}x"
  )
end
