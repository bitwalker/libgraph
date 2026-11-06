defmodule MultigraphCreationBench.Helpers do
  def build_edges(size) do
    labels = Enum.map(1..10, fn i -> :"label_#{i}" end)

    for i <- 1..size do
      v1 = :rand.uniform(div(size, 5))
      v2 = :rand.uniform(div(size, 5))
      label = Enum.random(labels)
      {v1, v2, label: label, weight: :rand.uniform(100)}
    end
  end
end

alias MultigraphCreationBench.Helpers

Benchee.run(
  %{
    "plain graph (no index)" => fn edges ->
      Enum.reduce(edges, Graph.new(), fn {v1, v2, opts}, g ->
        Graph.add_edge(g, v1, v2, opts)
      end)
    end,
    "multigraph (indexed)" => fn edges ->
      Enum.reduce(edges, Graph.new(multigraph: true), fn {v1, v2, opts}, g ->
        Graph.add_edge(g, v1, v2, opts)
      end)
    end
  },
  inputs: %{
    "10k edges" => Helpers.build_edges(10_000),
    "100k edges" => Helpers.build_edges(100_000)
  },
  time: 10,
  memory_time: 5
)
