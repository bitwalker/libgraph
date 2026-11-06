defmodule Graph.SplitEdgeTest do
  use ExUnit.Case, async: true

  for type <- [:directed, :undirected] do
    test "splitting removes old metadata as well as adjacency (#{type})" do
      g =
        Graph.new(type: unquote(type))
        |> Graph.add_edge(:a, :b, label: :x, weight: 2)
        |> Graph.add_edge(:a, :b, label: :y, weight: 3)
        |> Graph.split_edge(:a, :b, :middle)

      assert Graph.edge(g, :a, :b, :x) == nil
      assert Graph.edges(g, :a, :b) == []
      assert Graph.num_edges(g) == 4
      assert Graph.edge(g, :a, :middle, :x).weight == 2
      assert Graph.edge(g, :middle, :b, :y).weight == 3

      result = Graph.delete_vertex(g, :middle)
      assert result.edges == %{}
      assert Graph.edges(result) == []
    end
  end
end
