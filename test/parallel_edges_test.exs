defmodule Graph.ParallelEdgesTest do
  use ExUnit.Case, async: true

  for type <- [:directed, :undirected] do
    test "labels distinguish edges while neighbor relations remain unique (#{type})" do
      g =
        Graph.new(type: unquote(type))
        |> Graph.add_edge(:a, :b, label: :x, weight: 2)
        |> Graph.add_edge(:a, :b, label: :y, weight: 3)
        |> Graph.add_edge(:a, :b, label: :x, weight: 4)

      assert Graph.num_edges(g) == 2
      assert Graph.neighbors(g, :a) == [:b]
      assert Graph.out_degree(g, :a) == 2
      assert Graph.edge(g, :a, :b, :x).weight == 4
      assert Graph.edge(g, :a, :b, :y).weight == 3
      assert length(Graph.edges(g, :a, :b)) == 2
      assert Graph.delete_edge(g, :a, :b, :y) |> Graph.num_edges() == 1
    end

    test "DOT permits the parallel edges it emits (#{type})" do
      g =
        Graph.new(type: unquote(type))
        |> Graph.add_edge(:a, :b, label: :x)
        |> Graph.add_edge(:a, :b, label: :y)

      assert {:ok, dot} = Graph.to_dot(g)
      refute String.starts_with?(dot, "strict ")
      assert String.contains?(dot, "label=\"x\"")
      assert String.contains?(dot, "label=\"y\"")
    end
  end

  test "opposite directed edges are distinct and ordinary DOT stays strict" do
    g = Graph.new() |> Graph.add_edges([{:a, :b}, {:b, :a}])
    assert Graph.num_edges(g) == 2
    assert Graph.edge(g, :a, :b) !== Graph.edge(g, :b, :a)
    assert {:ok, dot} = Graph.to_dot(g)
    assert String.starts_with?(dot, "strict digraph ")
  end

  test "undirected reversal updates one labeled edge" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_edge(:a, :b, label: :x)
      |> Graph.add_edge(:b, :a, label: :x, weight: 5)
      |> Graph.add_edge(:b, :a, label: :y)

    assert Graph.num_edges(g) == 2
    assert Graph.edge(g, :a, :b, :x).weight == 5
    assert Graph.edge(g, :b, :a, :x) === Graph.edge(g, :a, :b, :x)
  end
end
