defmodule Graph.IncidentEdgesTest do
  use ExUnit.Case, async: true

  for type <- [:directed, :undirected] do
    test "each labeled self-loop is enumerated once (#{type})" do
      g =
        Graph.new(type: unquote(type))
        |> Graph.add_edges([{:a, :a, label: :x}, {:a, :a, label: :y}, {:a, :b}])

      edges = Graph.edges(g, :a)
      assert length(edges) == 3
      assert Enum.sort(edges) == Enum.sort(Graph.edges(g))
      assert Graph.edges(g, :missing) == []
    end
  end

  test "reciprocal directed edges and different labels stay distinct" do
    g =
      Graph.new()
      |> Graph.add_edges([{:a, :b, label: :x}, {:a, :b, label: :y}, {:b, :a}])

    assert length(Graph.edges(g, :a)) == 3
    assert Enum.sort(Graph.edges(g, :a)) == Enum.sort(Graph.edges(g))
  end
end
