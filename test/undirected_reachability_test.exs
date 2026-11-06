defmodule Graph.UndirectedReachabilityTest do
  use ExUnit.Case, async: true

  test "reachability follows both directions of canonical undirected edges" do
    g = Graph.new(type: :undirected) |> Graph.add_edges([{:a, :b}, {:b, :c}])

    assert Enum.sort(Graph.reachable(g, [:c])) == [:a, :b, :c]
    assert Enum.sort(Graph.reaching(g, [:a])) == [:a, :b, :c]
    assert Enum.sort(Graph.reachable_neighbors(g, [:c])) == [:a, :b, :c]
    assert Enum.sort(Graph.reaching_neighbors(g, [:a])) == [:a, :b, :c]
    assert Graph.strong_components(g) |> Enum.map(&Enum.sort/1) == [[:a, :b, :c]]
  end

  test "directed reachability keeps directionality" do
    g = Graph.new() |> Graph.add_edges([{:a, :b}, {:b, :c}])
    assert Graph.reachable(g, [:c]) == [:c]
    assert Graph.reaching(g, [:a]) == [:a]
    assert Graph.reachable_neighbors(g, [:c]) == []
  end
end
