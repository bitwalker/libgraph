defmodule Graph.BellmanFordTest do
  use ExUnit.Case, async: true

  test "parallel edge labels do not determine shortest distance" do
    g =
      Graph.new()
      |> Graph.add_edge(:a, :b, label: :a, weight: 9)
      |> Graph.add_edge(:a, :b, label: :z, weight: 1.5)

    assert Graph.bellman_ford(g, :a) == %{a: 0, b: 1.5}
  end

  test "a cheaper parallel edge can form a reachable negative cycle" do
    g =
      Graph.new()
      |> Graph.add_edge(:a, :b, label: :a, weight: 9)
      |> Graph.add_edge(:a, :b, label: :z, weight: -2)
      |> Graph.add_edge(:b, :a, weight: 1)

    assert Graph.bellman_ford(g, :a) == nil
  end

  test "Bellman-Ford respects custom identifiers and strict source identity" do
    g =
      Graph.new(vertex_identifier: fn v -> v end)
      |> Graph.add_edge(1, :a, weight: 3)
      |> Graph.add_vertex(1.0)

    assert Graph.bellman_ford(g, 1) == %{1 => 0, :a => 3, 1.0 => :infinity}
  end
end
