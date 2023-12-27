defmodule Graph.UndirectedTest do
  use ExUnit.Case, async: true

  describe "Graph.reachable/2" do
    test "reachable" do
      g =
        Graph.new(type: :undirected)
        |> Graph.add_edges([{:a, :b}, {:b, :c}])

      assert [:a, :b, :c] = Graph.reachable(g, [:c])
      assert [:c, :b, :a] = Graph.reachable(g, [:a])
    end

    test "parts reachable" do
      g =
        Graph.new(type: :undirected)
        |> Graph.add_edges([{:a, :b}, {:b, :c}, {:d, :e}])

      assert [:d, :e] = Graph.reachable(g, [:e])
      assert [:c, :a, :b] = Graph.reachable(g, [:b])
    end

    test "nothing reachable" do
      g =
        Graph.new(type: :undirected)
        |> Graph.add_edges([{:a, :b}, {:b, :d}])
        |> Graph.add_vertex(:c)

      assert [:c] = Graph.reachable(g, [:c])
    end

    test "unknown vertex" do
      g = Graph.new(type: :undirected)

      assert [nil] = Graph.reachable(g, [:a])
    end
  end
end
