defmodule GraphTest do
  use ExUnit.Case, async: true
  alias Graph.Edge
  alias Graph.Test.Generators

  describe "Graph.reachable/2" do
    test "reachable" do
      g =
        Graph.new(type: :undirected)
        |> Graph.add_edges([{:a, :b}, {:b, :c}])

      assert [:a, :b, :c] = Graph.reachable(g, [:c])
    end

    test "parts reachable" do
      g =
        Graph.new(type: :undirected)
        |> Graph.add_edges([{:a, :b}, {:b, :c}, {:d, :e}])

      assert [:d, :e] = Graph.reachable(g, [:e])
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

  describe "Graph.reachable_neighbours/2" do
    test "reachable" do
      g =
        Graph.new(type: :undirected)
        |> Graph.add_edges([{:a, :b}, {:b, :c}])

      assert [:a, :b] = Graph.reachable_neighbors(g, [:c])
    end

    test "parts reachable" do
      g =
        Graph.new(type: :undirected)
        |> Graph.add_edges([{:a, :b}, {:b, :c}, {:d, :e}, {:e, :f}])

      assert [:d, :e] = Graph.reachable_neighbors(g, [:f])
    end

    test "nothing reachable" do
      g =
        Graph.new(type: :undirected)
        |> Graph.add_edges([{:a, :b}, {:b, :d}])
        |> Graph.add_vertex(:c)

      assert [] = Graph.reachable_neighbors(g, [:c])
    end

    test "unknown vertex" do
      g = Graph.new(type: :undirected)

      assert [] = Graph.reachable_neighbors(g, [:a])
    end
  end
end
