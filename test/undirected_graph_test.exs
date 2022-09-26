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
        |> Graph.add_edges([{:a, :b}, {:b, :d}, {:e, :c}])

      assert [:e, :c] = Graph.reachable(g, [:c])
    end

    test "nothing reachable" do
      g =
        Graph.new(type: :undirected)
        |> Graph.add_edges([{:a, :b}, {:b, :d}])
        |> Graph.add_vertex(:c)

      assert [:c] = Graph.reachable(g, [:c])
    end
  end
end
