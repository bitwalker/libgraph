defmodule GraphTest do
  use ExUnit.Case
  doctest Graph

  test "find all paths" do
    g = Graph.new
    |> Graph.add_vertex(:a)
    |> Graph.add_vertex(:b)
    |> Graph.add_vertex(:c)
    |> Graph.add_vertex(:d)
    |> Graph.add_vertex(:e)
    |> Graph.add_edge(:a, :b)
    |> Graph.add_edge(:a, :c)
    |> Graph.add_edge(:b, :c)
    |> Graph.add_edge(:b, :d)
    |> Graph.add_edge(:c, :d)
    |> Graph.add_edge(:c, :a)
    |> Graph.add_edge(:d, :e)

    assert [[:a, :c, :d, :e], [:a, :b, :d, :e], [:a, :b, :c, :d, :e]] = Graph.get_paths(g, :a, :e)
  end

  test "find shortest path" do
    g = Graph.new
    |> Graph.add_vertex(:a)
    |> Graph.add_vertex(:b)
    |> Graph.add_vertex(:c)
    |> Graph.add_vertex(:d)
    |> Graph.add_vertex(:e)
    |> Graph.add_edge(:a, :b)
    |> Graph.add_edge(:a, :c)
    |> Graph.add_edge(:b, :c)
    |> Graph.add_edge(:b, :d)
    |> Graph.add_edge(:c, :d)
    |> Graph.add_edge(:c, :a)
    |> Graph.add_edge(:d, :e)

    assert [:a, :b, :d, :e] = Graph.get_shortest_path(g, :a, :e)
  end
end
