defmodule Graph.Reducer.Test do
  use ExUnit.Case, async: true
  doctest Graph.Reducers.Bfs
  doctest Graph.Reducers.Dfs

  test "can walk a graph depth-first" do
    g = Graph.new
    |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g])
    |> Graph.add_edge(:a, :b)
    |> Graph.add_edge(:b, :c)
    |> Graph.add_edge(:b, :d)
    |> Graph.add_edge(:c, :e)
    |> Graph.add_edge(:d, :f)
    |> Graph.add_edge(:f, :g)

    expected = [:a, :b, :c, :e, :d, :f, :g]
    assert ^expected = Graph.Reducers.Dfs.map(g, fn v -> v end)
  end

  test "first node of depth-first (preorder) with edge {1, 8000}" do
    g = Graph.new
    |> Graph.add_vertices([1, 8000])
    |> Graph.add_edge(1, 8000)

    assert 1 = Graph.Reducers.Dfs.map(g, fn v -> v end) |> List.first()
    # NB the same applies to the preorder function
    # assert 1 = Graph.preorder(g) |> List.first()
  end

  test "can walk a graph breadth-first" do
    g = Graph.new
    |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g])
    |> Graph.add_edge(:a, :b)
    |> Graph.add_edge(:a, :d)
    |> Graph.add_edge(:b, :c)
    |> Graph.add_edge(:b, :d)
    |> Graph.add_edge(:c, :e)
    |> Graph.add_edge(:d, :f)
    |> Graph.add_edge(:f, :g)

    expected = [:a, :b, :d, :c, :f, :e, :g]
    assert ^expected = Graph.Reducers.Bfs.map(g, fn v -> v end)
  end
end
