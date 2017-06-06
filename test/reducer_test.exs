defmodule Graph.Reducer.Test do
  use ExUnit.Case, async: true

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
    assert ^expected = Graph.map(g, fn v -> v end)
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
    assert ^expected = Graph.map(g, fn v -> v end, algorithm: :breadth_first)
  end

  test "can reduce a function over a graph" do
    g = Graph.new
    |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g])
    |> Graph.add_edge(:a, :b)
    |> Graph.add_edge(:b, :c)
    |> Graph.add_edge(:b, :d)
    |> Graph.add_edge(:c, :e)
    |> Graph.add_edge(:d, :f)
    |> Graph.add_edge(:f, :g)

    expected = "abcedfg"
    assert ^expected = Graph.reduce(g, "", fn v, acc -> "#{acc}#{v}" end)
  end
end
