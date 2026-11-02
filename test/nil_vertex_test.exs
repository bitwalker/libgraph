defmodule Graph.NilVertexTest do
  use ExUnit.Case, async: true

  test "reinserting nil preserves its labels" do
    g = Graph.new() |> Graph.add_vertex(nil, :keep)
    assert Graph.add_vertex(g, nil) == g
    assert Graph.add_edge(g, :a, nil) |> Graph.vertex_labels(nil) == [:keep]
  end

  test "replacement preserves incident and replacement nil labels" do
    g = Graph.new() |> Graph.add_edge(:a, nil) |> Graph.label_vertex(nil, :keep)
    assert Graph.replace_vertex(g, :a, :z) |> Graph.vertex_labels(nil) == [:keep]

    g = Graph.new() |> Graph.add_edge(:a, :b) |> Graph.label_vertex(:a, :keep)
    assert Graph.replace_vertex(g, :a, nil) |> Graph.vertex_labels(nil) == [:keep]
  end
end
