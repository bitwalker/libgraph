defmodule Graph.VertexMutationTest do
  use ExUnit.Case, async: true

  test "coalescing vertices preserves indexes for both incident neighborhoods" do
    g =
      Graph.new()
      |> Graph.add_edges([{:a, :c}, {:b, :d}])
      |> Graph.replace_vertex(:a, :b)

    assert Enum.sort(Graph.out_neighbors(g, :b)) == [:c, :d]
    result = g |> Graph.delete_vertex(:b) |> Graph.delete_vertex(:c)
    assert Graph.edges(result) == []
    assert Graph.vertices(result) == [:d]
  end

  test "replacing an undirected vertex reorients its incident edges" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_edges([{:a, :b, label: :x}, {:a, :a, label: :loop}, {:b, :c}])
      |> Graph.label_vertex(:a, :label)
      |> Graph.replace_vertex(:a, :z)

    assert Graph.edge(g, :b, :z, :x) == Graph.Edge.new(:b, :z, label: :x)
    assert Graph.edge(g, :z, :z, :loop) == Graph.Edge.new(:z, :z, label: :loop)
    assert Graph.vertex_labels(g, :z) == [:label]
    refute Graph.has_vertex?(g, :a)
    assert Graph.replace_vertex(g, :missing, :new) == {:error, :no_such_vertex}
  end

  test "deleting an integer ID preserves distinct float ID edges" do
    g =
      Graph.new(vertex_identifier: fn v -> v end)
      |> Graph.add_edges([{1, :a}, {1.0, :b}])
      |> Graph.delete_vertex(1)

    assert Graph.edges(g) == [Graph.Edge.new(1.0, :b)]
    assert Graph.out_neighbors(g, 1.0) == [:b]
  end

  for type <- [:directed, :undirected] do
    test "delete removes only the incident neighborhood (#{type})" do
      g =
        Graph.new(type: unquote(type))
        |> Graph.add_edges([
          {:a, :b},
          {:b, :a},
          {:b, :b, label: :loop},
          {:b, :c, label: :x},
          {:b, :c, label: :y},
          {:c, :d}
        ])
        |> Graph.label_vertex(:b, :label)

      result = Graph.delete_vertex(g, :b)
      assert Enum.sort(Graph.vertices(result)) == [:a, :c, :d]
      assert Graph.edges(result) == [Graph.Edge.new(:c, :d)]
      assert Graph.vertex_labels(result, :b) == []
      assert Graph.neighbors(result, :a) == []
      assert Graph.delete_vertex(result, :missing) == result

      assert Graph.new(type: unquote(type)) |> Graph.add_vertex(:a) |> Graph.delete_vertex(:a) ==
               Graph.new(type: unquote(type))
    end
  end
end
