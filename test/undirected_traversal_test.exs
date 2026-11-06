defmodule Graph.UndirectedTraversalTest do
  use ExUnit.Case, async: true

  test "depth-first traversals finish an undirected component before starting another" do
    ids = %{a: 3, b: 1, c: 4, d: 2}

    g =
      Graph.new(type: :undirected, vertex_identifier: &Map.fetch!(ids, &1))
      |> Graph.add_edges([{:a, :b}, {:c, :d}])

    for order <- [Graph.preorder(g), Graph.postorder(g)] do
      assert MapSet.new(order) == MapSet.new([:a, :b, :c, :d])
      components = Enum.chunk_by(order, fn v -> v in [:a, :b] end)
      assert length(components) == 2
      assert Enum.all?(components, &(length(&1) == 2))
    end
  end

  test "an undirected path has opposite entry and exit orders from the same root" do
    ids = %{a: 3, b: 2, c: 1}

    g =
      Graph.new(type: :undirected, vertex_identifier: &Map.fetch!(ids, &1))
      |> Graph.add_edges([{:a, :b}, {:b, :c}])

    assert Graph.postorder(g) === Enum.reverse(Graph.preorder(g))
  end

  test "undirected traversal visits cycles, self-loops and isolated vertices once" do
    g =
      Graph.new(type: :undirected, vertex_identifier: fn v -> v end)
      |> Graph.add_edges([{1, 1.0}, {1.0, :a}, {:a, 1}, {:a, :a}, {1, 1.0, label: :parallel}])
      |> Graph.add_vertex(nil)

    for order <- [Graph.preorder(g), Graph.postorder(g)] do
      assert length(order) == 4
      assert MapSet.new(order) === MapSet.new([1, 1.0, :a, nil])
    end

    assert Graph.preorder(Graph.new(type: :undirected)) == []
    assert Graph.postorder(Graph.new(type: :undirected)) == []
  end

  test "directed traversal retains outgoing-edge semantics" do
    g =
      Graph.new(vertex_identifier: fn v -> v end)
      |> Graph.add_edges([{3, 2}, {2, 1}])

    assert Graph.preorder(g) == [1, 2, 3]
    assert Graph.postorder(g) == [1, 2, 3]
    assert Graph.topsort(g) == [3, 2, 1]
    assert Enum.all?(Graph.strong_components(g), &(length(&1) == 1))
  end
end
