defmodule Graph.SubgraphIdentityTest do
  use ExUnit.Case, async: true

  for type <- [:directed, :undirected] do
    test "subgraphs preserve custom identifiers, labels and edges (#{type})" do
      a = {1, :a}
      b = {1.0, :b}
      c = {nil, :c}
      excluded = {false, :excluded}
      identifier = fn {id, _} -> id end

      g =
        Graph.new(type: unquote(type), vertex_identifier: identifier)
        |> Graph.add_vertex(a, :label)
        |> Graph.add_edges([{a, b, label: :x, weight: 2}, {b, c}, {c, excluded}])

      result = Graph.subgraph(g, [a, b, c, {2, :missing}])

      assert result.vertex_identifier === identifier
      assert MapSet.new(Map.keys(result.vertices)) === MapSet.new([1, 1.0, nil])
      assert MapSet.new(Graph.vertices(result)) === MapSet.new([a, b, c])
      assert Graph.vertex_labels(result, a) == [:label]
      assert Graph.edge(result, a, b, :x).weight == 2
      assert Graph.num_edges(result) == 2
      assert Graph.dijkstra(result, a, c) === [a, b, c]
      assert Graph.subgraph(g, []).vertex_identifier === identifier
      assert Graph.subgraph(g, [a]).vertices === %{1 => a}
    end

    test "subgraphs do not reintroduce default hash collisions (#{type})" do
      # These distinct integers collide under the default 32-bit phash2.
      left = 33_702
      right = 44_741
      assert Graph.Utils.vertex_id(left) == Graph.Utils.vertex_id(right)

      g =
        Graph.new(type: unquote(type), vertex_identifier: fn v -> v end)
        |> Graph.add_edge(left, right, label: :collision, weight: 2)

      result = Graph.subgraph(g, [left, right])

      assert Graph.num_vertices(result) == 2
      assert result.vertices === g.vertices
      assert Graph.edge(result, left, right, :collision).weight == 2
      assert Graph.dijkstra(result, left, right) == [left, right]
    end
  end

  test "core extraction retains collision-free identifiers" do
    identifier = fn v -> v end

    g =
      Graph.new(type: :undirected, vertex_identifier: identifier)
      |> Graph.add_edges([{33_702, 44_741}, {44_741, :a}, {:a, 33_702}])

    for core <- [Graph.k_core(g, 2), Graph.degeneracy_core(g)] do
      assert core.vertex_identifier === identifier
      assert core.vertices === g.vertices
      assert core.edges === g.edges
      assert Graph.num_vertices(core) == 3
    end
  end

  test "subgraphs preserve caller-defined logical identity" do
    identifier = fn {id, _} -> id end
    g = Graph.new(vertex_identifier: identifier) |> Graph.add_edge({1, :first}, {2, :second})
    result = Graph.subgraph(g, [{1, :alias}, {2, :alias}])

    assert result.vertex_identifier === identifier
    assert Graph.vertices(result) |> MapSet.new() == MapSet.new([{1, :first}, {2, :second}])
    assert Graph.add_vertex(result, {1, :replacement}) === result
    assert Graph.dijkstra(result, {1, :alias}, {2, :alias}) == [{1, :first}, {2, :second}]
  end
end
