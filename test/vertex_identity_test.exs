defmodule Graph.VertexIdentityTest do
  use ExUnit.Case, async: true

  test "reversing numerically equal undirected endpoints preserves one edge" do
    for {left, right} <- numeric_pairs() do
      g = graph() |> Graph.add_edge(left, right, weight: 3)
      reversed = Graph.add_edge(g, right, left, weight: 3)

      assert Graph.num_vertices(reversed) == 2
      assert Graph.num_edges(reversed) == 1
      assert reversed.edges === g.edges
      assert Graph.edge(g, right, left) === Graph.edge(g, left, right)
      assert Graph.edges(g, right, left) === Graph.edges(g, left, right)
      assert Graph.edge(g, left, right) !== nil
      assert Graph.edges(g) === Graph.edges(g, left, right)
      assert Graph.dijkstra(g, left, right) === [left, right]
      assert Graph.dijkstra(g, right, left) === [right, left]
      assert Graph.delete_vertex(g, left) |> Graph.vertices() === [right]
    end
  end

  test "updating an undirected edge accepts either exact endpoint order" do
    for {left, right} <- numeric_pairs() do
      g = graph() |> Graph.add_edge(left, right, label: :old)
      result = Graph.update_labelled_edge(g, right, left, :old, label: :new, weight: 4)

      assert %Graph{} = result
      assert Graph.edge(result, left, right, :new).weight == 4
      assert Graph.edge(result, right, left, :new) === Graph.edge(result, left, right, :new)
      assert Graph.num_edges(result) == 1
      assert Graph.edge(result, left, right, :old) == nil
    end
  end

  test "deleting undirected edges accepts either exact endpoint order" do
    for {left, right} <- numeric_pairs() do
      g = graph() |> Graph.add_edge(left, right) |> Graph.add_edge(left, right, label: :keep)

      labelled = Graph.delete_edge(g, right, left, nil)
      assert Graph.num_edges(labelled) == 1
      assert Graph.edge(labelled, left, right, :keep) !== nil
      assert Graph.edge(labelled, left, right) == nil

      for result <- [Graph.delete_edge(g, right, left), Graph.delete_edges(g, right, left)] do
        assert Graph.edges(result) == []
        assert Graph.neighbors(result, left) == []
        assert Graph.neighbors(result, right) == []
      end
    end
  end

  test "splitting an undirected edge accepts reversed exact endpoints" do
    for {left, right} <- numeric_pairs() do
      g = graph() |> Graph.add_edge(left, right, label: :split, weight: 3)
      result = Graph.split_edge(g, right, left, :middle)

      assert %Graph{} = result
      assert Graph.num_edges(result) == 2
      assert Graph.edge(result, left, right, :split) == nil
      assert Graph.edge(result, :middle, left, :split).weight == 3
      assert Graph.edge(result, right, :middle, :split).weight == 3
    end
  end

  test "replacing a vertex with a fresh numeric peer reorients its edges" do
    for {left, right} <- numeric_pairs() do
      g =
        graph()
        |> Graph.add_edge(:old, left, label: :incident)
        |> Graph.add_edge(:old, :old, label: :loop)
        |> Graph.label_vertex(:old, :label)
        |> Graph.replace_vertex(:old, right)

      assert Graph.edge(g, right, left, :incident) !== nil
      assert Graph.edge(g, left, right, :incident) === Graph.edge(g, right, left, :incident)
      assert Graph.edge(g, right, right, :loop) !== nil
      assert Graph.vertex_labels(g, right) == [:label]
      assert Graph.num_vertices(g) == 2
      assert Graph.num_edges(g) == 2
    end
  end

  test "core decomposition peels distinct numeric neighbors independently" do
    for {left, right} <- numeric_pairs() do
      g =
        graph()
        |> Graph.add_edges([{left, right}, {right, :a}, {:a, :b}, {:b, :c}, {:c, :a}])

      assert Graph.coreness(g, left) == 1
      assert Graph.coreness(g, right) == 1
      assert Graph.coreness(g, :a) == 2
      assert Graph.k_core_components(g)[1] |> MapSet.new() == MapSet.new([left, right])
    end
  end

  test "legacy numeric endpoint orientation remains queryable and mutable" do
    for {left, right} <- numeric_ties() do
      # Older undirected graphs kept caller order when ordinary comparison tied.
      # Directed insertion recreates that layout without the new normalization.
      edge = graph() |> Graph.add_edge(left, right) |> Graph.edge(left, right)

      legacy =
        Graph.new(vertex_identifier: fn v -> v end)
        |> Graph.add_edge(edge.v2, edge.v1, label: :legacy, weight: 3)
        |> Map.put(:type, :undirected)

      assert Graph.edge(legacy, left, right, :legacy) !== nil
      assert Graph.edge(legacy, right, left, :legacy) === Graph.edge(legacy, left, right, :legacy)
      assert Graph.edges(legacy) === Graph.edges(legacy, left, right)
      assert Graph.edges(legacy) === Graph.edges(legacy, right, left)
      assert Graph.add_edge(legacy, left, right, label: :legacy).edges |> map_size() == 1
      assert Graph.add_edge(legacy, right, left, label: :legacy).edges |> map_size() == 1

      for {from, to} <- [{left, right}, {right, left}] do
        updated = Graph.update_labelled_edge(legacy, from, to, :legacy, weight: 4)
        assert Graph.edge(updated, from, to, :legacy).weight == 4

        for result <- [
              Graph.delete_edge(legacy, from, to),
              Graph.delete_edge(legacy, from, to, :legacy),
              Graph.delete_edges(legacy, from, to)
            ] do
          assert result.edges == %{}
          assert Graph.edges(result) == []
        end

        split = Graph.split_edge(legacy, from, to, :middle)
        assert Graph.num_edges(split) == 2
        assert Graph.edge(split, from, to, :legacy) == nil
        assert Graph.dijkstra(split, from, to) === [from, :middle, to]
      end
    end
  end

  test "existing opposite numeric keys select the same orientation for either query" do
    for {left, right} <- numeric_ties() do
      edge = graph() |> Graph.add_edge(left, right, weight: 3) |> Graph.edge(left, right)

      legacy =
        Graph.new(vertex_identifier: fn v -> v end)
        |> Graph.add_edge(edge.v1, edge.v2, weight: 3)
        |> Graph.add_edge(edge.v2, edge.v1, weight: 9)
        |> Map.put(:type, :undirected)

      assert Graph.edge(legacy, left, right) === edge
      assert Graph.edge(legacy, right, left) === edge
    end
  end

  defp graph, do: Graph.new(type: :undirected, vertex_identifier: fn v -> v end)

  defp numeric_ties do
    Enum.filter(numeric_pairs(), fn {left, right} -> not (left < right) and not (left > right) end)
  end

  defp numeric_pairs do
    [
      {1, 1.0},
      {{:id, 1}, {:id, 1.0}},
      {[1], [1.0]},
      {%{id: 1}, %{id: 1.0}},
      {%{1 => :id}, %{1.0 => :id}},
      {0.0, -0.0}
    ]
    |> Enum.reject(fn {left, right} -> left === right end)
  end
end
