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

  defp graph, do: Graph.new(type: :undirected, vertex_identifier: fn v -> v end)

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
