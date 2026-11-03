defmodule Graph.BulkLabelsTest do
  use ExUnit.Case, async: true

  test "explicit bulk labeling handles arbitrary vertices and duplicates" do
    records = [
      {{:node, 1}, [:first]},
      {[:node, 2], :second},
      {%{id: 3}, [:third]},
      {{:node, 1}, :ignored}
    ]

    for type <- [:directed, :undirected] do
      g =
        Enum.reduce(records, Graph.new(type: type), fn {vertex, labels}, graph ->
          Graph.add_vertex(graph, vertex, labels)
        end)

      assert Graph.num_vertices(g) == 3
      assert Graph.vertex_labels(g, {:node, 1}) == [:first]
      assert Graph.vertex_labels(g, [:node, 2]) == [:second]
      assert Graph.vertex_labels(g, %{id: 3}) == [:third]
    end
  end

  test "add_vertices does not reinterpret tuple or list vertices as labels" do
    vertices = [{:a, [:label]}, [:b, :label]]
    g = Graph.new() |> Graph.add_vertices(vertices)
    assert MapSet.new(Graph.vertices(g)) == MapSet.new(vertices)
    assert Enum.all?(vertices, &(Graph.vertex_labels(g, &1) == []))
  end

  test "empty reduction and logical duplicate identifiers preserve insertion semantics" do
    original = Graph.new(vertex_identifier: &Map.fetch!(&1, :id))
    insert = fn {vertex, labels}, graph -> Graph.add_vertex(graph, vertex, labels) end
    assert Enum.reduce([], original, insert) === original

    first = %{id: 1, name: :first}
    second = %{id: 1, name: :second}
    g = Enum.reduce([{first, [:first]}, {second, [:ignored]}], original, insert)
    assert Graph.vertices(g) == [first]
    assert Graph.vertex_labels(g, second) == [:first]
    assert g.vertex_identifier === original.vertex_identifier
  end
end
