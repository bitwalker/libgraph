defmodule Graph.MultigraphPathfindingTest do
  use ExUnit.Case, async: true

  for type <- [:directed, :undirected] do
    test "#{type}: filtered shortest paths retain custom vertex identifiers" do
      [a, b, c] = Enum.map(1..3, &%{id: {:vertex, &1}})

      g =
        Graph.new(type: unquote(type), multigraph: true, vertex_identifier: & &1.id)
        |> Graph.add_edge(a, b, label: :flow, weight: 2)
        |> Graph.add_edge(b, c, label: :flow, weight: 3)
        |> Graph.add_edge(a, c, label: :history, weight: 1)

      assert Graph.dijkstra(g, a, c, by: :flow) == [a, b, c]
      assert Graph.a_star(g, a, c, fn _ -> 0 end, by: :flow) == [a, b, c]
      assert Graph.dijkstra(g, a, c) == [a, c]
    end
  end

  test "filtered Bellman-Ford uses custom identifiers and the cheapest selected parallel edge" do
    [a, b, c] = Enum.map(1..3, &%{id: {:vertex, &1}})

    g =
      Graph.new(
        multigraph: true,
        vertex_identifier: & &1.id,
        partition_by: fn edge -> [edge.properties.kind] end
      )
      |> Graph.add_edge(a, b, label: :expensive, weight: 8, properties: %{kind: :flow})
      |> Graph.add_edge(a, b, label: :cheap, weight: 2, properties: %{kind: :flow})
      |> Graph.add_edge(b, c, weight: 3, properties: %{kind: :flow})
      |> Graph.add_edge(a, c, weight: 1, properties: %{kind: :history})

    assert Graph.bellman_ford(g, a, by: :flow) == %{a => 0, b => 2, c => 5}
    assert Graph.bellman_ford(g, a) == %{a => 0, b => 2, c => 1}
  end

  test "undirected reverse traversal resolves properties from the stored edge orientation" do
    g =
      Graph.new(
        type: :undirected,
        multigraph: true,
        partition_by: fn edge -> [Map.get(edge.properties, :kind)] end
      )
      |> Graph.add_edge(:a, :b, properties: %{kind: :flow})
      |> Graph.add_edge(:b, :c, properties: %{kind: :flow})

    assert Graph.dijkstra(g, :c, :a, by: :flow) == [:c, :b, :a]
  end
end
