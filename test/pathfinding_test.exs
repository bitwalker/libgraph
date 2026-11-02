defmodule Graph.PathfindingTest do
  use ExUnit.Case, async: true

  for type <- [:directed, :undirected] do
    test "shortest paths use vertex callbacks only on vertices (#{type})" do
      a = {1, :a}
      b = {2, :b}
      c = {3, :c}
      missing = {4, :missing}

      g =
        Graph.new(type: unquote(type), vertex_identifier: fn {id, _} -> id end)
        |> Graph.add_edges([{a, b}, {b, c}])
        |> Graph.add_vertex(missing)

      assert Graph.a_star(g, a, c, fn {_, _} -> 0 end) == [a, b, c]
      assert Graph.dijkstra(g, a, b) == [a, b]
      assert Graph.get_shortest_path(g, a, c) == [a, b, c]
      assert Graph.dijkstra(g, a, missing) == nil
    end
  end
end
