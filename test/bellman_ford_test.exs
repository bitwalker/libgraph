defmodule Graph.BellmanFordTest do
  use ExUnit.Case, async: true

  test "Bellman-Ford respects custom identifiers and strict source identity" do
    g =
      Graph.new(vertex_identifier: fn v -> v end)
      |> Graph.add_edge(1, :a, weight: 3)
      |> Graph.add_vertex(1.0)

    assert Graph.bellman_ford(g, 1) == %{1 => 0, :a => 3, 1.0 => :infinity}
  end
end
