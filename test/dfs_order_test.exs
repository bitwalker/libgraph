defmodule Graph.DfsOrderTest do
  use ExUnit.Case, async: true

  test "a directed forest may have equal entry and exit lists" do
    g = Graph.new(vertex_identifier: fn v -> v end) |> Graph.add_edge(2, 1)
    assert Graph.preorder(g) === Graph.postorder(g)
    assert MapSet.new(Graph.preorder(g)) == MapSet.new([1, 2])
    assert Graph.topsort(g) == [2, 1]
  end

  test "tree parents precede children on entry and follow them on exit" do
    vertices = Enum.map(1..7, &{&1, :vertex})
    edges = for child <- 2..7, do: {{div(child, 2), :vertex}, {child, :vertex}}

    for type <- [:directed, :undirected] do
      g =
        Graph.new(type: type, vertex_identifier: fn {id, _} -> id end)
        |> Graph.add_vertices(vertices)
        |> Graph.add_edges(edges)

      # Small integer IDs put the root first in this controlled fixture; no
      # public insertion or lexical ordering guarantee is inferred from it.
      assert hd(Map.keys(g.vertices)) == 1
      entry = g |> Graph.preorder() |> Enum.with_index() |> Map.new()
      exit = g |> Graph.postorder() |> Enum.with_index() |> Map.new()

      for {parent, child} <- edges do
        assert entry[parent] < entry[child]
        assert exit[parent] > exit[child]
      end
    end
  end

  test "forests cover arbitrary vertices once beyond the small-map threshold" do
    vertices = Enum.to_list(1..40) ++ [1.0, nil, false, "root", {:tuple, 1}, %{id: :map}]

    for type <- [:directed, :undirected] do
      g =
        Graph.new(type: type, vertex_identifier: fn v -> v end)
        |> Graph.add_vertices(vertices)
        |> Graph.add_edges([{1, 2}, {2, 1}, {nil, false}, {"root", {:tuple, 1}}, {1.0, 1.0}])

      for order <- [Graph.preorder(g), Graph.postorder(g)] do
        assert length(order) == length(vertices)
        assert MapSet.new(order) === MapSet.new(vertices)
      end
    end
  end

  test "the reported string examples both cover their graph" do
    for source <- ["123456", "12345"] do
      g = Graph.new() |> Graph.add_edge(source, "b")

      for order <- [Graph.preorder(g), Graph.postorder(g)] do
        assert MapSet.new(order) == MapSet.new([source, "b"])
        assert length(order) == 2
      end
    end
  end
end
