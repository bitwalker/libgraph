defmodule GraphTest do
  use ExUnit.Case
  doctest Graph
  alias Graph.Edge

  test "get info about graph" do
    g = build_basic_cyclic_graph()

    assert %{num_vertices: 5, num_edges: 7} = Graph.info(g)
  end

  test "is_cyclic?" do
    dg = build_basic_cyclic_digraph()
    refute :digraph_utils.is_acyclic(dg)

    g = build_basic_cyclic_graph()
    assert Graph.is_cyclic?(g)
    refute Graph.is_acyclic?(g)
  end

  test "is_acyclic?" do
    dg = build_basic_acyclic_digraph()
    assert :digraph_utils.is_acyclic(dg)

    g = build_basic_acyclic_graph()
    assert Graph.is_acyclic?(g)
    refute Graph.is_cyclic?(g)
  end

  test "is_tree?" do
    dg = build_basic_tree_digraph()
    assert :digraph_utils.is_tree(dg)

    g = build_basic_tree_graph()
    assert Graph.is_tree?(g)
  end

  test "is_arborescence?" do
    dg = build_basic_tree_digraph()
    assert :digraph_utils.is_arborescence(dg)

    g = build_basic_tree_graph()
    assert Graph.is_arborescence?(g)
  end

  test "arborescence_root" do
    dg = build_basic_tree_digraph()
    assert {:yes, root} = :digraph_utils.arborescence_root(dg)

    g = build_basic_tree_graph()
    assert ^root = Graph.arborescence_root(g)
  end

  test "is_subgraph?" do
    g = build_basic_tree_graph()
    sg = Graph.subgraph(g, [:a, :b, :c])
    assert Graph.is_subgraph?(sg, g)
  end

  test "topsort" do
    dg = build_basic_acyclic_digraph()
    dg_sorted = :digraph_utils.topsort(dg)
    assert is_list(dg_sorted)

    g = build_basic_acyclic_graph()
    assert ^dg_sorted = Graph.topsort(g)
  end

  test "find all paths" do
    g = build_basic_cyclic_graph()

    assert [[:a, :c, :d, :e], [:a, :b, :d, :e], [:a, :b, :c, :d, :e]] = Graph.get_paths(g, :a, :e)
  end

  test "find shortest path" do
    g = build_basic_cyclic_graph()

    assert [:a, :b, :d, :e] = Graph.get_shortest_path(g, :a, :e)
  end

  test "out_edges" do
    g = build_basic_acyclic_graph()
    assert [%Edge{v1: :c, v2: :d}] = Graph.out_edges(g, :c)
  end

  test "in_edges" do
    g = build_basic_acyclic_graph()
    assert [%Edge{v1: :b, v2: :d}, %Edge{v1: :c, v2: :d}] = Graph.in_edges(g, :d)
  end

  test "out_neighbors" do
    g = build_basic_acyclic_graph()
    assert [:d] = Graph.out_neighbors(g, :c)
  end

  test "in_neighbors" do
    g = build_basic_acyclic_graph()
    assert [:b, :c] = Graph.in_neighbors(g, :d)
  end

  defp build_basic_cyclic_graph do
    Graph.new
    |> Graph.add_vertex(:a)
    |> Graph.add_vertex(:b)
    |> Graph.add_vertex(:c)
    |> Graph.add_vertex(:d)
    |> Graph.add_vertex(:e)
    |> Graph.add_edge(:a, :b)
    |> Graph.add_edge(:a, :c)
    |> Graph.add_edge(:b, :c)
    |> Graph.add_edge(:b, :d)
    |> Graph.add_edge(:c, :d)
    |> Graph.add_edge(:c, :a)
    |> Graph.add_edge(:d, :e)
  end

  defp build_basic_cyclic_digraph do
    dg = :digraph.new
    :digraph.add_vertex(dg, :a)
    :digraph.add_vertex(dg, :b)
    :digraph.add_vertex(dg, :c)
    :digraph.add_vertex(dg, :d)
    :digraph.add_vertex(dg, :e)
    :digraph.add_edge(dg, :a, :b)
    :digraph.add_edge(dg, :a, :c)
    :digraph.add_edge(dg, :b, :c)
    :digraph.add_edge(dg, :b, :d)
    :digraph.add_edge(dg, :c, :d)
    :digraph.add_edge(dg, :c, :a)
    :digraph.add_edge(dg, :d, :e)
    dg
  end

  defp build_basic_acyclic_graph do
    Graph.new
    |> Graph.add_vertex(:a)
    |> Graph.add_vertex(:b)
    |> Graph.add_vertex(:c)
    |> Graph.add_vertex(:d)
    |> Graph.add_vertex(:e)
    |> Graph.add_edge(:a, :b)
    |> Graph.add_edge(:a, :c)
    |> Graph.add_edge(:b, :c)
    |> Graph.add_edge(:b, :d)
    |> Graph.add_edge(:c, :d)
    |> Graph.add_edge(:d, :e)
  end

  defp build_basic_acyclic_digraph do
    dg = :digraph.new
    :digraph.add_vertex(dg, :a)
    :digraph.add_vertex(dg, :b)
    :digraph.add_vertex(dg, :c)
    :digraph.add_vertex(dg, :d)
    :digraph.add_vertex(dg, :e)
    :digraph.add_edge(dg, :a, :b)
    :digraph.add_edge(dg, :a, :c)
    :digraph.add_edge(dg, :b, :c)
    :digraph.add_edge(dg, :b, :d)
    :digraph.add_edge(dg, :c, :d)
    :digraph.add_edge(dg, :d, :e)
    dg
  end

  defp build_basic_tree_graph do
    Graph.new
    |> Graph.add_vertex(:a)
    |> Graph.add_vertex(:b)
    |> Graph.add_vertex(:c)
    |> Graph.add_vertex(:d)
    |> Graph.add_vertex(:e)
    |> Graph.add_edge(:a, :b)
    |> Graph.add_edge(:b, :c)
    |> Graph.add_edge(:c, :d)
    |> Graph.add_edge(:c, :e)
  end

  defp build_basic_tree_digraph do
    dg = :digraph.new
    :digraph.add_vertex(dg, :a)
    :digraph.add_vertex(dg, :b)
    :digraph.add_vertex(dg, :c)
    :digraph.add_vertex(dg, :d)
    :digraph.add_vertex(dg, :e)
    :digraph.add_edge(dg, :a, :b)
    :digraph.add_edge(dg, :b, :c)
    :digraph.add_edge(dg, :c, :d)
    :digraph.add_edge(dg, :c, :e)
    dg
  end
end
