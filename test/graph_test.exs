defmodule GraphTest do
  use ExUnit.Case, async: true
  doctest Graph
  doctest Graph.Edge
  alias Graph.Edge
  alias Graph.Test.Generators

  test "inspect" do
    g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}, {:b, :c, weight: 3}, {:b, :a, label: {:complex, :label}}])
    ug = Graph.new(type: :undirected) |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}, {:b, :c, weight: 3}, {:b, :a, label: {:complex, :label}}])

    # structs: false
    structs_false = "#{inspect g, structs: false}"
    doc = Inspect.Algebra.format(Inspect.Algebra.to_doc(g, %Inspect.Opts{structs: false}), 99999)
    assert ^structs_false = :erlang.iolist_to_binary(doc)

    # pretty printed
    str = "#{inspect g}"
    assert "#Graph<type: directed, vertices: [:a, :b, :c], edges: [:a -[foo]-> :b, :a -> :b, :b -[{:complex, :label}]-> :a, :b -> :c]>" = str
    ustr = "#{inspect ug}"
    assert "#Graph<type: undirected, vertices: [:a, :b, :c], edges: [:a <-[foo]-> :b, :a <-> :b, :a <-[{:complex, :label}]-> :b, :b <-> :c]>" = ustr

    # large graph
    g = Enum.reduce(1..150, Graph.new, fn i, g -> Graph.add_edge(g, i, i+1) end)
    str = "#{inspect g}"
    assert "#Graph<type: directed, num_vertices: 151, num_edges: 150>" = str
  end

  test "get info about graph" do
    g = build_basic_cyclic_graph()

    assert %{type: :directed, num_vertices: 5, num_edges: 7} = Graph.info(g)
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

  test "shortest path is correct" do
    g = Generators.dag(1_000)
    dg = Generators.libgraph_to_digraph(g)

    paths = Graph.get_paths(g, 1, 1_000)

    shortest_g = Graph.dijkstra(g, 1, 1_000)
    shortest_dg = :digraph.get_short_path(dg, 1, 1_000)
    assert is_list(shortest_g)
    assert is_list(shortest_dg)

    assert is_list(paths)
    [shortest|_] = Enum.sort_by(paths, &length/1)
    shortest_len = length(shortest)

    assert ^shortest_len = length(shortest_dg)
    assert ^shortest_len = length(shortest_g)
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

  test "k_core/2" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
          {:a, :b}, {:a, :c}, {:a, :d}, {:b, :c}, {:b, :d}, {:c, :d},
          {:c, :e}, {:e, :f}, {:f, :g}, {:f, :h}
        ])
    zero_core = Graph.k_core(g, 0)
    assert Graph.is_subgraph?(zero_core, g)
    assert Graph.vertices(g) == Graph.vertices(zero_core)

    one_core = Graph.k_core(g, 1)
    assert Graph.is_subgraph?(one_core, zero_core)
    assert Graph.vertices(one_core) == [:a, :b, :c, :d, :e, :f, :g, :h]

    three_core = Graph.k_core(g, 3)
    assert Graph.is_subgraph?(three_core, one_core)
    assert Graph.vertices(three_core) == [:a, :b, :c, :d]

    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
        {:a, :b}, {:a, :c}, {:a, :d}, {:b, :c}, {:b, :d}, {:c, :d}, {:d, :e},
        {:e, :f}, {:f, :g}, {:g, :h}, {:h, :i}, {:i, :f}, {:f, :h}, {:i, :g}
      ])
    three_core = Graph.k_core(g, 3)
    assert Graph.vertices(three_core) == [:a, :b, :c, :d, :f, :g, :h, :i]
  end

  test "k_core_components/1" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
      {:a, :b}, {:a, :c}, {:a, :d}, {:b, :c}, {:b, :d}, {:c, :d},
      {:c, :e}, {:e, :f}, {:f, :g}, {:f, :h}
    ])
    components = Graph.k_core_components(g)
    assert [:i] = components[0]
    assert [:e, :f, :g, :h] = components[1]
    assert is_nil(components[2])
    assert [:a, :b, :c, :d] = components[3]
  end

  test "coreness/2" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
      {:a, :b}, {:a, :c}, {:a, :d}, {:b, :c}, {:b, :d}, {:c, :d},
      {:c, :e}, {:e, :f}, {:f, :g}, {:f, :h}
    ])
    assert 3 = Graph.coreness(g, :a)
  end

  test "degeneracy_core/1" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
      {:a, :b}, {:a, :c}, {:a, :d}, {:b, :c}, {:b, :d}, {:c, :d},
      {:c, :e}, {:e, :f}, {:f, :g}, {:f, :h}
    ])
    assert 3 = Graph.degeneracy(g)
    dg = Graph.degeneracy_core(g)
    assert [:a, :b, :c, :d] = Graph.vertices(dg)
  end

  @tag timeout: 120_000
  @enron_emails Path.join([__DIR__, "fixtures", "email-Enron.txt"])
  test "degeneracy/1 - Enron emails" do
    g = Graph.Test.Fixtures.Parser.parse @enron_emails
    assert 36_692 = Graph.num_vertices(g)
    assert 183_831 = Graph.num_edges(g)
    assert 43 = Graph.degeneracy(g)
  end

  @tag timeout: 120_000
  @hamster_friends Path.join([__DIR__, "fixtures", "petster", "edges.txt"])
  test "degeneracy/1 - Petster hamster friendships" do
    g = Graph.Test.Fixtures.Parser.parse @hamster_friends
    assert 1_858 = Graph.num_vertices(g)
    assert 12_534 = Graph.num_edges(g)
    assert 20 = Graph.degeneracy(g)
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
