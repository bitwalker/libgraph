defmodule GraphTest do
  use ExUnit.Case, async: true
  doctest Graph
  doctest Graph.Edge
  alias Graph.Edge
  alias Graph.Test.Generators

  test "injectable vertex_identifier" do
    g = Graph.new()

    g_with_custom_vertex_identifier =
      Graph.new(vertex_identifier: fn v -> :erlang.phash2(v, trunc(:math.pow(2, 16))) end)

    g = Graph.add_vertex(g, :v1, :labelA)

    g_with_custom_vertex_identifier =
      Graph.add_vertex(g_with_custom_vertex_identifier, :v1, :labelA)

    assert Graph.has_vertex?(g, :v1)
    assert Graph.has_vertex?(g_with_custom_vertex_identifier, :v1)
  end

  describe "multigraphs" do
    test "`multigraph: true` option enables edge indexing on edge labels" do
      graph =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b},
          {:a, :b, label: :foo},
          {:a, :b, label: :bar},
          {:b, :c, weight: 3},
          {:b, :a, label: {:complex, :label}}
        ])

      assert Enum.count(Graph.out_edges(graph, :a)) == 3
      assert [%Edge{label: :foo}] = Graph.out_edges(graph, :a, by: [:foo])
      assert [%Edge{label: :foo}] = Graph.out_edges(graph, :a, by: :foo)
      assert [%Edge{label: :foo}] = Graph.in_edges(graph, :b, by: :foo)
      assert [%Edge{label: :bar}] = Graph.out_edges(graph, :a, by: :bar)
      assert [%Edge{label: nil}] = Graph.out_edges(graph, :a, by: nil)

      assert [%Edge{label: nil}] = Graph.out_edges(graph, :a, by: nil)

      assert [%Edge{label: {:complex, :label}}] =
               Graph.out_edges(graph, :b,
                 where: fn edge -> edge.label == {:complex, :label} or edge.label == :bar end
               )

      assert 1 == graph |> Graph.edges(by: :foo) |> Enum.count()
      assert 1 == graph |> Graph.edges(where: fn edge -> edge.weight > 2 end) |> Enum.count()

      assert 1 ==
               graph
               |> Graph.edges(:a, by: :foo)
               |> Enum.count()

      assert 2 ==
               graph
               |> Graph.edges(by: [:foo, :bar])
               |> Enum.count()

      assert 1 ==
               graph
               |> Graph.edges(by: [:foo, :bar], where: fn edge -> edge.label == :bar end)
               |> Enum.count()

      assert [] == Graph.out_edges(graph, :a, by: :foobar)
    end

    test "custom edge partition_by function" do
      graph =
        Graph.new(multigraph: true, partition_by: fn edge -> [edge.weight] end)
        |> Graph.add_edges([
          {:a, :b},
          {:a, :b, label: :foo},
          {:a, :b, label: :bar},
          {:b, :c, weight: 3},
          {:b, :a, weight: 6}
        ])

      assert Enum.count(Graph.out_edges(graph, :b)) == 2

      assert [%Edge{weight: 6}] =
               Graph.out_edges(graph, :b, where: fn edge -> edge.weight == 6 end)

      assert [%Edge{weight: 3}] =
               Graph.out_edges(graph, :b, where: fn edge -> edge.weight == 3 end)
    end

    test "custom partition_by supports indexing to more than one partition" do
      graph =
        Graph.new(multigraph: true, partition_by: fn edge -> [edge.weight, edge.label] end)
        |> Graph.add_edges([
          {:a, :b},
          {:a, :d, label: :foo},
          {:a, :b, label: :bar},
          {:b, :c, weight: 3},
          {:b, :a, weight: 6, label: :foo}
        ])

      assert Enum.count(Graph.out_edges(graph, :b)) == 2

      assert [%Edge{weight: 6, label: :foo}] =
               Graph.out_edges(graph, :b, by: 6)

      assert Enum.count(Graph.edges(graph, by: [:foo])) == 2
    end

    test "removing edges prunes index" do
      g =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b},
          {:a, :b, label: :foo},
          {:a, :b, label: :bar},
          {:b, :c, weight: 3},
          {:b, :a, label: {:complex, :label}}
        ])

      g = Graph.delete_edges(g, [{:b, :c}, {:b, :a}])
      refute Map.has_key?(g.edge_index, {:complex, :label})
      assert Enum.empty?(Graph.edges(g, by: [{:complex, :label}]))
      # nil partition still exists for a->b nil-label edge
      assert Map.has_key?(g.edge_index, nil)
    end

    test "delete_edge/3 removes only a multigraph's properties and index for the given partition key/label" do
      g =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b},
          {:a, :b, label: :foo},
          {:a, :b, label: :bar},
          {:b, :c, weight: 3},
          {:b, :a, label: {:complex, :label}}
        ])

      g = Graph.delete_edge(g, :a, :b, :foo)

      refute Map.has_key?(g.edge_index, :foo)
      assert Enum.empty?(Graph.out_edges(g, :a, by: :foo))
      assert Enum.empty?(Graph.edges(g, by: :foo))
    end

    test "update_labelled_edge/3 updates an indexed adge with new label" do
      g =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b},
          {:a, :b, label: :foo},
          {:a, :b, label: :bar},
          {:b, :c, weight: 3},
          {:b, :a, label: {:complex, :label}}
        ])

      g = Graph.update_labelled_edge(g, :a, :b, :foo, label: :baz)

      refute Map.has_key?(g.edge_index, :foo)
      assert Map.has_key?(g.edge_index[:baz], g.vertex_identifier.(:a))
    end

    test "update_labelled_edge preserves sibling edges in partition index" do
      g =
        Graph.new(multigraph: true)
        |> Graph.add_edge(:fact, :join, label: :runnable)
        |> Graph.add_edge(:fact, :step_a, label: :runnable)
        |> Graph.add_edge(:fact, :step_b, label: :runnable)

      assert length(Graph.edges(g, by: [:runnable])) == 3

      g = Graph.update_labelled_edge(g, :fact, :join, :runnable, label: :ran)

      ran_edges = Graph.edges(g, by: [:ran])
      assert length(ran_edges) == 1
      assert hd(ran_edges).v1 == :fact and hd(ran_edges).v2 == :join

      runnable_edges = Graph.edges(g, by: [:runnable])
      assert length(runnable_edges) == 2

      runnable_targets = Enum.map(runnable_edges, & &1.v2) |> Enum.sort()
      assert runnable_targets == [:step_a, :step_b]
    end

    test "delete_vertex prunes edge_index" do
      g =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b, label: :foo},
          {:a, :c, label: :bar},
          {:b, :c, label: :foo},
          {:c, :d, label: :baz}
        ])

      assert Map.has_key?(g.edge_index, :foo)
      assert Map.has_key?(g.edge_index, :bar)

      g = Graph.delete_vertex(g, :a)

      refute Graph.has_vertex?(g, :a)
      # :bar partition only had a->c, should be gone
      refute Map.has_key?(g.edge_index, :bar)
      # :foo partition still has b->c
      assert Map.has_key?(g.edge_index, :foo)
      assert [%Edge{v1: :b, v2: :c, label: :foo}] = Graph.edges(g, by: [:foo])

      # no stale references to deleted vertex
      Enum.each(g.edge_index, fn {_partition, vertex_map} ->
        refute Map.has_key?(vertex_map, g.vertex_identifier.(:a))

        Enum.each(vertex_map, fn {_v_id, edge_keys} ->
          Enum.each(edge_keys, fn {v1_id, v2_id} ->
            refute v1_id == g.vertex_identifier.(:a)
            refute v2_id == g.vertex_identifier.(:a)
          end)
        end)
      end)
    end

    test "delete_vertices prunes edge_index" do
      g =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b, label: :foo},
          {:b, :c, label: :bar},
          {:c, :d, label: :baz}
        ])

      g = Graph.delete_vertices(g, [:a, :c])

      refute Map.has_key?(g.edge_index, :foo)
      refute Map.has_key?(g.edge_index, :bar)
      refute Map.has_key?(g.edge_index, :baz)
      assert g.edge_index == %{}
    end

    test "transpose preserves edge_index with flipped edge keys" do
      g =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b, label: :foo},
          {:b, :c, label: :bar}
        ])

      gt = Graph.transpose(g)

      assert [%Edge{v1: :b, v2: :a, label: :foo}] = Graph.out_edges(gt, :b, by: :foo)
      assert [%Edge{v1: :c, v2: :b, label: :bar}] = Graph.out_edges(gt, :c, by: :bar)
      assert Enum.empty?(Graph.out_edges(gt, :a, by: :foo))
    end

    test "split_edge prunes old edge and indexes new edges" do
      g =
        Graph.new(multigraph: true)
        |> Graph.add_edge(:a, :c, label: :foo)
        |> Graph.add_edge(:a, :c, label: :bar)

      g = Graph.split_edge(g, :a, :c, :b)

      # old a->c edges should be gone from index
      a_id = g.vertex_identifier.(:a)
      c_id = g.vertex_identifier.(:c)

      Enum.each(g.edge_index, fn {_partition, vertex_map} ->
        Enum.each(vertex_map, fn {_v_id, edge_keys} ->
          refute MapSet.member?(edge_keys, {a_id, c_id})
        end)
      end)

      # new edges a->b and b->c should be indexed
      assert [%Edge{v1: :a, v2: :b, label: :foo}] = Graph.out_edges(g, :a, by: :foo)
      assert [%Edge{v1: :a, v2: :b, label: :bar}] = Graph.out_edges(g, :a, by: :bar)
      assert [%Edge{v1: :b, v2: :c, label: :foo}] = Graph.out_edges(g, :b, by: :foo)
      assert [%Edge{v1: :b, v2: :c, label: :bar}] = Graph.out_edges(g, :b, by: :bar)
    end

    test "subgraph preserves multigraph settings and rebuilds index" do
      g =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b, label: :foo},
          {:b, :c, label: :bar},
          {:c, :d, label: :baz}
        ])

      sg = Graph.subgraph(g, [:a, :b, :c])

      assert sg.multigraph == true
      assert [%Edge{v1: :a, v2: :b, label: :foo}] = Graph.edges(sg, by: [:foo])
      assert [%Edge{v1: :b, v2: :c, label: :bar}] = Graph.edges(sg, by: [:bar])
      # :baz edge is not in subgraph since :d is excluded
      assert Enum.empty?(Graph.edges(sg, by: [:baz]))
    end

    test "BFS traversal using multigraph partitions" do
      graph =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b, label: :foo},
          {:a, :b, label: :bar},
          {:a, :c, label: :foo},
          {:b, :d, label: :bar},
          {:c, :d, label: :foo}
        ])

      # BFS following only :foo edges: a -> b, a -> c, c -> d
      foo_result = Graph.Reducers.Bfs.map(graph, fn v -> v end, by: :foo)
      assert :a == hd(foo_result)
      assert MapSet.new(foo_result) == MapSet.new([:a, :b, :c, :d])

      # BFS following only :bar edges: a -> b, b -> d
      bar_result = Graph.Reducers.Bfs.map(graph, fn v -> v end, by: :bar)
      assert :a == hd(bar_result)
      assert MapSet.new(bar_result) == MapSet.new([:a, :b, :d])
    end

    test "DFS traversal using multigraph partitions" do
      graph =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b, label: :foo},
          {:a, :b, label: :bar},
          {:a, :c, label: :foo},
          {:b, :d, label: :bar},
          {:c, :d, label: :foo}
        ])

      # DFS following only :foo edges: a -> b, a -> c -> d
      foo_result = Graph.Reducers.Dfs.map(graph, fn v -> v end, by: :foo)
      assert :a == hd(foo_result)
      assert MapSet.new(foo_result) == MapSet.new([:a, :b, :c, :d])

      # DFS following only :bar edges: a -> b -> d
      bar_result = Graph.Reducers.Dfs.map(graph, fn v -> v end, by: :bar)
      assert :a == hd(bar_result)
      assert MapSet.new(bar_result) == MapSet.new([:a, :b, :d])
    end

    test "Dijkstra with multigraph partition filtering" do
      graph =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b, label: :fast, weight: 1},
          {:a, :c, label: :slow, weight: 10},
          {:b, :d, label: :fast, weight: 1},
          {:c, :d, label: :slow, weight: 1}
        ])

      # Via :fast edges only: a->b->d, cost 2
      assert [:a, :b, :d] = Graph.dijkstra(graph, :a, :d, by: :fast)

      # Via :slow edges only: a->c->d, cost 11
      assert [:a, :c, :d] = Graph.dijkstra(graph, :a, :d, by: :slow)

      # No :fast path from :a to :c
      assert nil == Graph.dijkstra(graph, :a, :c, by: :fast)
    end

    test "A* with multigraph partition filtering" do
      graph =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b, label: :fast, weight: 1},
          {:a, :c, label: :slow, weight: 10},
          {:b, :d, label: :fast, weight: 1},
          {:c, :d, label: :slow, weight: 1}
        ])

      assert [:a, :b, :d] = Graph.a_star(graph, :a, :d, fn _ -> 0 end, by: :fast)
      assert [:a, :c, :d] = Graph.a_star(graph, :a, :d, fn _ -> 0 end, by: :slow)
    end

    test "Bellman-Ford with multigraph partition filtering" do
      graph =
        Graph.new(multigraph: true)
        |> Graph.add_edges([
          {:a, :b, label: :fast, weight: 1},
          {:a, :c, label: :slow, weight: 10},
          {:b, :d, label: :fast, weight: 2},
          {:c, :d, label: :slow, weight: 1}
        ])

      result = Graph.bellman_ford(graph, :a, by: :fast)
      assert result[:a] == 0
      assert result[:b] == 1
      assert result[:d] == 3
      # :c is not reachable via :fast edges
      assert result[:c] == :infinity
    end
  end

  describe "edge properties" do
    test "setting edge properties" do
      g =
        Graph.new()
        |> Graph.add_edges([
          {:a, :b, properties: %{foo: :bar}},
          {:a, :b, label: :foo, properties: %{bar: :foo}}
        ])

      edges = Graph.out_edges(g, :a) |> Enum.sort_by(fn e -> {e.label != nil, e.label} end)

      assert [
               %Edge{v1: :a, v2: :b, properties: %{foo: :bar}},
               %Edge{v1: :a, v2: :b, label: :foo, properties: %{bar: :foo}}
             ] = edges
    end

    test "updating edge properties" do
      g =
        Graph.new()
        |> Graph.add_edges([
          {:a, :b, properties: %{foo: :bar}},
          {:a, :b, label: :foo, properties: %{bar: :foo}}
        ])
        |> Graph.update_edge(:a, :b, properties: %{ham: :potato})
        |> Graph.update_labelled_edge(:a, :b, :foo, properties: %{potato: :ham})

      edges = Graph.out_edges(g, :a) |> Enum.sort_by(fn e -> {e.label != nil, e.label} end)

      assert [
               %Edge{v1: :a, v2: :b, properties: %{ham: :potato}},
               %Edge{v1: :a, v2: :b, label: :foo, properties: %{potato: :ham}}
             ] = edges
    end

    test "adding edge struct with properties" do
      g =
        Graph.new()

      edge = Edge.new(:a, :b, properties: %{foo: :bar})

      g = Graph.add_edge(g, edge)

      assert [
               %Edge{v1: :a, v2: :b, properties: %{foo: :bar}}
             ] = Graph.out_edges(g, :a)
    end
  end

  test "delete vertex" do
    g = Graph.new()
    g = Graph.add_vertex(g, :v1, :labelA)
    g = Graph.delete_vertex(g, :v1)
    g = Graph.add_vertex(g, :v1, :labelB)

    assert [:labelB] = Graph.vertex_labels(g, :v1)
  end

  test "delete vertices" do
    graph =
      Graph.new()
      |> Graph.add_vertices([1, 2, 4, 6])
      |> Graph.add_edge(1, 2)
      |> Graph.add_edge(2, 4)
      |> Graph.add_edge(4, 6)

    graph_two =
      graph
      |> Graph.add_vertices([3, 5, 7])
      |> Graph.add_edge(1, 3)
      |> Graph.add_edge(3, 4)
      |> Graph.add_edge(3, 5)
      |> Graph.add_edge(5, 6)
      |> Graph.add_edge(5, 7)

    assert graph == Graph.delete_vertices(graph_two, [3, 5, 7])
  end

  test "inspect" do
    g =
      Graph.new()
      |> Graph.add_edges([
        {:a, :b},
        {:a, :b, label: :foo},
        {:b, :c, weight: 3},
        {:b, :a, label: {:complex, :label}}
      ])

    ug =
      Graph.new(type: :undirected)
      |> Graph.add_edges([
        {:a, :b},
        {:a, :b, label: :foo},
        {:b, :c, weight: 3},
        {:b, :a, label: {:complex, :label}}
      ])

    # structs: false
    structs_false = "#{inspect(g, structs: false)}"
    doc = Inspect.Algebra.format(Inspect.Algebra.to_doc(g, %Inspect.Opts{structs: false}), 99999)
    assert ^structs_false = :erlang.iolist_to_binary(doc)

    # pretty printed - edge order within a vertex pair is non-deterministic (map iteration)
    str = "#{inspect(g)}"

    assert str =~ ~r/^#Graph<type: directed, vertices: \[:a, :b, :c\], edges: \[/
    assert str =~ ":a -> :b"
    assert str =~ ":a -[foo]-> :b"
    assert str =~ ":b -[{:complex, :label}]-> :a"
    assert str =~ ":b -> :c"

    ustr = "#{inspect(ug)}"

    assert ustr =~ ~r/^#Graph<type: undirected, vertices: \[:a, :b, :c\], edges: \[/
    assert ustr =~ ":a <-> :b"
    assert ustr =~ ":a <-[foo]-> :b"
    assert ustr =~ ":a <-[{:complex, :label}]-> :b"
    assert ustr =~ ":b <-> :c"

    # large graph
    g = Enum.reduce(1..150, Graph.new(), fn i, g -> Graph.add_edge(g, i, i + 1) end)
    str = "#{inspect(g)}"
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

  test "edges/2 returns both directions" do
    generated_result =
      Graph.new()
      |> Graph.add_edges([
        {:a, :b, label: "label1"},
        {:a, :b, label: "label2"},
        {:b, :a, label: "label3"}
      ])
      |> Graph.edges(:a)

    for edge <- generated_result do
      assert edge.label in ["label1", "label2", "label3"] and
               ((edge.v1 == :a and edge.v2 == :b) or
                  (edge.v1 == :b and edge.v2 == :a))
    end
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

    assert [[:a, :c, :d, :e], [:a, :b, :d, :e], [:a, :b, :c, :d, :e]] =
             Graph.get_paths(g, :a, :e)
  end

  test "find all paths on loopy graph" do
    g =
      Graph.new()
      |> Graph.add_edge(:a, :b)
      |> Graph.add_edge(:a, :c)
      |> Graph.add_edge(:b, :d)
      |> Graph.add_edge(:c, :d)
      |> Graph.add_edge(:d, :e)
      |> Graph.add_edge(:e, :d)
      |> Graph.add_edge(:d, :f)
      |> Graph.add_edge(:f, :d)

    assert [[:a, :c, :d], [:a, :b, :d]] == Graph.get_paths(g, :a, :d)
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
    [shortest | _] = Enum.sort_by(paths, &length/1)
    shortest_len = length(shortest)

    assert ^shortest_len = length(shortest_dg)
    assert ^shortest_len = length(shortest_g)
  end

  test "shortest path for complex graph" do
    g = build_complex_graph()

    shortest_g = Graph.dijkstra(g, "start", "end")

    assert shortest_g ==
             [
               "start",
               "start_0",
               96,
               97,
               98,
               33,
               100,
               34,
               35,
               36,
               37,
               19,
               65,
               66,
               67,
               "end_0",
               "end"
             ]
  end

  test "shortest path for complex undirected graph" do
    g = build_complex_graph(:undirected)

    shortest_g = Graph.dijkstra(g, "start", "end")

    assert shortest_g ==
             ["start", "start_0", 95, 94, 93, 39, 38, 21, 69, 68, "end_0", "end"]
  end

  test "shortest path for complex graph using float weights" do
    g = build_complex_graph_float()

    shortest_g = Graph.dijkstra(g, "start", "end")

    assert shortest_g ==
             [
               "start",
               "start_0",
               96,
               97,
               98,
               33,
               100,
               34,
               35,
               36,
               37,
               19,
               65,
               66,
               67,
               "end_0",
               "end"
             ]
  end

  test "shortest path for complex undirected graph using float weights" do
    g = build_complex_graph_float(:undirected)

    shortest_g = Graph.dijkstra(g, "start", "end")

    assert shortest_g ==
             ["start", "start_0", 95, 94, 93, 39, 38, 21, 69, 68, "end_0", "end"]
  end

  test "shortest paths for complex graph using signed weights (negative and positive)" do
    g = build_complex_signed_graph()
    shortest_paths = Graph.bellman_ford(g, :a)
    assert shortest_paths == %{a: 0, b: -1, c: 2, d: -2, e: 1}
  end

  test "edge undirected graph v1 > v2" do
    g = build_basic_undirected_graph()
    e1 = Graph.edge(g, :a, :b)
    e2 = Graph.edge(g, :b, :a)
    assert e1 == e2
  end

  test "edge undirected graph v1 < v2" do
    g = build_basic_undirected_graph()
    e1 = Graph.edge(g, :b, :c)
    e2 = Graph.edge(g, :c, :b)
    assert e1 == e2
  end

  test "edges undirected graph v1 > v2" do
    g = build_basic_undirected_graph()
    e1 = Graph.edges(g, :a, :b)
    e2 = Graph.edges(g, :b, :a)
    assert e1 == e2
  end

  test "edges undirected graph v1 < v2" do
    g = build_basic_undirected_graph()
    e1 = Graph.edges(g, :b, :c)
    e2 = Graph.edges(g, :c, :b)
    assert e1 == e2
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

  test "cliques/1" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f])
      |> Graph.add_edges([
        {:a, :b},
        {:b, :c},
        {:c, :d},
        {:d, :e},
        {:e, :a},
        {:e, :b},
        {:d, :f}
      ])

    cliques = Graph.cliques(g) |> Enum.map(&Enum.sort/1) |> Enum.sort()
    assert cliques == [[:a, :b, :e], [:b, :c], [:c, :d], [:d, :e], [:d, :f]]
  end

  test "k_cliques/2" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f])
      |> Graph.add_edges([
        {:a, :b},
        {:b, :c},
        {:c, :d},
        {:d, :e},
        {:e, :a},
        {:e, :b},
        {:d, :f}
      ])

    assert [[:a, :b, :e]] = Graph.k_cliques(g, 3)
  end

  test "k_core/2" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
        {:a, :b},
        {:a, :c},
        {:a, :d},
        {:b, :c},
        {:b, :d},
        {:c, :d},
        {:c, :e},
        {:e, :f},
        {:f, :g},
        {:f, :h}
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
        {:a, :b},
        {:a, :c},
        {:a, :d},
        {:b, :c},
        {:b, :d},
        {:c, :d},
        {:d, :e},
        {:e, :f},
        {:f, :g},
        {:g, :h},
        {:h, :i},
        {:i, :f},
        {:f, :h},
        {:i, :g}
      ])

    three_core = Graph.k_core(g, 3)
    assert Graph.vertices(three_core) == [:a, :b, :c, :d, :f, :g, :h, :i]

    g =
      Graph.new()
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
        {:a, :b},
        {:a, :c},
        {:a, :d},
        {:b, :a},
        {:b, :c},
        {:b, :d},
        {:c, :a},
        {:c, :b},
        {:c, :d},
        {:d, :a},
        {:d, :b},
        {:d, :c},
        {:d, :e},
        {:e, :d},
        {:e, :f},
        {:f, :e},
        {:f, :g},
        {:g, :f},
        {:g, :h},
        {:h, :g},
        {:h, :i},
        {:i, :h},
        {:i, :f},
        {:f, :i},
        {:h, :f},
        {:f, :h},
        {:g, :i},
        {:i, :g}
      ])

    three_core = Graph.k_core(g, 3)
    assert Graph.vertices(three_core) == [:a, :b, :c, :d, :f, :g, :h, :i]
  end

  test "k_core_components/1" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
        {:a, :a},
        {:a, :b},
        {:a, :c},
        {:a, :d},
        {:b, :c},
        {:b, :d},
        {:c, :d},
        {:c, :e},
        {:e, :f},
        {:f, :g},
        {:f, :h}
      ])

    components = Graph.k_core_components(g)
    assert Enum.sort(components[0]) == [:i]
    assert Enum.sort(components[1]) == [:e, :f, :g, :h]
    assert is_nil(components[2])
    assert Enum.sort(components[3]) == [:a, :b, :c, :d]
  end

  test "coreness/2" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
        {:a, :b},
        {:a, :c},
        {:a, :d},
        {:b, :c},
        {:b, :d},
        {:c, :d},
        {:c, :e},
        {:e, :f},
        {:f, :g},
        {:f, :h}
      ])

    assert 3 = Graph.coreness(g, :a)
  end

  test "degeneracy_core/1" do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_vertices([:a, :b, :c, :d, :e, :f, :g, :h, :i])
      |> Graph.add_edges([
        {:a, :b},
        {:a, :c},
        {:a, :d},
        {:b, :c},
        {:b, :d},
        {:c, :d},
        {:c, :e},
        {:e, :f},
        {:f, :g},
        {:f, :h}
      ])

    assert 3 = Graph.degeneracy(g)
    dg = Graph.degeneracy_core(g)
    assert [:a, :b, :c, :d] = Graph.vertices(dg)
  end

  @tag timeout: 120_000
  @enron_emails Path.join([__DIR__, "fixtures", "email-Enron.txt"])
  test "degeneracy/1 - Enron emails" do
    g = Graph.Test.Fixtures.Parser.parse(@enron_emails)
    assert 36_692 = Graph.num_vertices(g)
    assert 183_831 = Graph.num_edges(g)
    assert 43 = Graph.degeneracy(g)
  end

  @tag timeout: 120_000
  @hamster_friends Path.join([__DIR__, "fixtures", "petster", "edges.txt"])
  test "degeneracy/1 - Petster hamster friendships" do
    g = Graph.Test.Fixtures.Parser.parse(@hamster_friends)
    assert 1_858 = Graph.num_vertices(g)
    assert 12_534 = Graph.num_edges(g)
    assert 20 = Graph.degeneracy(g)
  end

  defp build_basic_cyclic_graph do
    Graph.new()
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
    dg = :digraph.new()
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
    Graph.new()
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
    dg = :digraph.new()
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
    Graph.new()
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
    dg = :digraph.new()
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

  defp build_basic_undirected_graph do
    Graph.new(type: :undirected)
    |> Graph.add_vertices([:a, :b, :c])
    |> Graph.add_edge(:a, :b)
    |> Graph.add_edge(:c, :b)
  end

  defp build_complex_signed_graph do
    Graph.new()
    |> Graph.add_edge(:a, :b, weight: -1)
    |> Graph.add_edge(:b, :e, weight: 2)
    |> Graph.add_edge(:e, :d, weight: -3)
    |> Graph.add_edge(:d, :c, weight: 5)
    |> Graph.add_edge(:a, :c, weight: 4)
    |> Graph.add_edge(:b, :c, weight: 3)
    |> Graph.add_edge(:b, :d, weight: 2)
    |> Graph.add_edge(:d, :b, weight: 1)
  end

  defp build_complex_graph(type \\ :directed) do
    Graph.new(type: type)
    |> Graph.add_edge(42, 25, weight: 2525)
    |> Graph.add_edge(66, 67, weight: 2254)
    |> Graph.add_edge(71, 72, weight: 3895)
    |> Graph.add_edge(79, 80, weight: 37236)
    |> Graph.add_edge(0, 1, weight: 1573)
    |> Graph.add_edge(0, 64, weight: 1595)
    |> Graph.add_edge(30, 31, weight: 518)
    |> Graph.add_edge(58, 56, weight: 431)
    |> Graph.add_edge(58, 60, weight: 468)
    |> Graph.add_edge(58, 47, weight: 1175)
    |> Graph.add_edge(23, 24, weight: 1807)
    |> Graph.add_edge(50, 56, weight: 1192)
    |> Graph.add_edge(50, 49, weight: 198)
    |> Graph.add_edge(50, 57, weight: 1192)
    |> Graph.add_edge(22, 23, weight: 1919)
    |> Graph.add_edge(22, 91, weight: 4032)
    |> Graph.add_edge(43, 44, weight: 255)
    |> Graph.add_edge(60, 46, weight: 1167)
    |> Graph.add_edge(60, 55, weight: 159)
    |> Graph.add_edge(60, 58, weight: 468)
    |> Graph.add_edge(36, 37, weight: 9132)
    |> Graph.add_edge(75, 77, weight: 2120)
    |> Graph.add_edge(14, 15, weight: 3483)
    |> Graph.add_edge(32, 33, weight: 1008)
    |> Graph.add_edge(41, 20, weight: 2271)
    |> Graph.add_edge(101, 102, weight: 27752)
    |> Graph.add_edge(102, 104, weight: 44964)
    |> Graph.add_edge(102, 103, weight: 1287)
    |> Graph.add_edge(104, 78, weight: 944)
    |> Graph.add_edge(85, 86, weight: 3029)
    |> Graph.add_edge(72, 73, weight: 2872)
    |> Graph.add_edge(88, 89, weight: 7817)
    |> Graph.add_edge(103, 92, weight: 2884)
    |> Graph.add_edge(69, 70, weight: 1719)
    |> Graph.add_edge(69, 21, weight: 3059)
    |> Graph.add_edge(13, 14, weight: 3002)
    |> Graph.add_edge(84, 85, weight: 3735)
    |> Graph.add_edge(48, 47, weight: 204)
    |> Graph.add_edge(34, 35, weight: 6487)
    |> Graph.add_edge(80, 90, weight: 29876)
    |> Graph.add_edge(80, 103, weight: 2047)
    |> Graph.add_edge(95, "start_0", weight: 3130)
    |> Graph.add_edge(49, 50, weight: 198)
    |> Graph.add_edge(38, 39, weight: 11222)
    |> Graph.add_edge(37, 19, weight: 5284)
    |> Graph.add_edge(68, 69, weight: 2476)
    |> Graph.add_edge(77, 78, weight: 2138)
    |> Graph.add_edge(86, 87, weight: 8289)
    |> Graph.add_edge(61, 64, weight: 508)
    |> Graph.add_edge(61, 46, weight: 1181)
    |> Graph.add_edge(61, 59, weight: 490)
    |> Graph.add_edge("end_0", 68, weight: 288)
    |> Graph.add_edge("end_0", "end", weight: 0)
    |> Graph.add_edge(87, 88, weight: 5729)
    |> Graph.add_edge(94, 95, weight: 2665)
    |> Graph.add_edge(74, 75, weight: 1641)
    |> Graph.add_edge(12, 13, weight: 5014)
    |> Graph.add_edge(25, 30, weight: 1645)
    |> Graph.add_edge(25, 24, weight: 248)
    |> Graph.add_edge(15, 1, weight: 2005)
    |> Graph.add_edge(4, 12, weight: 3150)
    |> Graph.add_edge(54, 56, weight: 547)
    |> Graph.add_edge(54, 52, weight: 1332)
    |> Graph.add_edge(54, 27, weight: 2095)
    |> Graph.add_edge(70, 71, weight: 22390)
    |> Graph.add_edge(29, 32, weight: 2449)
    |> Graph.add_edge(59, 47, weight: 1190)
    |> Graph.add_edge(59, 57, weight: 418)
    |> Graph.add_edge(59, 61, weight: 490)
    |> Graph.add_edge(35, 36, weight: 6814)
    |> Graph.add_edge(52, 51, weight: 195)
    |> Graph.add_edge(52, 54, weight: 1332)
    |> Graph.add_edge(52, 55, weight: 1186)
    |> Graph.add_edge("start", "start_0", weight: 0)
    |> Graph.add_edge(78, 84, weight: 3418)
    |> Graph.add_edge(78, 79, weight: 6596)
    |> Graph.add_edge("start_0", 96, weight: 120)
    |> Graph.add_edge(39, 93, weight: 4220)
    |> Graph.add_edge(39, 40, weight: 5082)
    |> Graph.add_edge(45, 46, weight: 235)
    |> Graph.add_edge(18, 29, weight: 600)
    |> Graph.add_edge(73, 74, weight: 788)
    |> Graph.add_edge(98, 41, weight: 2187)
    |> Graph.add_edge(98, 33, weight: 1496)
    |> Graph.add_edge(93, 94, weight: 13132)
    |> Graph.add_edge(20, 42, weight: 566)
    |> Graph.add_edge(67, "end_0", weight: 483)
    |> Graph.add_edge(64, 0, weight: 1595)
    |> Graph.add_edge(64, 44, weight: 1174)
    |> Graph.add_edge(64, 61, weight: 508)
    |> Graph.add_edge(96, 13, weight: 2865)
    |> Graph.add_edge(96, 97, weight: 2773)
    |> Graph.add_edge(46, 60, weight: 1167)
    |> Graph.add_edge(46, 45, weight: 235)
    |> Graph.add_edge(46, 61, weight: 1181)
    |> Graph.add_edge(19, 70, weight: 2732)
    |> Graph.add_edge(19, 65, weight: 1911)
    |> Graph.add_edge(65, 66, weight: 2886)
    |> Graph.add_edge(51, 52, weight: 195)
    |> Graph.add_edge(33, 100, weight: 4369)
    |> Graph.add_edge(89, 21, weight: 3165)
    |> Graph.add_edge(89, 65, weight: 3221)
    |> Graph.add_edge(1, 0, weight: 1573)
    |> Graph.add_edge(1, 3, weight: 1999)
    |> Graph.add_edge(100, 93, weight: 2056)
    |> Graph.add_edge(100, 34, weight: 4038)
    |> Graph.add_edge(55, 60, weight: 159)
    |> Graph.add_edge(55, 52, weight: 1186)
    |> Graph.add_edge(55, 57, weight: 358)
    |> Graph.add_edge(21, 38, weight: 16458)
    |> Graph.add_edge(40, 41, weight: 5104)
    |> Graph.add_edge(3, 4, weight: 3476)
    |> Graph.add_edge(91, 22, weight: 4032)
    |> Graph.add_edge(91, 101, weight: 1970)
    |> Graph.add_edge(44, 64, weight: 1174)
    |> Graph.add_edge(44, 57, weight: 1129)
    |> Graph.add_edge(44, 43, weight: 255)
    |> Graph.add_edge(24, 22, weight: 1820)
    |> Graph.add_edge(24, 25, weight: 248)
    |> Graph.add_edge(27, 54, weight: 2095)
    |> Graph.add_edge(27, 29, weight: 1205)
    |> Graph.add_edge(57, 44, weight: 1129)
    |> Graph.add_edge(57, 50, weight: 1192)
    |> Graph.add_edge(57, 55, weight: 358)
    |> Graph.add_edge(57, 59, weight: 418)
    |> Graph.add_edge(92, 38, weight: 3589)
    |> Graph.add_edge(47, 48, weight: 204)
    |> Graph.add_edge(47, 59, weight: 1190)
    |> Graph.add_edge(47, 58, weight: 1175)
    |> Graph.add_edge(56, 50, weight: 1192)
    |> Graph.add_edge(56, 54, weight: 547)
    |> Graph.add_edge(56, 58, weight: 431)
    |> Graph.add_edge(90, 91, weight: 2301)
    |> Graph.add_edge(31, 18, weight: 861)
    |> Graph.add_edge(31, 27, weight: 1178)
    |> Graph.add_edge(97, 98, weight: 13465)
  end

  defp build_complex_graph_float(type \\ :directed) do
    build_complex_graph(type)
    |> Graph.edges()
    |> Enum.reduce(Graph.new(type: type), fn %Graph.Edge{weight: weight} = edge, acc ->
      acc
      |> Graph.add_edge(%Graph.Edge{edge | weight: weight / 1000})
    end)
  end
end
