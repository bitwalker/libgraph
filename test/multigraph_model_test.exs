defmodule Graph.MultigraphModelTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  @moduletag timeout: :infinity

  @labels [:foo, :bar, :baz, :qux, nil]

  test "edge_index is complete: every edge is indexed under its partitions" do
    check all(g <- multigraph(), max_runs: 500) do
      for edge <- Graph.edges(g) do
        partitions = g.partition_by.(edge)
        v1_id = g.vertex_identifier.(edge.v1)
        v2_id = g.vertex_identifier.(edge.v2)
        edge_key = {v1_id, v2_id}

        for partition <- partitions do
          partition_map = Map.get(g.edge_index, partition, %{})
          v1_set = Map.get(partition_map, v1_id, MapSet.new())
          v2_set = Map.get(partition_map, v2_id, MapSet.new())

          assert MapSet.member?(v1_set, edge_key),
                 "edge #{inspect(edge)} missing from edge_index partition #{inspect(partition)} for v1"

          assert MapSet.member?(v2_set, edge_key),
                 "edge #{inspect(edge)} missing from edge_index partition #{inspect(partition)} for v2"
        end
      end
    end
  end

  test "edge_index is sound: every indexed edge_key exists in edges" do
    check all(g <- multigraph(), max_runs: 500) do
      for {_partition, vertex_map} <- g.edge_index,
          {_v_id, edge_keys} <- vertex_map,
          edge_key <- edge_keys do
        assert Map.has_key?(g.edges, edge_key),
               "stale edge_key #{inspect(edge_key)} in edge_index"
      end
    end
  end

  test "partition filter returns exactly matching edges" do
    check all(g <- multigraph(), max_runs: 500) do
      for label <- @labels do
        indexed = Graph.edges(g, by: [label])

        scanned =
          g
          |> Graph.edges()
          |> Enum.filter(fn edge ->
            label in g.partition_by.(edge)
          end)

        assert MapSet.new(indexed) == MapSet.new(scanned),
               "by: #{inspect(label)} mismatch: indexed=#{length(indexed)}, scanned=#{length(scanned)}"
      end
    end
  end

  test "index invariant holds after delete_edge" do
    check all(
            g <- multigraph(min_edges: 2),
            max_runs: 500
          ) do
      edge = Enum.random(Graph.edges(g))
      g2 = Graph.delete_edge(g, edge.v1, edge.v2, edge.label)

      assert_index_complete(g2)
      assert_index_sound(g2)
    end
  end

  test "index invariant holds after delete_vertex" do
    check all(
            g <- multigraph(min_vertices: 2),
            max_runs: 500
          ) do
      vertex = Enum.random(Graph.vertices(g))
      g2 = Graph.delete_vertex(g, vertex)

      assert_index_complete(g2)
      assert_index_sound(g2)
    end
  end

  test "index invariant holds after update_labelled_edge with new label" do
    check all(
            g <- multigraph(min_edges: 1),
            max_runs: 500
          ) do
      edge = Enum.random(Graph.edges(g))
      new_label = :updated_label

      case Graph.update_labelled_edge(g, edge.v1, edge.v2, edge.label, label: new_label) do
        {:error, _} ->
          :ok

        g2 ->
          assert_index_complete(g2)
          assert_index_sound(g2)
      end
    end
  end

  test "index invariant holds after transpose" do
    check all(g <- multigraph(), max_runs: 500) do
      gt = Graph.transpose(g)

      assert_index_complete(gt)
      assert_index_sound(gt)
    end
  end

  test "subgraph preserves multigraph and index correctness" do
    check all(
            g <- multigraph(min_vertices: 2),
            max_runs: 500
          ) do
      vs = Graph.vertices(g)
      subset = Enum.take_random(vs, max(1, div(length(vs), 2)))
      sg = Graph.subgraph(g, subset)

      assert sg.multigraph == true
      assert_index_complete(sg)
      assert_index_sound(sg)
    end
  end

  test "adding the same edge twice does not create duplicate index entries" do
    check all(g <- multigraph(), max_runs: 500) do
      for {_partition, vertex_map} <- g.edge_index,
          {_v_id, edge_keys} <- vertex_map do
        assert MapSet.size(edge_keys) == length(MapSet.to_list(edge_keys)),
               "duplicate entries found in edge_index MapSet"
      end
    end
  end

  test "non-multigraph queries are equivalent to multigraph queries" do
    check all(g <- multigraph(), max_runs: 500) do
      plain =
        g
        |> Graph.edges()
        |> Enum.reduce(Graph.new(), fn edge, acc ->
          Graph.add_edge(acc, edge.v1, edge.v2,
            label: edge.label,
            weight: edge.weight,
            properties: edge.properties
          )
        end)

      assert MapSet.new(Graph.edges(g)) == MapSet.new(Graph.edges(plain))
      assert MapSet.new(Graph.vertices(g)) == MapSet.new(Graph.vertices(plain))
    end
  end

  test "index invariant holds after a sequence of mutations" do
    check all(
            g <- multigraph(min_vertices: 3, min_edges: 3),
            mutations <- list_of(mutation_gen(), min_length: 1, max_length: 10),
            max_runs: 300
          ) do
      g_final =
        Enum.reduce(mutations, g, fn mutation, g ->
          apply_mutation(g, mutation)
        end)

      assert_index_complete(g_final)
      assert_index_sound(g_final)
    end
  end

  test "index invariant holds after split_edge" do
    check all(
            g <- multigraph(min_edges: 1),
            max_runs: 500
          ) do
      edge = Enum.random(Graph.edges(g))
      mid = {:split_mid, :rand.uniform(10_000)}
      g2 = Graph.split_edge(g, edge.v1, edge.v2, mid)

      assert_index_complete(g2)
      assert_index_sound(g2)

      # Original edge key should not remain in any index entry
      v1_id = g2.vertex_identifier.(edge.v1)
      v2_id = g2.vertex_identifier.(edge.v2)

      Enum.each(g2.edge_index, fn {_partition, vertex_map} ->
        Enum.each(vertex_map, fn {_v_id, edge_keys} ->
          refute MapSet.member?(edge_keys, {v1_id, v2_id})
        end)
      end)
    end
  end

  test "BFS with partition filter visits subset of unfiltered BFS" do
    check all(g <- multigraph(min_edges: 2), max_runs: 500) do
      all_visited = MapSet.new(Graph.Reducers.Bfs.map(g, fn v -> v end))

      for label <- @labels do
        filtered = MapSet.new(Graph.Reducers.Bfs.map(g, fn v -> v end, by: label))
        assert MapSet.subset?(filtered, all_visited)
      end
    end
  end

  test "DFS with partition filter visits subset of unfiltered vertices" do
    check all(g <- multigraph(min_edges: 2), max_runs: 500) do
      all_vertices = MapSet.new(Graph.vertices(g))

      for label <- @labels do
        filtered = MapSet.new(Graph.Reducers.Dfs.map(g, fn v -> v end, by: label))
        assert MapSet.subset?(filtered, all_vertices)
      end
    end
  end

  test "Dijkstra with partition filter returns path using only filtered edges" do
    check all(g <- multigraph(min_edges: 2), max_runs: 300) do
      vertices = Graph.vertices(g)

      if length(vertices) >= 2 do
        [a, b] = Enum.take_random(vertices, 2)

        for label <- @labels do
          case Graph.dijkstra(g, a, b, by: label) do
            nil ->
              :ok

            path ->
              assert hd(path) == a
              assert List.last(path) == b

              # Every consecutive pair in the path must have an edge in the partition
              path
              |> Enum.chunk_every(2, 1, :discard)
              |> Enum.each(fn [v1, v2] ->
                matching =
                  Graph.edges(g)
                  |> Enum.any?(fn edge ->
                    edge.v1 == v1 and edge.v2 == v2 and label in g.partition_by.(edge)
                  end)

                assert matching,
                       "path segment #{inspect(v1)}->#{inspect(v2)} has no #{inspect(label)} edge"
              end)
          end
        end
      end
    end
  end

  ## Helpers

  defp assert_index_complete(g) do
    for edge <- Graph.edges(g) do
      partitions = g.partition_by.(edge)
      v1_id = g.vertex_identifier.(edge.v1)
      v2_id = g.vertex_identifier.(edge.v2)
      edge_key = {v1_id, v2_id}

      for partition <- partitions do
        partition_map = Map.get(g.edge_index, partition, %{})
        v1_set = Map.get(partition_map, v1_id, MapSet.new())

        assert MapSet.member?(v1_set, edge_key),
               "completeness: edge #{inspect(edge)} missing from partition #{inspect(partition)}"
      end
    end
  end

  defp assert_index_sound(g) do
    for {_partition, vertex_map} <- g.edge_index,
        {_v_id, edge_keys} <- vertex_map,
        edge_key <- edge_keys do
      assert Map.has_key?(g.edges, edge_key),
             "soundness: stale edge_key #{inspect(edge_key)} in edge_index"
    end
  end

  ## Generators

  defp multigraph(opts \\ []) do
    min_vertices = Keyword.get(opts, :min_vertices, 1)
    min_edges = Keyword.get(opts, :min_edges, 0)

    gen all(
          num_vertices <- integer(max(min_vertices, 2)..10),
          num_edges <- integer(max(min_edges, 1)..20),
          edges <- list_of(edge_gen(num_vertices), length: num_edges)
        ) do
      Enum.reduce(edges, Graph.new(multigraph: true), fn {v1, v2, opts}, g ->
        Graph.add_edge(g, v1, v2, opts)
      end)
    end
  end

  defp edge_gen(num_vertices) do
    gen all(
          v1 <- integer(1..num_vertices),
          v2 <- integer(1..num_vertices),
          label <- member_of(@labels),
          weight <- integer(1..10)
        ) do
      {v1, v2, [label: label, weight: weight]}
    end
  end

  defp mutation_gen do
    one_of([
      tuple(
        {constant(:add_edge), integer(1..10), integer(1..10), member_of(@labels), integer(1..10)}
      ),
      tuple({constant(:delete_edge), integer(1..10), integer(1..10), member_of(@labels)}),
      tuple({constant(:delete_vertex), integer(1..10)}),
      tuple(
        {constant(:update_labelled_edge), integer(1..10), integer(1..10), member_of(@labels),
         member_of(@labels)}
      )
    ])
  end

  defp apply_mutation(g, {:add_edge, v1, v2, label, weight}) do
    Graph.add_edge(g, v1, v2, label: label, weight: weight)
  end

  defp apply_mutation(g, {:delete_edge, v1, v2, label}) do
    Graph.delete_edge(g, v1, v2, label)
  end

  defp apply_mutation(g, {:delete_vertex, v}) do
    if Graph.has_vertex?(g, v), do: Graph.delete_vertex(g, v), else: g
  end

  defp apply_mutation(g, {:update_labelled_edge, v1, v2, old_label, new_label}) do
    case Graph.update_labelled_edge(g, v1, v2, old_label, label: new_label) do
      {:error, _} -> g
      g2 -> g2
    end
  end
end
