defmodule Graph.IndexContractTest do
  use ExUnit.Case, async: true

  alias Graph, as: Graph
  alias Graph.Edge

  defp partitions(edge) do
    [edge.label, :all, {:weight, edge.weight}, {:kind, Map.get(edge.properties, :kind)}]
  end

  defp graph(type) do
    Graph.new(multigraph: true, type: type, partition_by: &partitions/1)
  end

  # Independent oracle: read the canonical edge records, not an adjacency index.
  # Every assertion checks complete structs, including orientation and properties.
  defp assert_index_contract(g) do
    edges = Graph.edges(g)
    partitions = Enum.flat_map(edges, &g.partition_by.(&1)) |> Enum.uniq()
    vertices = Graph.vertices(g)

    expected_index =
      Enum.reduce(edges, %{}, fn edge, index ->
        from = g.vertex_identifier.(edge.v1)
        to = g.vertex_identifier.(edge.v2)

        Enum.reduce(g.partition_by.(edge), index, fn partition, index ->
          Enum.reduce(Enum.uniq([from, to]), index, fn endpoint, index ->
            Map.update(index, partition, %{endpoint => MapSet.new([{from, to}])}, fn owners ->
              Map.update(owners, endpoint, MapSet.new([{from, to}]), &MapSet.put(&1, {from, to}))
            end)
          end)
        end)
      end)

    assert g.edge_index == expected_index

    for partition <- [:missing | partitions] do
      selected = Enum.filter(edges, &(partition in g.partition_by.(&1)))
      assert MapSet.new(Graph.edges(g, by: partition)) == MapSet.new(selected)

      for vertex <- vertices do
        incident = Enum.filter(selected, &(&1.v1 == vertex or &1.v2 == vertex))

        incoming =
          if g.type == :undirected, do: incident, else: Enum.filter(selected, &(&1.v2 == vertex))

        outgoing =
          if g.type == :undirected, do: incident, else: Enum.filter(selected, &(&1.v1 == vertex))

        assert MapSet.new(Graph.edges(g, vertex, by: partition)) == MapSet.new(incident)
        assert MapSet.new(Graph.in_edges(g, vertex, by: partition)) == MapSet.new(incoming)
        assert MapSet.new(Graph.out_edges(g, vertex, by: partition)) == MapSet.new(outgoing)
      end
    end

    g
  end

  for type <- [:directed, :undirected] do
    @graph_type type

    test "#{type}: overlapping partitions, opposite orientations, self loops, and predicates" do
      g =
        graph(@graph_type)
        |> Graph.add_edge(:a, :b, label: :flow, weight: 3, properties: %{kind: :definition})
        |> Graph.add_edge(:a, :b, label: :produced, properties: %{kind: :runtime})
        |> Graph.add_edge(:c, :b, label: :flow)
        |> Graph.add_edge(:b, :a, label: :reverse)
        |> Graph.add_edge(:b, :b, label: :flow, weight: 2)
        |> assert_index_contract()

      result = Graph.out_edges(g, :b, by: [:all, :flow, :all], where: &(&1.weight >= 2))
      assert length(result) == length(Enum.uniq(result))
      expected = Graph.out_edges(g, :b, by: :all) |> Enum.filter(&(&1.weight >= 2))
      assert MapSet.new(result) == MapSet.new(expected)
      assert Graph.out_edges(g, :b, by: []) == []
    end

    test "#{type}: relabel preserves another label's shared partition and properties" do
      g =
        graph(@graph_type)
        |> Graph.add_edge(:a, :b, label: :runnable, properties: %{kind: :active})
        |> Graph.add_edge(:a, :b, label: :flow, properties: %{kind: :definition})
        |> Graph.add_edge(:x, :y, label: :flow)
        |> Graph.update_labelled_edge(:a, :b, :runnable,
          label: :ran,
          properties: %{kind: :history}
        )
        |> assert_index_contract()

      assert [%Edge{label: :flow, properties: %{kind: :definition}}] =
               Graph.out_edges(g, :a, by: {:kind, :definition})

      assert Graph.out_edges(g, :a, by: {:kind, :active}) == []
      assert length(Graph.out_edges(g, :a, by: :all)) == 2
    end

    test "#{type}: relabel replacing an existing label removes its old custom partitions" do
      graph(@graph_type)
      |> Graph.add_edge(:a, :b, label: :first, properties: %{kind: :first})
      |> Graph.add_edge(:a, :b, label: :second, properties: %{kind: :second})
      |> Graph.update_labelled_edge(:a, :b, :first,
        label: :second,
        properties: %{kind: :replacement}
      )
      |> assert_index_contract()
    end

    test "#{type}: same-label updates and add overwrite refresh custom partitions" do
      graph(@graph_type)
      |> Graph.add_edge(:a, :b, label: :flow, weight: 1, properties: %{kind: :old})
      |> Graph.update_labelled_edge(:a, :b, :flow, weight: 4, properties: %{kind: :new})
      |> assert_index_contract()
      |> Graph.update_labelled_edge(:a, :b, :flow, label: :flow, weight: 8)
      |> assert_index_contract()
      |> Graph.add_edge(:a, :b, label: :flow, weight: 2, properties: %{kind: :newer})
      |> assert_index_contract()
      |> Graph.add_edge(:a, :b, label: :flow, weight: 3)
      |> assert_index_contract()
    end

    test "#{type}: deleting one parallel label retains the others and deletes empty indexes" do
      g =
        graph(@graph_type)
        |> Graph.add_edge(:a, :b, label: :one, properties: %{kind: :common})
        |> Graph.add_edge(:a, :b, label: :two, properties: %{kind: :common})
        |> Graph.delete_edge(:a, :b, :one)
        |> assert_index_contract()

      assert [%Edge{label: :two}] = Graph.in_edges(g, :b, by: {:kind, :common})
      assert Graph.delete_edge(g, :a, :b, :absent) == g
      g = g |> Graph.delete_edge(:a, :b, :two) |> assert_index_contract()
      assert g.edge_index == %{}
      assert g.edge_properties == %{}
    end

    test "#{type}: deleting all labels, reinsertion, and deleting vertices clean properties" do
      g =
        graph(@graph_type)
        |> Graph.add_edge(:a, :b, label: :one, properties: %{kind: :old})
        |> Graph.add_edge(:a, :b, label: :two, properties: %{kind: :old})
        |> Graph.delete_edge(:a, :b)
        |> assert_index_contract()

      assert g.edge_properties == %{}
      g = g |> Graph.add_edge(:a, :b, label: :one) |> assert_index_contract()
      assert [%Edge{properties: %{}}] = Graph.edges(g)

      g =
        g
        |> Graph.add_edge(:a, :a, label: :self, properties: %{kind: :old})
        |> Graph.add_edge(:c, :a, label: :back, properties: %{kind: :old})
        |> Graph.delete_vertex(:a)
        |> assert_index_contract()

      assert g.edge_index == %{}
      assert g.edge_properties == %{}
    end

    test "#{type}: removing and relabeling self loops updates the endpoint only once" do
      graph(@graph_type)
      |> Graph.add_edge(:a, :a, label: :one)
      |> Graph.add_edge(:a, :a, label: :two)
      |> Graph.add_edge(:a, :b, label: :one)
      |> Graph.update_labelled_edge(:a, :a, :one, label: :three)
      |> assert_index_contract()
      |> Graph.delete_edge(:a, :a, :two)
      |> assert_index_contract()
      |> Graph.delete_edge(:a, :a)
      |> assert_index_contract()
    end

    test "#{type}: fixed-seed mutation traces preserve index equivalence after every operation" do
      :rand.seed(:exsss, {101, 203, 307})

      Enum.reduce(1..180, graph(@graph_type), fn _, g ->
        from = Enum.at([:a, :b, :c, :d], :rand.uniform(4) - 1)
        to = Enum.at([:a, :b, :c, :d], :rand.uniform(4) - 1)
        label = Enum.at([:flow, :runnable, :ran], :rand.uniform(3) - 1)

        opts = [
          label: label,
          weight: :rand.uniform(4),
          properties: %{kind: rem(:rand.uniform(10), 3)}
        ]

        g =
          case :rand.uniform(6) do
            1 ->
              Graph.delete_edge(g, from, to, label)

            2 ->
              Graph.delete_edge(g, from, to)

            3 ->
              Graph.delete_vertex(g, from)

            4 ->
              case Graph.update_labelled_edge(g, from, to, :runnable, opts) do
                {:error, :no_such_edge} -> g
                updated -> updated
              end

            _ ->
              Graph.add_edge(g, from, to, opts)
          end

        assert_index_contract(g)
      end)
    end
  end

  test "partition query preserves structured vertices and custom vertex identifiers" do
    Graph.new(multigraph: true, vertex_identifier: & &1.id, partition_by: &partitions/1)
    |> Graph.add_edge(%{id: {:node, 1}}, %{id: {:node, 2}}, label: :flow)
    |> Graph.add_edge(%{id: {:node, 3}}, %{id: {:node, 2}}, label: :ran)
    |> assert_index_contract()
  end

  test "partition queries do not call partition function for unrelated historical neighbors" do
    partition = fn edge ->
      if edge.label == :history, do: send(self(), :visited_history)
      [edge.label]
    end

    g =
      Enum.reduce(1..500, Graph.new(multigraph: true, partition_by: partition), fn n, g ->
        Graph.add_edge(g, {:fact, n}, :reducer, label: :history)
      end)
      |> Graph.add_edge(:mapper, :reducer, label: :fan_in)

    drain_history_messages()
    assert [%Edge{v1: :mapper}] = Graph.in_edges(g, :reducer, by: :fan_in)
    refute_received :visited_history
  end

  test "same-identity replacement preserves indexed queries with current vertex values" do
    g =
      Graph.new(multigraph: true, vertex_identifier: & &1.id)
      |> Graph.add_edge(%{id: 1, value: :old}, %{id: 2, value: :target}, label: :flow)
      |> Graph.replace_vertex(%{id: 1}, %{id: 1, value: :new})
      |> assert_index_contract()

    assert [%Edge{v1: %{value: :new}}] = Graph.in_edges(g, %{id: 2}, by: :flow)
  end

  test "stale changed-identity replacement candidates do not introduce query crashes" do
    # Upstream replace_vertex does not migrate its edge index for changed IDs.
    # Preserve its incomplete-query behavior without dereferencing removed IDs.
    g =
      graph(:directed)
      |> Graph.add_edge(:a, :b, label: :flow)
      |> Graph.replace_vertex(:a, :replacement)

    assert [%Edge{v1: :replacement, v2: :b}] = Graph.edges(g)
    assert Graph.in_edges(g, :b, by: :flow) == []
    assert Graph.out_edges(g, :a, by: :flow) == []
    assert Graph.edges(g, :b, by: :flow) == []
  end

  defp drain_history_messages do
    receive do
      :visited_history -> drain_history_messages()
    after
      0 -> :ok
    end
  end
end
