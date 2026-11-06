Code.require_file("fixtures/topsort_consumer.ex", __DIR__)

defmodule Graph.TopsortContractTest do
  use ExUnit.Case, async: true
  alias Graph.Test.TopsortConsumer

  test "empty directed sorts return empty lists to typed callers" do
    g = Graph.new()
    assert Graph.topsort(g) == []
    assert Graph.batch_topsort(g) == []
    assert TopsortConsumer.ordered(g) == {:ok, []}
    assert TopsortConsumer.batch_sizes(g) == []
  end

  test "DAG batches group independent vertices while retaining dependencies" do
    g = Graph.new() |> Graph.add_edges([{:a, :b}, {:a, :c}, {:a, :b, label: :parallel}])
    assert {:ok, ordered} = TopsortConsumer.ordered(g)
    assert hd(ordered) == :a
    assert MapSet.new(ordered) == MapSet.new([:a, :b, :c])
    assert TopsortConsumer.batch_sizes(g) == [1, 2]

    assert Enum.map(Graph.batch_topsort(g), &MapSet.new/1) == [
             MapSet.new([:a]),
             MapSet.new([:b, :c])
           ]
  end

  test "cycles return false and typed callers handle that branch" do
    g = Graph.new() |> Graph.add_edges([{:a, :b}, {:b, :a}])
    assert Graph.topsort(g) == false
    assert Graph.batch_topsort(g) == false
    assert TopsortConsumer.ordered(g) == :cyclic
    assert TopsortConsumer.batch_sizes(g) == :cyclic
  end

  test "undirected inputs return false including the empty graph" do
    for g <- [
          Graph.new(type: :undirected),
          Graph.new(type: :undirected) |> Graph.add_edge(:a, :b)
        ] do
      assert Graph.topsort(g) == false
      assert Graph.batch_topsort(g) == false
    end
  end

  test "legacy self-loop handling remains explicit" do
    g = Graph.new() |> Graph.add_edge(:a, :a)
    assert Graph.topsort(g) == [:a]
    assert Graph.batch_topsort(g) == false
    refute Graph.is_acyclic?(g)
  end
end
