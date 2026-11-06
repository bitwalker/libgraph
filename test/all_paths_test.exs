defmodule Graph.AllPathsTest do
  use ExUnit.Case, async: true
  import Bitwise, only: [band: 2, bsl: 2]

  test "work on a single-path chain does not grow quadratically with depth" do
    small = chain_reductions(600)
    large = chain_reductions(1200)
    assert large < 3 * small
  end

  test "all-path traversal agrees with a simple path oracle on small graphs" do
    pairs = for from <- 1..3, to <- 1..3, do: {from, to}

    for mask <- 0..511, type <- [:directed, :undirected] do
      edges =
        pairs
        |> Enum.with_index()
        |> Enum.filter(fn {_, index} -> band(mask, bsl(1, index)) != 0 end)
        |> Enum.map(&elem(&1, 0))

      g =
        Graph.new(type: type, vertex_identifier: fn v -> v end)
        |> Graph.add_vertices([1, 2, 3])
        |> Graph.add_edges(edges)

      for from <- 1..4, to <- 1..4 do
        actual = Graph.get_paths(g, from, to) |> MapSet.new()
        assert actual === MapSet.new(oracle(g, from, to, [from]))
      end
    end
  end

  test "all-path traversal preserves vertex callbacks and strict numeric identities" do
    a = {1, :a}
    b = {1.0, :b}
    c = {nil, :c}

    g =
      Graph.new(vertex_identifier: fn {id, _} -> id end)
      |> Graph.add_edges([{a, b}, {b, a}, {b, c}, {a, c}, {a, b, label: :parallel}])

    assert MapSet.new(Graph.get_paths(g, a, c)) === MapSet.new([[a, c], [a, b, c]])
    assert Graph.get_paths(g, a, a) === [[a, b, a]]
  end

  defp chain_reductions(size) do
    g =
      Graph.new(vertex_identifier: fn v -> v end)
      |> Graph.add_edges(Enum.map(1..(size - 1), &{&1, &1 + 1}))

    task =
      Task.async(fn ->
        {:reductions, before} = Process.info(self(), :reductions)
        result = Graph.get_paths(g, 1, size)
        {:reductions, after_search} = Process.info(self(), :reductions)
        assert result == [Enum.to_list(1..size)]
        after_search - before
      end)

    Task.await(task, 30_000)
  end

  # Read the stored outgoing relation directly: this optimization preserves the
  # existing get_paths behavior for both graph types, including return-to-source
  # cycles. This independent oracle explores by list membership, without sets.
  defp oracle(g, from, target, path) do
    g.out_edges
    |> Map.get(from, MapSet.new())
    |> Enum.flat_map(fn neighbor ->
      cond do
        neighbor === target -> [Enum.reverse([neighbor | path])]
        neighbor in path -> []
        true -> oracle(g, neighbor, target, [neighbor | path])
      end
    end)
  end
end
