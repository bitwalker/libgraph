defmodule Graph.Reducers.Bfs do
  @moduledoc """
  This reducer traverses the graph using Breadth-First Search.
  """
  use Graph.Reducer

  @doc """
  Performs a breadth-first traversal of the graph, applying the provided mapping function to
  each new vertex encountered.

  NOTE: The algorithm will follow lower-weighted edges first.

  Returns a list of values returned from the mapper in the order they were encountered.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 3}])
      ...> #{__MODULE__}.map(g, fn v -> v end)
      [1, 3, 4, 2]
  """
  def map(g, fun) when is_function(fun, 1) do
    map(g, fun, [])
  end

  def map(g, fun, opts) when is_function(fun, 1) and is_list(opts) do
    g
    |> reduce([], fn v, results -> {:next, [fun.(v) | results]} end, opts)
    |> Enum.reverse()
  end

  @doc """
  Performs a breadth-first traversal of the graph, applying the provided reducer function to
  each new vertex encountered and the accumulator.

  NOTE: The algorithm will follow lower-weighted edges first.

  The result will be the state of the accumulator after the last reduction.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 3}])
      ...> #{__MODULE__}.reduce(g, [], fn v, acc -> {:next, [v|acc]} end)
      [2, 4, 3, 1]

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 3}, {4, 5}])
      ...> #{__MODULE__}.reduce(g, [], fn 5, acc -> {:skip, acc}; v, acc -> {:next, [v|acc]} end)
      [2, 4, 3, 1]

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 3}, {4, 5}])
      ...> #{__MODULE__}.reduce(g, [], fn 4, acc -> {:halt, acc}; v, acc -> {:next, [v|acc]} end)
      [3, 1]
  """
  def reduce(%Graph{} = g, acc, fun) when is_function(fun, 2) do
    reduce(g, acc, fun, [])
  end

  def reduce(%Graph{vertices: vs} = g, acc, fun, opts)
      when is_function(fun, 2) and is_list(opts) do
    partitions = partitions_from_opts(opts)

    vs
    # Start with a cost of zero
    |> Stream.map(fn {id, _} -> {id, 0} end)
    # Only populate the initial queue with those vertices which have no inbound edges
    |> Stream.reject(fn {id, _cost} -> inbound_edges?(g, id, partitions) end)
    |> Enum.reduce(PriorityQueue.new(), fn {id, cost}, q ->
      PriorityQueue.push(q, id, cost)
    end)
    |> traverse(g, MapSet.new(), fun, acc, partitions)
  end

  defp partitions_from_opts(opts) do
    case Keyword.fetch(opts, :by) do
      {:ok, by} when is_list(by) -> by
      {:ok, by} -> [by]
      :error -> nil
    end
  end

  defp inbound_edges?(%Graph{in_edges: ie}, v_id, _partitions) do
    case Map.get(ie, v_id) do
      nil -> false
      edges -> MapSet.size(edges) > 0
    end
  end

  defp out_neighbors(%Graph{out_edges: oe}, v_id, nil) do
    Map.get(oe, v_id, MapSet.new())
  end

  defp out_neighbors(%Graph{out_edges: oe, edge_index: edge_index}, v_id, partitions) do
    out_set = Map.get(oe, v_id, MapSet.new())

    partitions
    |> Enum.reduce(MapSet.new(), fn partition, acc ->
      edge_index
      |> Map.get(partition, %{})
      |> Map.get(v_id, MapSet.new())
      |> Enum.reduce(acc, fn
        {^v_id, v2_id}, acc -> MapSet.put(acc, v2_id)
        _, acc -> acc
      end)
    end)
    |> MapSet.intersection(out_set)
  end

  defp edge_weight_for(g, v_id, id, nil) do
    Graph.Utils.edge_weight(g, v_id, id)
  end

  defp edge_weight_for(g, v_id, id, partitions) do
    Graph.Utils.edge_weight(g, v_id, id, partitions)
  end

  defp traverse(q, %Graph{vertices: vertices} = g, visited, fun, acc, partitions) do
    case PriorityQueue.pop(q) do
      {{:value, v_id}, q1} ->
        if MapSet.member?(visited, v_id) do
          traverse(q1, g, visited, fun, acc, partitions)
        else
          v = Map.get(vertices, v_id)

          case fun.(v, acc) do
            {:next, acc2} ->
              visited = MapSet.put(visited, v_id)
              v_out = out_neighbors(g, v_id, partitions)

              q2 =
                v_out
                |> MapSet.to_list()
                |> Enum.reduce(q1, fn id, q ->
                  weight = edge_weight_for(g, v_id, id, partitions)
                  PriorityQueue.push(q, id, weight)
                end)

              traverse(q2, g, visited, fun, acc2, partitions)

            {:skip, acc2} ->
              visited = MapSet.put(visited, v_id)
              traverse(q1, g, visited, fun, acc2, partitions)

            {:halt, acc2} ->
              acc2
          end
        end

      {:empty, _} ->
        acc
    end
  end
end
