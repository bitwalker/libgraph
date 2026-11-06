defmodule Graph.Pathfindings.BellmanFord do
  @moduledoc """
  The Bellman–Ford algorithm is an algorithm that computes shortest paths from a single
  source vertex to all of the other vertices in a weighted digraph.
  It is capable of handling graphs in which some of the edge weights are negative numbers
  Time complexity: O(VE)
  """

  @typep distance() :: %{Graph.vertex_id() => number() | :infinity}

  @doc """
  Returns nil when graph has negative cycle.
  """
  @spec call(Graph.t(), Graph.vertex()) ::
          %{Graph.vertex() => number() | :infinity} | nil
  def call(%Graph{} = g, a), do: do_call(g, a, nil)

  def call(%Graph{} = g, a, opts) when is_list(opts) do
    partitions = partitions_from_opts(opts)
    do_call(g, a, partitions)
  end

  defp partitions_from_opts(opts) do
    case Keyword.fetch(opts, :by) do
      {:ok, by} when is_list(by) -> by
      {:ok, by} -> [by]
      :error -> nil
    end
  end

  defp do_call(%Graph{vertices: vs, edges: meta} = g, a, partitions) do
    distances = a |> g.vertex_identifier.() |> init_distances(vs)

    weights = edges_with_weights(meta, g, partitions)

    distances =
      for _ <- 1..map_size(vs),
          edge <- weights,
          reduce: distances do
        acc -> update_distance(edge, acc)
      end

    if has_negative_cycle?(distances, weights) do
      nil
    else
      Map.new(distances, fn {k, v} -> {Map.fetch!(g.vertices, k), v} end)
    end
  end

  defp edges_with_weights(meta, _g, nil) do
    Enum.map(meta, &edge_weight/1)
  end

  defp edges_with_weights(
         meta,
         %Graph{partition_by: partition_by, edge_properties: ep},
         partitions
       ) do
    Enum.flat_map(meta, fn {edge_key, edge_value} ->
      edge_value
      |> Enum.filter(fn {label, weight} ->
        props =
          case ep do
            %{^edge_key => %{^label => p}} -> p
            _ -> %{}
          end

        eps = partition_by.(%{label: label, weight: weight, properties: props})
        Enum.any?(eps, fn ep -> ep in partitions end)
      end)
      |> Enum.map(fn {_label, weight} ->
        {edge_key, weight}
      end)
    end)
  end

  @spec init_distances(Graph.vertex(), Graph.vertices()) :: distance
  defp init_distances(vertex_id, vertices) do
    Map.new(vertices, fn
      {id, _vertex} when id === vertex_id -> {id, 0}
      {id, _} -> {id, :infinity}
    end)
  end

  @spec update_distance(term, distance) :: distance
  defp update_distance({{u, v}, weight}, distances) do
    %{^u => du, ^v => dv} = distances

    if du != :infinity and du + weight < dv do
      %{distances | v => du + weight}
    else
      distances
    end
  end

  @spec edge_weight(term) :: {Graph.edge_key(), number()}
  defp edge_weight({e, edge_value}), do: {e, edge_value |> Map.values() |> Enum.min()}

  defp has_negative_cycle?(distances, meta) do
    Enum.any?(meta, fn {{u, v}, weight} ->
      %{^u => du, ^v => dv} = distances

      du != :infinity and du + weight < dv
    end)
  end
end
