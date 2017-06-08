defmodule Graph.Pathing do
  @moduledoc """
  This module contains implementation code for path finding algorithms used by `libgraph`.
  """
  import Graph.Directed, only: [find_out_edges: 2]
  import Graph.Utils, only: [find_vertex_id: 2, edge_weight: 3]

  @doc """
  Finds the shortest path between `a` and `b` as a list of vertices.
  Returns `nil` if no path can be found.

  The shortest path is calculated here by using a cost function to choose
  which path to explore next. The cost function in Dijkstra's algorithm is
  `weight(E(A, B))+lower_bound(E(A, B))` where `lower_bound(E(A, B))` is always 0.
  """
  def dijkstra(%Graph{} = g, a, b) do
    a_star(g, a, b, fn _v -> 0 end)
  end

  @doc """
  Finds the shortest path between `a` and `b` as a list of vertices.
  Returns `nil` if no path can be found.

  This implementation takes a heuristic function which allows you to
  calculate the lower bound cost of a given vertex `v`. The algorithm
  then uses that lower bound function to determine which path to explore
  next in the graph.

  The `dijkstra` function is simply `a_star` where the heuristic function
  always returns 0, and thus the next vertex is chosen based on the weight of
  the edge between it and the current vertex.
  """
  def a_star(%Graph{ids: ids} = g, a, b, hfun) when is_function(hfun, 1) do
    with {:ok, a_id}  <- find_vertex_id(g, a),
         {:ok, b_id}  <- find_vertex_id(g, b),
         {:ok, a_out} <- find_out_edges(g, a_id) do
      tree = Graph.new |> Graph.add_vertex(a_id)
      q = :queue.new()
      q =
        a_out
        |> Enum.sort_by(fn id -> cost(g, a_id, id, hfun) end)
        |> Enum.reduce(q, fn id, q -> :queue.in({a_id, id}, q) end)
      case do_shortpath(q, g, b_id, tree, hfun) do
        nil ->
          nil
        path ->
          for id <- path, do: Map.get(ids, id)
      end
    else
      _ -> nil
    end
  end

  @doc """
  Finds all paths between `a` and `b`, each path as a list of vertices.
  Returns `nil` if no path can be found.
  """
  def all(%Graph{ids: ids} = g, a, b) do
    with {:ok, a_id}  <- find_vertex_id(g, a),
         {:ok, b_id}  <- find_vertex_id(g, b),
         {:ok, a_out} <- find_out_edges(g, a_id) do
      case build_paths(g, a_out, b_id, [a_id], []) do
        nil ->
          []
        paths ->
          paths
          |> Enum.map(fn path -> Enum.map(path, &Map.get(ids, &1)) end)
      end
    else
      _ -> []
    end
  end

  ## Private

  defp cost(%Graph{ids: ids} = g, v1_id, v2_id, hfun) do
    edge_weight(g, v1_id, v2_id) + hfun.(Map.get(ids, v2_id))
  end

  defp do_shortpath(q, %Graph{out_edges: oe} = g, target_id, tree, hfun) do
    case :queue.out(q) do
      {{:value, {v_id, ^target_id}}, _q1} ->
        follow_path(v_id, tree, [target_id])
      {{:value, {v1_id, v2_id}}, q1} ->
        if Map.has_key?(tree.vertices, v2_id) do
          do_shortpath(q1, g, target_id, tree, hfun)
        else
          case Map.get(oe, v2_id) do
            nil ->
              do_shortpath(q1, g, target_id, tree, hfun)
            v2_out ->
              tree = tree |> Graph.add_vertex(v2_id) |> Graph.add_edge(v2_id, v1_id)
              q2 =
                v2_out
                |> Enum.sort_by(fn id -> cost(g, v2_id, id, hfun) end)
                |> Enum.reduce(q1, fn id, q -> :queue.in_r({v2_id, id}, q) end)
              do_shortpath(q2, g, target_id, tree, hfun)
          end
        end
      {:empty, _} ->
        nil
    end
  end

  defp follow_path(v_id, %Graph{vertices: vertices, ids: ids, out_edges: oe} = tree, path) do
    path = [v_id | path]
    v_id_tree = Map.get(vertices, v_id)
    case oe |> Map.get(v_id_tree, MapSet.new) |> MapSet.to_list do
      [] ->
        path
      [next_id] ->
        follow_path(Map.get(ids, next_id), tree, path)
    end
  end

  defp build_paths(%Graph{} = g, neighbors, target_id, path, acc) do
    if MapSet.member?(neighbors, target_id) do
      acc = [Enum.reverse([target_id|path]) | acc]
      neighbors = MapSet.difference(neighbors, MapSet.new(path))
      check_neighbors(g, MapSet.to_list(neighbors), target_id, path, acc)
    else
      neighbors = MapSet.difference(neighbors, MapSet.new(path))
      check_neighbors(g, MapSet.to_list(neighbors), target_id, path, acc)
    end
  end

  defp check_neighbors(_g, [], _target_id, _path, acc) do
    acc
  end
  defp check_neighbors(%Graph{out_edges: oe} = g, [next_neighbor_id|neighbors], target_id, path, acc) do
    case Map.get(oe, next_neighbor_id) do
      nil ->
        check_neighbors(g, neighbors, target_id, path, acc)
      next_neighbors ->
        case build_paths(g, next_neighbors, target_id, [next_neighbor_id | path], acc) do
          nil ->
            check_neighbors(g, neighbors, target_id, path, acc)
          paths ->
            check_neighbors(g, neighbors, target_id, path, paths)
        end
    end
  end
end
