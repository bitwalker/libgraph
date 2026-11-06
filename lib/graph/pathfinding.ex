defmodule Graph.Pathfinding do
  @moduledoc """
  This module contains implementation code for path finding algorithms used by `libgraph`.
  """
  import Graph.Utils, only: [edge_weight: 3, edge_weight: 4]

  @type heuristic_fun :: (Graph.vertex() -> integer)

  @spec bellman_ford(Graph.t(), Graph.vertex()) ::
          %{Graph.vertex() => integer() | :infinity} | nil
  def bellman_ford(g, a), do: Graph.Pathfindings.BellmanFord.call(g, a)

  def bellman_ford(g, a, opts) when is_list(opts),
    do: Graph.Pathfindings.BellmanFord.call(g, a, opts)

  @doc """
  Finds the shortest path between `a` and `b` as a list of vertices.
  Returns `nil` if no path can be found.

  The shortest path is calculated here by using a cost function to choose
  which path to explore next. The cost function in Dijkstra's algorithm is
  `weight(E(A, B))+lower_bound(E(A, B))` where `lower_bound(E(A, B))` is always 0.
  """
  @spec dijkstra(Graph.t(), Graph.vertex(), Graph.vertex()) ::
          [Graph.vertex()] | nil
  def dijkstra(%Graph{} = g, a, b) do
    a_star(g, a, b, fn _v -> 0 end)
  end

  def dijkstra(%Graph{} = g, a, b, opts) when is_list(opts) do
    a_star(g, a, b, fn _v -> 0 end, opts)
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
  @spec a_star(Graph.t(), Graph.vertex(), Graph.vertex(), heuristic_fun) ::
          [Graph.vertex()] | nil
  def a_star(%Graph{} = g, a, b, hfun) when is_function(hfun, 1) do
    do_a_star(g, a, b, hfun, nil)
  end

  def a_star(%Graph{} = g, a, b, hfun, opts) when is_function(hfun, 1) and is_list(opts) do
    partitions = partitions_from_opts(opts)
    do_a_star(g, a, b, hfun, partitions)
  end

  @doc """
  Finds all paths between `a` and `b`, each path as a list of vertices.
  Returns `[]` if no path can be found. Results are eager and their number can
  grow exponentially; see `Graph.get_paths/3` for usage and complexity guidance.
  """
  @spec all(Graph.t(), Graph.vertex(), Graph.vertex()) :: [[Graph.vertex()]]
  def all(%Graph{vertices: vs, out_edges: oe, vertex_identifier: vertex_identifier} = g, a, b) do
    with a_id <- vertex_identifier.(a),
         b_id <- vertex_identifier.(b),
         {:ok, a_out} <- Map.fetch(oe, a_id) do
      case dfs(g, a_out, b_id, [a_id], MapSet.new([a_id]), []) do
        [] ->
          []

        paths ->
          paths
          |> Enum.map(fn path -> Enum.map(path, &Map.get(vs, &1)) end)
      end
    else
      _ -> []
    end
  end

  ## Private

  defp partitions_from_opts(opts) do
    case Keyword.fetch(opts, :by) do
      {:ok, by} when is_list(by) -> by
      {:ok, by} -> [by]
      :error -> nil
    end
  end

  defp do_a_star(
         %Graph{type: :directed, vertices: vs, vertex_identifier: vertex_identifier} = g,
         a,
         b,
         hfun,
         partitions
       ) do
    a_id = vertex_identifier.(a)
    b_id = vertex_identifier.(b)
    a_out = get_out_neighbors(g, a_id, partitions)

    if a_out do
      tree = %{a_id => a_id}
      q = PriorityQueue.new()

      q =
        a_out
        |> Stream.map(fn id -> {id, cost(g, a_id, id, hfun, partitions)} end)
        |> Enum.reduce(q, fn {id, cost}, q ->
          PriorityQueue.push(
            q,
            {a_id, id, do_edge_weight(g, a_id, id, partitions)},
            cost
          )
        end)

      case do_bfs(q, g, b_id, tree, hfun, partitions) do
        nil ->
          nil

        path when is_list(path) ->
          for id <- path, do: Map.get(vs, id)
      end
    else
      nil
    end
  end

  defp do_a_star(
         %Graph{type: :undirected, vertices: vs, vertex_identifier: vertex_identifier} = g,
         a,
         b,
         hfun,
         partitions
       ) do
    a_id = vertex_identifier.(a)
    b_id = vertex_identifier.(b)
    a_neighbors = get_all_neighbors(g, a_id, partitions)
    tree = %{a_id => a_id}
    q = PriorityQueue.new()

    q =
      a_neighbors
      |> Stream.map(fn id -> {id, cost(g, a_id, id, hfun, partitions)} end)
      |> Enum.reduce(q, fn {id, cost}, q ->
        PriorityQueue.push(
          q,
          {a_id, id, do_edge_weight(g, a_id, id, partitions)},
          cost
        )
      end)

    case do_bfs(q, g, b_id, tree, hfun, partitions) do
      nil ->
        nil

      path when is_list(path) ->
        for id <- path, do: Map.get(vs, id)
    end
  end

  defp get_out_neighbors(%Graph{out_edges: oe}, v_id, nil) do
    case Map.fetch(oe, v_id) do
      {:ok, out} -> out
      :error -> nil
    end
  end

  defp get_out_neighbors(%Graph{out_edges: oe, edge_index: edge_index}, v_id, partitions) do
    case Map.get(oe, v_id) do
      nil ->
        nil

      out_set ->
        filtered =
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

        if MapSet.size(filtered) == 0, do: nil, else: filtered
    end
  end

  defp all_edges(%Graph{type: :undirected, out_edges: oe, in_edges: ie}, v_id) do
    v_in = Map.get(ie, v_id, MapSet.new())
    v_out = Map.get(oe, v_id, MapSet.new())
    MapSet.union(v_in, v_out)
  end

  defp get_all_neighbors(%Graph{type: :undirected} = g, v_id, nil) do
    all_edges(g, v_id)
  end

  defp get_all_neighbors(
         %Graph{type: :undirected, in_edges: ie, out_edges: oe, edge_index: edge_index},
         v_id,
         partitions
       ) do
    all_set = MapSet.union(Map.get(ie, v_id, MapSet.new()), Map.get(oe, v_id, MapSet.new()))

    partitions
    |> Enum.reduce(MapSet.new(), fn partition, acc ->
      edge_index
      |> Map.get(partition, %{})
      |> Map.get(v_id, MapSet.new())
      |> Enum.reduce(acc, fn
        {^v_id, v2_id}, acc -> MapSet.put(acc, v2_id)
        {v1_id, ^v_id}, acc -> MapSet.put(acc, v1_id)
        _, acc -> acc
      end)
    end)
    |> MapSet.intersection(all_set)
  end

  defp cost(g, v1_id, v2_id, hfun, nil) do
    cost(g, v1_id, v2_id, hfun)
  end

  defp cost(%Graph{vertices: vs} = g, v1_id, v2_id, hfun, partitions) do
    edge_weight(g, v1_id, v2_id, partitions) + hfun.(Map.get(vs, v2_id))
  end

  defp cost(%Graph{vertices: vs} = g, v1_id, v2_id, hfun) do
    edge_weight(g, v1_id, v2_id) + hfun.(Map.get(vs, v2_id))
  end

  defp do_edge_weight(g, a, b, nil), do: edge_weight(g, a, b)
  defp do_edge_weight(g, a, b, partitions), do: edge_weight(g, a, b, partitions)

  defp do_bfs(
         q,
         %Graph{type: :directed} = g,
         target_id,
         tree,
         hfun,
         partitions
       ) do
    case PriorityQueue.pop(q) do
      {{:value, {v_id, ^target_id, _}}, _q1} ->
        construct_path(v_id, tree, [target_id])

      {{:value, {v1_id, v2_id, v2_acc_weight}}, q1} ->
        if Map.has_key?(tree, v2_id) do
          do_bfs(q1, g, target_id, tree, hfun, partitions)
        else
          case get_out_neighbors(g, v2_id, partitions) do
            nil ->
              do_bfs(q1, g, target_id, tree, hfun, partitions)

            v2_out ->
              tree = Map.put(tree, v2_id, v1_id)

              q2 =
                v2_out
                |> Enum.map(fn id ->
                  {id, v2_acc_weight + cost(g, v2_id, id, hfun, partitions)}
                end)
                |> Enum.reduce(q1, fn {id, cost}, q ->
                  PriorityQueue.push(
                    q,
                    {v2_id, id, v2_acc_weight + do_edge_weight(g, v2_id, id, partitions)},
                    cost
                  )
                end)

              do_bfs(q2, g, target_id, tree, hfun, partitions)
          end
        end

      {:empty, _} ->
        nil
    end
  end

  defp do_bfs(
         q,
         %Graph{type: :undirected} = g,
         target_id,
         tree,
         hfun,
         partitions
       ) do
    case PriorityQueue.pop(q) do
      {{:value, {v_id, ^target_id, _}}, _q1} ->
        construct_path(v_id, tree, [target_id])

      {{:value, {v1_id, v2_id, v2_acc_weight}}, q1} ->
        if Map.has_key?(tree, v2_id) do
          do_bfs(q1, g, target_id, tree, hfun, partitions)
        else
          neighbors = get_all_neighbors(g, v2_id, partitions)

          if MapSet.equal?(neighbors, MapSet.new()) do
            do_bfs(q1, g, target_id, tree, hfun, partitions)
          else
            tree = Map.put(tree, v2_id, v1_id)

            q2 =
              neighbors
              |> Enum.map(fn id ->
                {id, v2_acc_weight + cost(g, v2_id, id, hfun, partitions)}
              end)
              |> Enum.reduce(q1, fn {id, cost}, q ->
                PriorityQueue.push(
                  q,
                  {v2_id, id, v2_acc_weight + do_edge_weight(g, v2_id, id, partitions)},
                  cost
                )
              end)

            do_bfs(q2, g, target_id, tree, hfun, partitions)
          end
        end

      {:empty, _} ->
        nil
    end
  end

  defp construct_path(id, tree, path) do
    case Map.fetch!(tree, id) do
      ^id -> [id | path]
      parent -> construct_path(parent, tree, [id | path])
    end
  end

  defp dfs(%Graph{} = g, neighbors, target_id, path, visited, paths) when is_list(paths) do
    {paths, excluded} =
      if MapSet.member?(neighbors, target_id) do
        {[Enum.reverse([target_id | path]) | paths], MapSet.put(visited, target_id)}
      else
        {paths, visited}
      end

    neighbors = MapSet.difference(neighbors, excluded)
    do_dfs(g, MapSet.to_list(neighbors), target_id, path, visited, paths)
  end

  defp do_dfs(_g, [], _target_id, _path, _visited, paths) when is_list(paths) do
    paths
  end

  defp do_dfs(
         %Graph{out_edges: oe} = g,
         [next_neighbor_id | neighbors],
         target_id,
         path,
         visited,
         acc
       ) do
    case Map.get(oe, next_neighbor_id) do
      nil ->
        do_dfs(g, neighbors, target_id, path, visited, acc)

      next_neighbors ->
        paths =
          dfs(
            g,
            next_neighbors,
            target_id,
            [next_neighbor_id | path],
            MapSet.put(visited, next_neighbor_id),
            acc
          )

        do_dfs(g, neighbors, target_id, path, visited, paths)
    end
  end
end
