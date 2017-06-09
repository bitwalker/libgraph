defmodule Graph.Reducer do
  @moduledoc false

  def walk(%Graph{out_edges: oe, vertices: vs} = g, acc, fun, opts) when is_function(fun, 4) do
    case Keyword.get(opts, :algorithm, :depth_first) do
      :depth_first ->
        visit_dfs(Map.keys(vs), g, MapSet.new, fun, acc)
      :breadth_first ->
        case Map.keys(vs) do
          [] -> acc
          [starting_id|rest] ->
            starting_out = Map.get(oe, starting_id, MapSet.new)
            q = :queue.in(starting_id, :queue.new)
            q = Enum.reduce(starting_out, q, fn v_id, q -> :queue.in(v_id, q) end)
            visit_bfs(q, rest, g, MapSet.new, fun, acc)
        end
    end
  end

  defp visit_dfs([v_id|rest], %Graph{out_edges: oe, vertices: vs} = g, visited, fun, acc) do
    if MapSet.member?(visited, v_id) do
      visit_dfs(rest, g, visited, fun, acc)
    else
      v = Map.get(vs, v_id)
      out_neighbors = Graph.out_edges(g, v)
      in_neighbors = Graph.in_edges(g, v)
      case fun.(v, out_neighbors, in_neighbors, acc) do
        {:next, acc2} ->
          visited = MapSet.put(visited, v_id)
          out = oe |> Map.get(v_id, MapSet.new) |> MapSet.to_list
          visit_dfs(out ++ rest, g, visited, fun, acc2)
        {:next, v_next, acc2} ->
          v_next_id = Map.get(vs, v_next)
          visited = visited |> MapSet.put(v_id) |> MapSet.delete(v_next_id)
          visit_dfs([v_next_id | rest], g, visited, fun, acc2)
        {:skip, acc2} ->
          # Skip this vertex and it's out-neighbors
          visited = MapSet.put(visited, v_id)
          visit_dfs(rest, g, visited, fun, acc2)
        {:halt, acc2} ->
          acc2
      end
    end
  end
  defp visit_dfs([], _g, _visited, _fun, acc) do
    acc
  end

  def visit_bfs(q, vs, %Graph{out_edges: oe, vertices: vertices} = g, visited, fun, acc) do
    case {:queue.out(q), vs} do
      {{{:value, v_id}, q1}, _vs} ->
        if MapSet.member?(visited, v_id) do
          visit_bfs(q1, vs, g, visited, fun, acc)
        else
          v = Map.get(vertices, v_id)
          out_neighbors = Graph.out_edges(g, v)
          in_neighbors = Graph.in_edges(g, v)
          case fun.(v, out_neighbors, in_neighbors, acc) do
            {:next, acc2} ->
              visited = MapSet.put(visited, v_id)
              v_out = Map.get(oe, v_id, MapSet.new)
              q2 = v_out |> MapSet.to_list |> List.foldl(q1, fn vid, q -> :queue.in(vid, q) end)
              visit_bfs(q2, vs, g, visited, fun, acc2)
            {:next, v_next, acc2} ->
              v_next_id = Map.get(vertices, v_next)
              visited = visited |> MapSet.put(v_id) |> MapSet.delete(v_next_id)
              q2 = :queue.in_r(v_next_id, q1)
              visit_bfs(q2, vs, g, visited, fun, acc2)
            {:skip, acc2} ->
              visited = MapSet.put(visited, v_id)
              visit_bfs(q1, vs, g, visited, fun, acc2)
            {:halt, acc2} ->
              acc2
          end
        end
      {{:empty, _}, []} ->
        acc
      {{:empty, q1}, [v_id|vs]} ->
        q2 = :queue.in(v_id, q1)
        visit_bfs(q2, vs, g, visited, fun, acc)
    end
  end
end
