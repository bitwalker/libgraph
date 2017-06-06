defmodule Graph.Impl do
  @moduledoc false

  def topsort(%Graph{ids: ids} = g) do
    l = reverse_postorder(g)
    if length(forest(g, &in_neighbors/3, l)) == map_size(ids) do
      Enum.map(l, &Map.get(ids, &1))
    else
      false
    end
  end

  def preorder(%Graph{ids: ids} = g) do
    g
    |> reverse_preorder()
    |> Stream.map(fn id -> Map.get(ids, id) end)
    |> Enum.reverse
  end

  def postorder(%Graph{ids: ids} = g) do
    g
    |> reverse_postorder()
    |> Stream.map(fn id -> Map.get(ids, id) end)
    |> Enum.reverse
  end

  def is_arborescence?(%Graph{} = g) do
    arborescence_root(g) != nil
  end

  def arborescence_root(%Graph{edges: es, ids: ids} = g) when map_size(es) == (map_size(ids) - 1) do
    [root] = List.foldl(ids, [], fn v, acc ->
      case length(in_neighbors(g, v)) do
        1 -> acc
        0 when acc == [] -> [v]
      end
    end)
    root
  catch
    _, _ ->
      nil
  end
  def arborescence_root(_g), do: nil

  def is_acyclic?(%Graph{} = g) do
    loop_vertices_w_ids(g) == [] and topsort(g) != false
  end

  def loop_vertices(%Graph{ids: ids} = g) do
    for id <- loop_vertices_w_ids(g), do: Map.get(ids, id)
  end

  defp loop_vertices_w_ids(%Graph{ids: ids} = g) do
    for v <- Map.keys(ids), is_reflexive_vertex(g, v), do: v
  end

  def is_reflexive_vertex(g, v) do
    Enum.member?(out_neighbors(g, v), v)
  end

  def components(%Graph{ids: ids} = g) do
    for component <- forest(g, &inout/3) do
      for id <- component, do: Map.get(ids, id)
    end
  end

  def strong_components(%Graph{ids: ids} = g) do
    for component <- forest(g, &in_neighbors/3, reverse_postorder(g)) do
      for id <- component, do: Map.get(ids, id)
    end
  end

  def reachable(%Graph{ids: ids} = g, vs) when is_list(vs) do
    for id <- :lists.append(forest(g, &out_neighbors/3, vs, :first)), do: Map.get(ids, id)
  end

  def reachable_neighbors(%Graph{ids: ids} = g, vs) when is_list(vs) do
    for id <- :lists.append(forest(g, &out_neighbors/3, vs, :not_first)), do: Map.get(ids, id)
  end

  def reaching(%Graph{ids: ids} = g, vs) when is_list(vs) do
    for id <- :lists.append(forest(g, &in_neighbors/3, vs, :first)), do: Map.get(ids, id)
  end

  def reaching_neighbors(%Graph{ids: ids} = g, vs) when is_list(vs) do
    for id <- :lists.append(forest(g, &in_neighbors/3, vs, :not_first)), do: Map.get(ids, id)
  end

  ## Private

  defp forest(%Graph{ids: ids} = g, fun) do
    forest(g, fun, Map.keys(ids))
  end

  defp forest(g, fun, vs) do
    forest(g, fun, vs, :first)
  end

  defp forest(g, fun, vs, handle_first) do
    {_, acc} = List.foldl(vs, {MapSet.new, []}, fn v, {visited, acc} ->
      pretraverse(handle_first, v, fun, g, visited, acc)
    end)
    acc
  end

  defp pretraverse(:first, v, fun, g, visited, acc) do
    ptraverse([v], fun, g, visited, [], acc)
  end
  defp pretraverse(:not_first, v, fun, g, visited, acc) do
    if MapSet.member?(visited, v) do
      {visited, acc}
    else
      ptraverse(fun.(g, v, []), fun, g, visited, [], acc)
    end
  end

  defp ptraverse([v | vs], fun, g, visited, results, acc) do
    if MapSet.member?(visited, v) do
      ptraverse(vs, fun, g, visited, results, acc)
    else
      visited = MapSet.put(visited, v)
      ptraverse(fun.(g, v, vs), fun, g, visited, [v | results], acc)
    end
  end
  defp ptraverse([], _fun, _g, visited, [], acc), do: {visited, acc}
  defp ptraverse([], _fun, _g, visited, results, acc), do: {visited, [results|acc]}

  defp reverse_preorder(g) do
    :lists.append(forest(g, &out_neighbors/3))
  end

  def reverse_postorder(%Graph{ids: ids} = g) do
    {_, l} = posttraverse(Map.keys(ids), g, MapSet.new, [])
    l
  end

  defp posttraverse([v | vs], g, visited, acc) do
    {visited, acc} = if MapSet.member?(visited, v) do
          {visited, acc}
        else
          visited = MapSet.put(visited, v)
          {visited2, acc2} = posttraverse(out_neighbors(g, v, []), g, visited, acc)
          {visited2, [v|acc2]}
        end
    posttraverse(vs, g, visited, acc)
  end
  defp posttraverse([], _g, visited, acc), do: {visited, acc}

  def in_neighbors(%Graph{edges: edges}, v, vs \\ []) do
    Enum.reduce(edges, vs, fn {v1, out_edges}, acc ->
      if MapSet.member?(out_edges, v) do
        [v1|acc]
      else
        acc
      end
    end)
  end

  def out_neighbors(%Graph{edges: edges}, v, vs \\ []) do
    edges
    |> Map.get(v, MapSet.new)
    |> MapSet.to_list
    |> Enum.concat(vs)
  end

  defp inout(g, v, vs) do
    in_neighbors(g, v, out_neighbors(g, v, vs))
  end
end
