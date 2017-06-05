defmodule Graph.Queries do
  @moduledoc false

  def topsort(%Graph{ids: ids} = g) do
    l = revpostorder(g)
    if length(forest(g, &in_neighbors/3, l)) == map_size(ids) do
      Enum.map(l, &Map.get(ids, &1))
    else
      false
    end
  end

  def preorder(%Graph{ids: ids} = g) do
    g
    |> revpreorder()
    |> Stream.map(fn id -> Map.get(ids, id) end)
    |> Enum.reverse
  end

  def postorder(%Graph{ids: ids} = g) do
    g
    |> revpostorder()
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
    for component <- forest(g, &in_neighbors/3, revpostorder(g)) do
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

  defp forest(%Graph{ids: ids} = g, sf) do
    forest(g, sf, Map.keys(ids))
  end

  defp forest(g, sf, vs) do
    forest(g, sf, vs, :first)
  end

  defp forest(g, sf, vs, handle_first) do
    {_, ll} = List.foldl(vs, {MapSet.new, []}, fn v, {t, ll} ->
      pretraverse(handle_first, v, sf, g, t, ll)
    end)
    ll
  end

  defp pretraverse(:first, v, sf, g, t, ll) do
    ptraverse([v], sf, g, t, [], ll)
  end
  defp pretraverse(:not_first, v, sf, g, t, ll) do
    if MapSet.member?(t, v) do
      {t, ll}
    else
      ptraverse(sf.(g, v, []), sf, g, t, [], ll)
    end
  end

  defp ptraverse([v | vs], sf, g, t, rs, ll) do
    if MapSet.member?(t, v) do
      ptraverse(vs, sf, g, t, rs, ll)
    else
      t = MapSet.put(t, v)
      ptraverse(sf.(g, v, vs), sf, g, t, [v | rs], ll)
    end
  end
  defp ptraverse([], _sf, _g, t, [], ll), do: {t, ll}
  defp ptraverse([], _sf, _g, t, rs, ll), do: {t, [rs | ll]}

  defp revpreorder(g) do
    :lists.append(forest(g, &out_neighbors/3))
  end

  def revpostorder(%Graph{ids: ids} = g) do
    {_, l} = posttraverse(Map.keys(ids), g, MapSet.new, [])
    l
  end

  defp posttraverse([v | vs], g, t, l) do
    {t, l} = if MapSet.member?(t, v) do
          {t, l}
        else
          t = MapSet.put(t, v)
          {t2, l2} = posttraverse(out_neighbors(g, v, []), g, t, l)
          {t2, [v|l2]}
        end
    posttraverse(vs, g, t, l)
  end
  defp posttraverse([], _g, t, l), do: {t, l}

  defp in_neighbors(%Graph{edges: edges}, v, vs \\ []) do
    Enum.reduce(edges, vs, fn {v1, out_edges}, acc ->
      if MapSet.member?(out_edges, v) do
        [v1|acc]
      else
        acc
      end
    end)
  end

  defp out_neighbors(%Graph{edges: edges}, v, vs \\ []) do
    edges
    |> Map.get(v, MapSet.new)
    |> MapSet.to_list
    |> Enum.concat(vs)
  end

  defp inout(g, v, vs) do
    in_neighbors(g, v, out_neighbors(g, v, vs))
  end
end
