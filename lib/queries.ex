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

  def is_acyclic?(%Graph{} = g) do
    loop_vertices(g) == [] and topsort(g) != false
  end

  def loop_vertices(%Graph{ids: ids} = g) do
    for v <- Map.keys(ids), is_reflexive_vertex(g, v), do: v
  end

  def is_reflexive_vertex(g, v) do
    Enum.member?(out_neighbors(g, v), v)
  end

  def components(g) do
    forest(g, &inout/3)
  end

  def strong_components(g) do
    forest(g, &in_neighbors/3, revpostorder(g))
  end

  def reachable(g, vs) when is_list(vs) do
    :lists.append(forest(g, &out_neighbors/3, vs, :first))
  end

  def reachable_neighbors(g, vs) when is_list(vs) do
    :lists.append(forest(g, &out_neighbors/3, vs, :not_first))
  end

  def reaching(g, vs) when is_list(vs) do
    :lists.append(forest(g, &in_neighbors/3, vs, :first))
  end

  def reaching_neighbors(g, vs) when is_list(vs) do
    :lists.append(forest(g, &in_neighbors/3, vs, :not_first))
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

  defp in_neighbors(%Graph{edges: edges}, v, vs) do
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
