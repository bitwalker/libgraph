defmodule Graph.Queries do
  @moduledoc false

  def topsort(%Graph{vertices: vertices} = g) do
    l = revpostorder(g)
    if length(forest(g, &in_neighbors/3, l)) == map_size(vertices) do
      l
    else
      false
    end
  end

  def preorder(g) do
    Enum.reverse(revpreorder(g))
  end

  def postorder(g) do
    Enum.reverse(revpostorder(g))
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
    Enum.reduce(vs, {MapSet.new, []}, fn v, {t, ll} ->
      pretraverse(handle_first, v, sf, g, t, ll)
    end)
  end

  defp pretraverse(:first, v, sf, g, t, ll) do
    ptraverse([v], sf, g, t, [], ll)
  end
  defp pretraverse(:not_first, v, sf, g, t, ll) do
    if MapSet.member?(t, v) do
      ll
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
  defp ptraverse([], _sf, _g, _t, [], ll), do: ll
  defp ptraverse([], _sf, _g, _t, rs, ll), do: [rs | ll]

  defp revpreorder(g) do
    :lists.append(forest(g, &out_neighbors/3))
  end

  def revpostorder(%Graph{ids: ids} = g) do
    posttraverse(Map.keys(ids), g, MapSet.new, [])
  end

  defp posttraverse([v | vs], g, t, l) do
    {t, l} = if MapSet.member?(t, v) do
          {t, l}
        else
          {MapSet.put(t, v), [v | posttraverse(out_neighbors(g, v, []), g, t, l)]}
        end
    posttraverse(vs, g, t, l)
  end
  defp posttraverse([], _g, _t, l), do: l

  defp in_neighbors(%Graph{edges: edges}, v, vs) do
    Enum.reduce(edges, vs, fn {v1, out_edges}, acc ->
      if MapSet.member?(out_edges, v) do
        acc
      else
        [v1|acc]
      end
    end)
  end

  defp out_neighbors(%Graph{edges: edges}, v, vs \\ []) do
    case Map.get(edges, v) do
      nil -> vs
      ms -> MapSet.to_list(ms) ++ vs
    end
  end

  defp inout(g, v, vs) do
    in_neighbors(g, v, out_neighbors(g, v, vs))
  end
end
