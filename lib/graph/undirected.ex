defmodule Graph.Undirected do
  @moduledoc false
  @compile {:inline, [in_neighbors: 2, in_neighbors: 3, out_neighbors: 2, out_neighbors: 3]}

  def reachable(%Graph{vertices: vertices, vertex_identifier: vertex_identifier} = g, vs)
      when is_list(vs) do
    vs = Enum.map(vs, vertex_identifier)
    for id <- :lists.append(forest(g, &neighbors/3, vs, :first)), do: Map.get(vertices, id)
  end

  def reachable_neighbors(
        %Graph{vertices: vertices, vertex_identifier: vertex_identifier} = g,
        vs
      )
      when is_list(vs) do
    vs = Enum.map(vs, vertex_identifier)

    for id <- :lists.append(forest(g, &out_neighbors/3, vs, :not_first)),
        do: Map.get(vertices, id)
  end

  def neighbors(%Graph{} = g, v, []) do
    out_neighbors(g, v) ++ in_neighbors(g, v)
  end

  def in_neighbors(%Graph{} = g, v, []) do
    in_neighbors(g, v)
  end

  def in_neighbors(%Graph{in_edges: ie}, v, vs) do
    case Map.get(ie, v) do
      nil -> vs
      v_in -> MapSet.to_list(v_in) ++ vs
    end
  end

  def in_neighbors(%Graph{in_edges: ie}, v) do
    case Map.get(ie, v) do
      nil -> []
      v_in -> MapSet.to_list(v_in)
    end
  end

  def out_neighbors(%Graph{} = g, v, []) do
    in_neighbors(g, v)
  end

  def out_neighbors(%Graph{out_edges: oe}, v, vs) do
    case Map.get(oe, v) do
      nil -> vs
      v_out -> MapSet.to_list(v_out) ++ vs
    end
  end

  def out_neighbors(%Graph{out_edges: oe}, v) do
    case Map.get(oe, v) do
      nil -> []
      v_out -> MapSet.to_list(v_out)
    end
  end

  defp forest(%Graph{vertices: vs} = g, fun) do
    forest(g, fun, Map.keys(vs))
  end

  defp forest(g, fun, vs) do
    forest(g, fun, vs, :first)
  end

  defp forest(g, fun, vs, handle_first) do
    {_, acc} =
      List.foldl(vs, {MapSet.new(), []}, fn v, {visited, acc} ->
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
  defp ptraverse([], _fun, _g, visited, results, acc), do: {visited, [results | acc]}
end
