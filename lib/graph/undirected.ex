defmodule Graph.Undirected do
  @compile {:inline}

  def reachable(%Graph{vertices: vertices, vertex_identifier: vertex_identifier} = g, vs)
      when is_list(vs) do
    vs = Enum.map(vs, vertex_identifier)
    for id <- :lists.append(forest(g, &neighbors/3, vs, :first)), do: Map.get(vertices, id)
  end

  def neighbors(%Graph{} = g, v, []) do
    neighbors(g, v)
  end

  def neighbors(%Graph{out_edges: oe, in_edges: ie}, v, vs) do
    case {Map.get(ie, v), Map.get(oe, v)} do
      {nil, nil} ->
        vs

      {v_in, nil} ->
        MapSet.to_list(v_in) ++ vs

      {nil, v_out} ->
        MapSet.to_list(v_out) ++ vs

      {v_in, v_out} ->
        s = MapSet.union(v_in, v_out)
        MapSet.to_list(s) ++ vs
    end
  end

  def neighbors(%Graph{out_edges: oe, in_edges: ie}, v) do
    v_in = Map.get(ie, v, MapSet.new())
    v_out = Map.get(oe, v, MapSet.new())

    MapSet.union(v_in, v_out)
    |> MapSet.to_list()
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
