defmodule Graph.Bench.Generators do
  @moduledoc false

  def dag(size) do
    dag(size, Graph.new)
  end
  defp dag(0, g) do
    g
  end
  defp dag(i, g) do
    i = i+1
    g = Enum.reduce(0..i, g, fn v, g -> Graph.add_vertex(g, v) end)
    Enum.reduce(1..i, g, fn v, g ->
      if v+1 > i do
        g
      else
        r = (v+1)..i
        v2s = Stream.iterate(Enum.random(r), fn _ -> Enum.random(r) end)
        Enum.reduce(Enum.take(v2s, 6), g, fn v2, acc ->
          Graph.add_edge(acc, v, v2)
        end)
      end
    end)
  end

  def libgraph_to_digraph(%Graph{vertices: vs, out_edges: es, ids: ids}) do
    dg = :digraph.new
    for {v, _} <- vs, do: :digraph.add_vertex(dg, v)
    for {v_id, v_out} <- es, v2_id <- v_out do
      v2 = Map.get(ids, v2_id)
      :digraph.add_edge(dg, v_id, v2_id)
    end
    dg
  end
end
