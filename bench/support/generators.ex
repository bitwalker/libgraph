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
        Stream.iterate(Enum.random(r), fn _ -> Enum.random(r) end)
        |> Enum.take(6)
        |> Enum.reduce(g, fn v2, acc ->
          Graph.add_edge(acc, v, v2)
        end)
      end
    end)
  end

  def biased_dag(size) do
    biased_dag(size, Graph.new)
  end
  defp biased_dag(0, g) do
    g
  end
  defp biased_dag(i, g) do
    i = i+1
    g = Enum.reduce(0..i, g, fn v, g -> Graph.add_vertex(g, v) end)
    Enum.reduce(1..i, g, fn v, g ->
      if v+1 > i do
        g
      else
        r = (v+1)..i
        odds =
          Stream.iterate(Enum.random(r), fn _ -> Enum.random(r) end)
          |> Stream.filter(fn i -> rem(i, 2) != 0 end)
          |> Enum.take(3)
        evens =
          r
          |> Stream.map(fn j -> j * 2 end)
          |> Stream.filter(fn j -> j <= i && rem(j, 2) == 0 end)
          |> Enum.take(3)
        Enum.reduce(odds ++ evens, g, fn v2, acc ->
          Graph.add_edge(acc, v, v2)
        end)
      end
    end)
  end

  def libgraph_to_digraph(%Graph{vertices: vs, out_edges: es, ids: ids}) do
    dg = :digraph.new
    for {v, _} <- vs, do: :digraph.add_vertex(dg, v)
    for {v_id, v_out} <- es, v2_id <- v_out do
      :digraph.add_edge(dg, v_id, v2_id)
    end
    dg
  end
end
