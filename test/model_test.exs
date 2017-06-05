defmodule Graph.Model.Test do
  use ExUnit.Case, async: true
  use PropCheck

  property "a directed acyclic graph (DAG) is always acyclic", [max_size: 1_000] do
    forall g <- dag(integer()) do
      Graph.is_acyclic?(g)
    end
  end

  property "a directed acyclic graph (DAG) is always topologically sortable", [max_size: 1_000] do
    forall g <- dag(integer()) do
      Graph.topsort(g) != false
    end
  end

  property "a topsort of a DAG is correct if each element only has edges pointing to subsequent elements", [max_size: 1_000] do
    forall %Graph{vertices: vs, edges: es, ids: ids} = g <- dag(integer()) do
      sorted = Graph.topsort(g)
      case sorted do
        false ->
          false
        _ ->
          {_, correct?} = Enum.reduce(sorted, {[], true}, fn
            _, {_, false} = res ->
              res
            v, {visited, _} ->
              v_id = Map.get(vs, v)
              edges = Map.get(es, v_id, MapSet.new)
              backreferences? = Enum.any?(edges, fn e -> Enum.member?(visited, Map.get(ids, e)) end)
              {[v|visited], not backreferences?}
          end)
          correct?
      end
    end
  end

  def dag(_) do
    sized(s, dag(s, Graph.new))
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
        Enum.reduce(Enum.take(v2s, :rand.uniform(6)), g, fn v2, acc ->
          Graph.add_edge(acc, v, v2)
        end)
      end
    end)
  end
end
