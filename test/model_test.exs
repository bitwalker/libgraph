defmodule Graph.Model.Test do
  use ExUnit.Case, async: true
  use EQC.ExUnit

  @tag numtests: 1000
  property "a directed acyclic graph (DAG) is always acyclic" do
    forall g <- dag() do
      Graph.is_acyclic?(g)
    end
  end

  @tag numtests: 1000
  property "a directed acyclic graph (DAG) is always topologically sortable" do
    forall g <- dag() do
      Graph.topsort(g) != false
    end
  end

  @tag numtests: 1000
  property "a topsort of a DAG is correct if each element only has edges pointing to subsequent elements" do
    forall %Graph{vertices: vs, out_edges: oe} = g <- dag() do
      sorted = Graph.topsort(g)
      case sorted do
        false ->
          false
        _ ->
          {_, correct?} = Enum.reduce(sorted, {[], true}, fn
            _, {_, false} = res ->
              res
            v, {visited, _} ->
              v_id = Graph.Utils.vertex_id(v)
              edges = Map.get(oe, v_id, MapSet.new)
              backreferences? = Enum.any?(edges, fn e -> Enum.member?(visited, Map.get(vs, e)) end)
              {[v|visited], not backreferences?}
          end)
          correct?
      end
    end
  end

  @tag numtests: 1000
  property "a directed cyclic graph (DCG) is always cyclic" do
    forall g <- dcg() do
      Graph.is_cyclic?(g)
    end
  end

  @tag numtests: 1000
  property "a directed cyclic graph (DCG) is never topologically sortable" do
    forall g <- dcg() do
      Graph.topsort(g) == false
    end
  end

  property "the out degree of a vertex is equal to the number of out neighbors of that vertex (DAG)" do
    forall g <- dag() do
      vs = Graph.vertices(g)
      Enum.reduce(vs, true, fn
        _, false ->
          false
        v, _ ->
          Graph.out_degree(g, v) == length(Graph.out_neighbors(g, v))
      end)
    end
  end

  property "the in degree of a vertex is equal to the number of in neighbors of that vertex (DAG)" do
    forall g <- dag() do
      vs = Graph.vertices(g)
      Enum.reduce(vs, true, fn
        _, false ->
          false
        v, _ ->
          Graph.in_degree(g, v) == length(Graph.in_neighbors(g, v))
      end)
    end
  end

  property "the out degree of a vertex is equal to the number of out neighbors of that vertex (DCG)" do
    forall g <- dcg() do
      vs = Graph.vertices(g)
      Enum.reduce(vs, true, fn
        _, false ->
          false
        v, _ ->
          Graph.out_degree(g, v) == length(Graph.out_neighbors(g, v))
      end)
    end
  end

  property "the in degree of a vertex is equal to the number of in neighbors of that vertex (DCG)" do
    forall g <- dcg() do
      vs = Graph.vertices(g)
      Enum.reduce(vs, true, fn
        _, false ->
          false
        v, _ ->
          Graph.in_degree(g, v) == length(Graph.in_neighbors(g, v))
      end)
    end
  end

  property "the subgraph G' of a given graph G, implies that the vertices and edges of G' form subsets of those from G (DAG)" do
    forall g <- dag() do
      implies Graph.num_vertices(g) > 0 do
        g_vertices = g |> Graph.vertices |> MapSet.new
        g_edges = g |> Graph.edges |> MapSet.new
        subset_vertices = g_vertices |> Enum.shuffle |> Enum.take(:rand.uniform(MapSet.size(g_vertices) - 1))
        sg = Graph.subgraph(g, subset_vertices)
        sg_vertices = sg |> Graph.vertices |> MapSet.new
        sg_edges = sg |> Graph.edges |> MapSet.new
        MapSet.subset?(sg_vertices, g_vertices) && MapSet.subset?(sg_edges, g_edges)
      end
    end
  end

  property "the subgraph G' of a given graph G, implies that the vertices and edges of G' form subsets of those from G (DCG)" do
    forall g <- dcg() do
      g_vertices = g |> Graph.vertices |> MapSet.new
      g_edges = g |> Graph.edges |> MapSet.new
      subset_vertices = g_vertices |> Enum.shuffle |> Enum.take(:rand.uniform(MapSet.size(g_vertices) - 1))
      sg = Graph.subgraph(g, subset_vertices)
      sg_vertices = sg |> Graph.vertices |> MapSet.new
      sg_edges = sg |> Graph.edges |> MapSet.new
      MapSet.subset?(sg_vertices, g_vertices) && MapSet.subset?(sg_edges, g_edges)
    end
  end

  property "connected components of a graph are lists of vertices where exists an adirectional path between each pair of vertices" do
    forall g <- dag() do
      implies Graph.num_vertices(g) > 0 do
        components = Graph.components(g)
        Enum.all?(components, fn
          component when length(component) < 2 ->
            true
          component ->
            for j <- component, k <- component, j != k do
              Graph.get_shortest_path(g, j, k) != nil || Graph.get_shortest_path(g, k, j) != nil
            end
        end)
      end
    end
  end

  property "strongly connected components of a graph are lists of vertices where exits a bidirectional path between each pair of vertices" do
    forall g <- dcg() do
      strong_components = Graph.strong_components(g)
      Enum.all?(strong_components, fn
        component ->
          for j <- component, k <- component, j != k do
            Graph.get_shortest_path(g, j, k) != nil && Graph.get_shortest_path(g, j, k) != nil
          end
      end)
    end
  end

  ## Private

  def dag() do
    such_that g <- sized_dag() do
      Graph.is_acyclic?(g)
    end
  end
  defp sized_dag() do
    sized s, do: sized_dag(s, Graph.new)
  end
  defp sized_dag(0, g) do
    g
  end
  defp sized_dag(i, g) do
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

  def dcg() do
    such_that g <- sized_dcg() do
      Graph.is_cyclic?(g)
    end
  end
  defp sized_dcg() do
    sized s, do: sized_dcg(s, Graph.new)
  end
  # We cannot produce a "real" DCG unless we have at least 2 vertices,
  # as a single vertex DCG which has an edge to itself is still topsortable
  # and so does not hold to the property that DCGs are not topsortable
  # We are handling it this way to maintain compatibility with digraph (for now),
  # but this may change in the future
  defp sized_dcg(size, g) when size < 2 do
    sized_dcg(size + 1, g)
  end
  defp sized_dcg(0, g), do: g
  defp sized_dcg(1, g), do: g
  defp sized_dcg(i, g) do
    g = Enum.reduce(0..i, g, fn v, g -> Graph.add_vertex(g, v) end)
    Enum.reduce(0..i, g, fn v, g ->
      r = 0..i
      Stream.iterate(Enum.random(r), fn _ -> Enum.random(r) end)
      |> Stream.filter(fn v2 -> v2 != v end)
      |> Enum.take(:rand.uniform(6))
      |> Enum.reduce(g, fn v2, acc -> Graph.add_edge(acc, v, v2) end)
    end)
  end

  def mesh() do
    such_that g <- sized_mesh() do
      cyclic? = Graph.is_cyclic?(g)
      num_vertices = Graph.num_vertices(g)
      strongly_connected? = Enum.reduce(g.vertices, true, fn
        _, false ->
          false
        {v_id, _v}, _acc ->
          out_edges = Map.get(g.out_edges, v_id, MapSet.new) |> MapSet.delete(v_id)
          in_edges = Map.get(g.in_edges, v_id, MapSet.new) |> MapSet.delete(v_id)
          MapSet.size(out_edges) == (num_vertices - 1) &&
            MapSet.size(in_edges) == (num_vertices - 1) &&
            MapSet.size(MapSet.difference(out_edges, in_edges)) == 0
      end)
      cyclic? && strongly_connected?
    end
  end
  defp sized_mesh() do
    sized s, do: sized_mesh(s, Graph.new)
  end
  defp sized_mesh(size, g) when size < 2 do
    sized_mesh(size + 1, g)
  end
  defp sized_mesh(size, g) do
    g = Enum.reduce(0..size, g, fn v, g -> Graph.add_vertex(g, v) end)
    vs = Graph.vertices(g)
    Enum.reduce(vs, g, fn v, acc ->
      Enum.reduce(vs, acc, fn
        ^v, acc2 ->
          acc2
        v2, acc2 ->
          Graph.add_edge(acc2, v, v2)
      end)
    end)
  end
end
