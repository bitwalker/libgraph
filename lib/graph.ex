defmodule Graph do
  @moduledoc """
  This module defines a directed graph data structure, which supports both acyclic and cyclic forms.
  It also defines the API for creating, manipulating, and querying that structure.

  This is intended as a replacement for `:digraph`, which requires the use of 3 ETS tables at a minimum,
  but up to 6 at a time during certain operations (such as `get_short_path/3`). In environments where many
  graphs are in memory at a time, this can be dangerous, as it is easy to hit the system limit for max ETS tables,
  which will bring your node down. This graph implementation does not use ETS, so it can be used freely without
  concern for hitting this limit.

  With regards to space requirements, vertices require 2N space, where N is the size of each vertex, as vertices are
  mapped to indexes (integers), and we store both the vertex->id map and the inverted index (id->vertex) for efficient lookups
  when querying the graph. Edges are stored as a map of `vertex_id -> MapSet(vertex_id)`, which is
  the most compact representation we're allowed while still supporting efficient lookups on edges/neighbors. It is recommended
  that you avoid stuffing large objects in the graph as vertices, and instead use a key which you can then use to reify a list
  of objects you care about from ETS or some other source once you have results you care about.

  There are benchmarks provided with this library which compare it directly to `:digraph` for some common operations,
  and thus far, `libgraph` outperforms `:digraph` in all of them.

  The only bit of data I have not yet evaluated is how much garbage is generated when querying/manipulating the graph
  between `libgraph` and `digraph`, but I suspect the use of ETS means that `digraph` is able to keep that to a minimum.
  Until I verify if that's the case, I would assume that `libgraph` has higher memory requirements, but better performance,
  and is able to side-step the ETS limit. If your requirements, like mine, mean that you are dynamically constructing and querying
  graphs concurrently, I think `libgraph` is the better choice - however if you either need the APIs of `:digraph` that I have
  not yet implemented, or do not have the same use case, I would stick to `:digraph` for now.
  """
  defstruct edges: %{},
            vertices: %{},
            ids: %{},
            next_id: 0

  @opaque t :: %__MODULE__{}
  @type vertex :: term
  @type edge :: {vertex, vertex}

  @doc """
  Creates a new graph.
  """
  @spec new() :: t
  def new do
    %__MODULE__{}
  end

  @doc """
  Returns a map of summary information about this graph.
  """
  @spec info(t) :: %{num_edges: non_neg_integer, num_vertices: non_neg_integer, memory: non_neg_integer}
  def info(%__MODULE__{edges: es, vertices: vs} = g) do
    %{num_edges: es |> Enum.map(&MapSet.size/1) |> Enum.sum,
      num_vertices: map_size(vs),
      size_in_bytes: :erlang.external_size(g)}
  end

  @doc """
  Returns true if and only if the graph `g` is a tree.
  """
  @spec is_tree?(t) :: boolean
  def is_tree?(%__MODULE__{} = g) do
    %{num_edges: num_edges, num_vertices: num_vertices} = info(g)
    if num_edges == (num_vertices - 1) do
      length(components(g)) == 1
    else
      false
    end
  end

  @doc """
  Returns true if and only if the graph `g` is acyclic.
  """
  @spec is_acyclic?(t) :: boolean
  defdelegate is_acyclic?(g), to: Graph.Queries

  @doc """
  Returns true if the graph `g` is not acyclic.
  """
  @spec is_cyclic?(t) :: boolean
  def is_cyclic?(%__MODULE__{} = g) do
    not is_acyclic?(g)
  end

  @doc """
  Gets the shortest path between `a` and `b`. If there are multiple paths of the same length,
  where all are "shortest", this implementation simply takes the first one encountered.

  The algorithm used here is a breadth-first search, which stops evaluation
  as soon as the shortest path to `b` is found. It follows very closely the implementation of
  `get_short_path` from `:digraph`, the only difference being in how some lookups, etc. are performed.

  Example usages can be found in the test suite.
  """
  @spec get_shortest_path(t, vertex, vertex) :: [vertex]
  defdelegate get_shortest_path(g, a, b), to: Graph.Pathfinder

  @doc """
  Builds a list of paths between vertex `a` and vertex `b`.

  The algorithm used here is a depth-first search, which evaluates the whole
  graph until all paths are found. Order is guaranteed to be deterministic,
  but not guaranteed to be in any meaningful order (i.e. shortest to longest).

  Example usages can be found in the test suite.
  """
  @spec get_paths(t, vertex, vertex) :: [[vertex]]
  defdelegate get_paths(g, a, b), to: Graph.Pathfinder

  @doc """
  Return a list of all the edges, where each edge is expressed as a tuple
  of `{A, B}`, where the elements are the vertices involved, and implying the
  direction of the edge to be from `A` to `B`.

  NOTE: You should be careful when using this on dense graphs, as it produces
  lists with whatever you've provided as vertices, with likely many copies of
  each. I'm not sure if those copies are shared in-memory as they are unchanged,
  so it *should* be fairly compact in memory, but I have not verified that to be sure.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a) |> Graph.add_vertex(:b) |> Graph.add_vertex(:c)
      ...> g = g |> Graph.add_edge(:a, :c) |> Graph.add_edge(:b, :c)
      ...> Graph.edges(g)
      [{:a, :c}, {:b, :c}]

  """
  @spec edges(t) :: [edge]
  def edges(%__MODULE__{edges: edges, ids: ids}) do
    edges
    |> Enum.flat_map(fn {source_id, out_neighbors} ->
      source = Map.get(ids, source_id)
      for out_neighbor <- out_neighbors, do: {source, Map.get(ids, out_neighbor)}
    end)
  end

  @doc """
  Returns a list of all the vertices in the graph.

  NOTE: You should be careful when using this on large graphs, as the list it produces
  contains every vertex on the graph. I have not yet verified whether Erlang ensures that
  they are a shared reference with the original, or copies, but if the latter it could result
  in running out of memory if the graph is too large.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a) |> Graph.add_vertex(:b)
      ...> Graph.vertices(g)
      [:a, :b]
  """
  @spec vertices(t) :: vertex
  def vertices(%__MODULE__{vertices: vs}) do
    Map.keys(vs)
  end

  @doc """
  Adds a new vertex to the graph. If the vertex is already present in the graph, the add is a no-op.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a) |> Graph.add_vertex(:a)
      ...> Graph.vertices(g)
      [:a]
  """
  @spec add_vertex(t, vertex) :: t
  def add_vertex(%__MODULE__{vertices: vs, ids: ids, next_id: id} = g, vertex) do
    case Map.get(vs, vertex) do
      nil ->
        %__MODULE__{g |
          vertices: Map.put(vs, vertex, id),
          ids: Map.put(ids, id, vertex),
          next_id: id + 1
        }
      _ ->
        g
    end
  end

  @doc """
  Removes a vertex from the graph, as well as any edges which refer to that vertex. If the vertex does
  not exist in the graph, it is a no-op.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a) |> Graph.add_vertex(:b) |> Graph.add_edge(:a, :b)
      ...> [:a, :b] = Graph.vertices(g)
      ...> [{:a, :b}] = Graph.edges(g)
      ...> g = Graph.delete_vertex(g, :b)
      ...> [:a] = Graph.vertices(g)
      ...> Graph.edges(g)
      []
  """
  @spec delete_vertex(t, vertex) :: t
  def delete_vertex(%__MODULE__{edges: es, vertices: vs, ids: ids} = g, vertex) do
    case Map.get(vs, vertex) do
      nil ->
        g
      v_id ->
        edges = for {source_id, neighbors} <- es, source_id != v_id, do: {source_id, MapSet.delete(neighbors, v_id)}, into: %{}
        %__MODULE__{g |
                    vertices: Map.delete(vs, vertex),
                    ids: for {id, _} = kv <- ids, id != v_id, into: %{} do kv end,
                    edges: edges}
    end
  end

  @doc """
  Adds an edge connecting `a` to `b`. If either `a` or `b` do not exist in the graph,
  they are automatically added. Adding the same edge more than once does not create multiple edges,
  each edge is only ever stored once.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(:a, :b)
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [{:a, :b}]
  """
  @spec add_edge(t, vertex, vertex) :: t
  def add_edge(%__MODULE__{} = g, a, b) do
    %__MODULE__{edges: es, vertices: vs} = g =
      g |> add_vertex(a) |> add_vertex(b)

    a_id = Map.get(vs, a)
    b_id = Map.get(vs, b)
    neighbors = case Map.get(es, a_id) do
                  nil -> MapSet.new([b_id])
                  ms  -> MapSet.put(ms, b_id)
                end
    %__MODULE__{g | edges: Map.put(es, a_id, neighbors)}
  end

  @doc """
  Removes an edge connecting `a` to `b`. If no such vertex exits, or the edge does not exist,
  it is effectively a no-op.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(:a, :b) |> Graph.delete_edge(:a, :b)
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      []
  """
  def delete_edge(%__MODULE__{edges: es, vertices: vs} = g, a, b) do
    case Map.get(vs, a) do
      nil ->
        g
      a_id ->
        case Map.get(vs, b) do
          nil ->
            g
          b_id ->
            case Map.get(es, a_id) do
              nil ->
                g
              neighbors ->
                new_neighbors = MapSet.delete(neighbors, b_id)
                %__MODULE__{g | edges: Map.put(es, a_id, new_neighbors)}
            end
        end
    end
  end

  @doc """
  Returns a topological ordering of the vertices of graph `g`, if such an ordering exists, otherwise it returns false.
  For each vertex in the returned list, no out-neighbors occur earlier in the list.
  """
  @spec topsort(t) :: [vertex]
  defdelegate topsort(g), to: Graph.Queries

  @doc """
  Returns a list of connected components, where each component is a list of vertices.

  A *connected component* is a maximal subgraph such that there is a path between each pair of vertices,
  considering all edges undirected.

  A *subgraph* is a graph whose vertices and edges are a subset of the vertices and edges of the source graph.

  A *maximal subgraph* is a subgraph with property `P` where all other subgraphs which contain the same vertices
  do not have that same property `P`.

  See the test suite for an example of how this is used.
  """
  @spec components(t) :: [[vertex]]
  defdelegate components(g), to: Graph.Queries

  @doc """
  Returns a list of strongly connected components, where each component is a list of vertices.

  A *strongly connected component* is a maximal subgraph such that there is a path between each pair of vertices.

  See `components/1` for the definitions of *subgraph* and *maximal subgraph* if you are unfamiliar with the
  terminology.

  See the test suite for an example of how this is used.
  """
  @spec strong_components(t) :: [[vertex]]
  defdelegate strong_components(g), to: Graph.Queries

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path in the graph from some vertex of `vs` to `v`.

  As paths of length zero are allowed, the vertices of `vs` are also included in the returned list.
  """
  @spec reachable(t, [vertex]) :: [[vertex]]
  defdelegate reachable(g, vs), to: Graph.Queries

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path in the graph of length one or more from some vertex of `vs` to `v`.

  As a consequence, only those vertices of `vs` that are included in some cycle are returned.
  """
  @spec reachable_neighbors(t, [vertex]) :: [[vertex]]
  defdelegate reachable_neighbors(g, vs), to: Graph.Queries

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path from `v` to some vertex of `vs`.

  As paths of length zero are allowed, the vertices of `vs` are also included in the returned list.
  """
  @spec reaching(t, [vertex]) :: [[vertex]]
  defdelegate reaching(g, vs), to: Graph.Queries

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path of length one or more from `v` to some vertex of `vs`.

  As a consequence, only those vertices of `vs` that are included in some cycle are returned.
  """
  @spec reaching_neighbors(t, [vertex]) :: [[vertex]]
  defdelegate reaching_neighbors(g, vs), to: Graph.Queries

  @doc """
  Returns all vertices of graph `g`. The order is given by a depth-first traversal of the graph,
  collecting visited vertices in preorder.
  """
  @spec preorder(t) :: [vertex]
  defdelegate preorder(g), to: Graph.Queries

  @doc """
  Returns all vertices of graph `g`. The order is given by a depth-first traversal of the graph,
  collecting visited vertices in postorder. More precisely, the vertices visited while searching from an
  arbitrarily chosen vertex are collected in postorder, and all those collected vertices are placed before
  the subsequently visited vertices.
  """
  @spec postorder(t) :: [vertex]
  defdelegate postorder(g), to: Graph.Queries

  @doc """
  Returns a list of vertices from graph `g` which are included in a loop.
  """
  @spec loop_vertices(t) :: [vertex]
  defdelegate loop_vertices(g), to: Graph.Queries

  @doc """
  Returns the in-degree of vertex `v` of graph `g`.

  The *in-degree* of a vertex is the number of edges directed inbound towards that vertex.
  """
  def in_degree(%__MODULE__{vertices: vs, edges: es}, v) do
    case Map.get(vs, v) do
      nil ->
        0
      v_id ->
        Enum.reduce(es, 0, fn {_, out}, acc ->
          if MapSet.member?(out, v_id) do
            acc+1
          else
            acc
          end
        end)
    end
  end

  @doc """
  Returns the out-degree of vertex `v` of graph `g`.

  The *out-degree* of a vertex is the number of edges directed outbound from that vertex.
  """
  @spec out_degree(t, vertex) :: non_neg_integer
  def out_degree(%__MODULE__{edges: es, vertices: vs}, v) do
    case Map.get(vs, v) do
      nil ->
        0
      v_id ->
        MapSet.size(Map.get(es, v_id, MapSet.new))
    end
  end

  @doc """
  Returns a list of vertices which all have edges coming in to the given vertex `v`.
  """
  @spec in_neighbors(t, vertex) :: [vertex]
  def in_neighbors(%Graph{vertices: vertices, edges: edges}, v) do
    case Map.get(vertices, v) do
      nil ->
        []
      v_id ->
        Enum.reduce(edges, [], fn {v1_id, out_edges}, acc ->
          if MapSet.member?(out_edges, v_id) do
            acc
          else
            [Map.get(vertices, v1_id)|acc]
          end
        end)
    end
  end

  @doc """
  Returns a list of vertices which the given vertex `v` has edges going to.
  """
  @spec out_neighbors(t, vertex) :: [vertex]
  def out_neighbors(%__MODULE__{edges: es, vertices: vs}, v) do
    case Map.get(vs, v) do
      nil ->
        []
      v_id ->
        es
        |> Map.get(v_id, MapSet.new)
        |> MapSet.to_list
        |> Enum.map(&Map.get(vs, &1))
    end
  end

  @doc """
  Builds a maximal subgraph of `g` which includes all of the vertices in `vs` and the edges which connect them.

  See the test suite for example usage.
  """
  @spec subgraph(t, [vertex]) :: t
  def subgraph(%__MODULE__{edges: es, vertices: vs, ids: ids} = g, vs) do
    allowed = vs |> Enum.map(&Map.get(ids, &1)) |> MapSet.new
    Enum.reduce(vs, new(), fn v, sg ->
      v_id = Map.get(vs, v)
      v_edges = es |> Map.get(v_id, MapSet.new) |> MapSet.intersection(allowed) |> MapSet.to_list
      add_edges(v_edges, v, g, sg, MapSet.new([v_id]), allowed)
    end)
  end

  defp add_edges([], _v, _g, sg, _visited, _allowed), do: sg
  defp add_edges([v2_id|vs], v1, %__MODULE__{ids: ids, edges: es} = g, sg, visited, allowed) do
    if MapSet.member?(visited, v2_id) do
      add_edges(vs, v1, g, sg, visited, allowed)
    else
      v2 = Map.get(ids, v2_id)
      v2_edges = es |> Map.get(v2_id, MapSet.new) |> MapSet.intersection(allowed) |> MapSet.to_list
      add_edges(v2_edges, v2, g, add_edge(sg, v1, v2), MapSet.put(visited, v2_id), allowed)
    end
  end
end
