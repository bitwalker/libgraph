defmodule Graph do
  @moduledoc """
  This module defines a directed graph data structure, which supports both acyclic and cyclic forms.
  It also defines the API for creating, manipulating, and querying that structure.

  This is intended as a replacement for `:digraph`, which requires the use of 3 ETS tables at a minimum,
  but up to 6 at a time during certain operations (such as `get_short_path/3`). In environments where many
  graphs are in memory at a time, this can be dangerous, as it is easy to hit the system limit for max ETS tables,
  which will bring your node down. This graph implementation does not use ETS, so it can be used freely without
  concern for hitting this limit.

  The following properties should be kept in mind when planning space requirements:

  - The graph structure stores vertices twice, as a map of vertex -> id (integer) and it's inverse index;
  I have not yet been able to determine if shared references are used, or if the runtime forces copies, but you should be aware that
  the graph may require up to `2((sizeof(V)*N)+(sizeof(integer)*N))`, where `N` is the number of vertices
  - The graph also contains a map of all out edges and it's inverse, so each edge is
  `2((3*sizeof(integer))+2)` bytes, which represents the 6 integers used, and the bytes needed
  for the tuples used
  - Additionally, each edge with metadata (weight/label) will incur the cost for a new list, a tuple (per option), and
  the size of the term stored

  You can obtain a "true" size in bytes, by calling `Graph.info/1`, which gets the size in bytes of the graph when encoded
  using Erlang External Term Format.

  The reason for the different internal structures, particularly the inverse indexes, is performance. In order to efficiently
  perform queries on the graph, we need quick key-based lookup for both in-edges and out-edges for a vertex. Additionally, we
  need to work with the smallest possible keys when storing edges, which means we need a map of vertices to their ids, and the inverse
  of that lookup so that we can reify a collection of ids to their associated vertices/edges. Internally, we work strictly with ids
  and only convert back to the actual vertex (or create an `Edge` struct) when we have the result set. This balances performance,
  space requirements, and ease of maintenance.

  There are benchmarks provided with this library which compare it directly to `:digraph` for some common operations,
  and thus far, `libgraph` outperforms `:digraph` in all of them.

  The only bit of data I have not yet evaluated is how much garbage is generated when querying/manipulating the graph
  between `libgraph` and `digraph`, but I suspect the use of ETS means that `digraph` is able to keep that to a minimum.
  Until I verify if that's the case, I would assume that `libgraph` has higher memory requirements, but better performance,
  and is able to side-step the ETS limit. If your requirements, like mine, mean that you are dynamically constructing and querying
  graphs concurrently, I think `libgraph` is the better choice - however if you either need the APIs of `:digraph` that I have
  not yet implemented, or do not have the same use case, I would stick to `:digraph` for now.
  """
  defstruct in_edges: %{},
            out_edges: %{},
            edges_meta: %{},
            vertices: %{},
            ids: %{},
            next_id: 0

  alias Graph.Edge

  @opaque t :: %__MODULE__{}
  @type vertex :: term
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
  @spec info(t) :: %{num_edges: non_neg_integer, num_vertices: non_neg_integer}
  def info(%__MODULE__{} = g) do
    %{num_edges: num_edges(g),
      num_vertices: num_vertices(g),
      size_in_bytes: :erlang.external_size(g)}
  end

  @doc """
  Returns the number of edges in the graph
  """
  @spec num_edges(t) :: non_neg_integer
  def num_edges(%__MODULE__{out_edges: es}) do
    Enum.reduce(es, 0, fn {_, out}, sum -> sum + MapSet.size(out) end)
  end

  @doc """
  Returns the number of vertices in the graph
  """
  @spec num_vertices(t) :: non_neg_integer
  def num_vertices(%__MODULE__{vertices: vs}) do
    map_size(vs)
  end

  @doc """
  Returns true if and only if the graph `g` is a tree.
  """
  @spec is_tree?(t) :: boolean
  def is_tree?(%__MODULE__{out_edges: es, vertices: vs} = g) do
    num_edges = Enum.reduce(es, 0, fn {_, out}, sum -> sum + MapSet.size(out) end)
    num_vertices = map_size(vs)
    if num_edges == (num_vertices - 1) do
      length(components(g)) == 1
    else
      false
    end
  end

  @doc """
  Returns true if the graph is an aborescence, a directed acyclic graph,
  where the *root*, a vertex, of the arborescence has a unique path from itself
  to every other vertex in the graph.
  """
  @spec is_arborescence?(t) :: boolean
  defdelegate is_arborescence?(g), to: Graph.Impl

  @doc """
  Returns the root vertex of the arborescence, if one exists, otherwise nil.
  """
  @spec arborescence_root(t) :: vertex | nil
  defdelegate arborescence_root(g), to: Graph.Impl

  @doc """
  Returns true if and only if the graph `g` is acyclic.
  """
  @spec is_acyclic?(t) :: boolean
  defdelegate is_acyclic?(g), to: Graph.Impl

  @doc """
  Returns true if the graph `g` is not acyclic.
  """
  @spec is_cyclic?(t) :: boolean
  def is_cyclic?(%__MODULE__{} = g) do
    not is_acyclic?(g)
  end

  @doc """
  Returns true if graph `g1` is a subgraph of `g2`.

  A graph is a subgraph of another graph if it's vertices and edges
  are a subset of that graph's vertices and edges.

  ## Example

      iex> g1 = Graph.new |> Graph.add_vertices([:a, :b, :c, :d]) |> Graph.add_edge(:a, :b) |> Graph.add_edge(:b, :c)
      ...> g2 = Graph.new |> Graph.add_vertices([:b, :c]) |> Graph.add_edge(:b, :c)
      ...> Graph.is_subgraph?(g2, g1)
      true
  """
  @spec is_subgraph?(t, t) :: boolean
  def is_subgraph?(%__MODULE__{out_edges: es1, vertices: vs1} = g1, %__MODULE__{out_edges: es2, vertices: vs2} = g2) do
    ids1 = g1.ids
    ids2 = g2.ids
    for {v, _} <- vs1 do
      unless Map.has_key?(vs2, v), do: throw(:not_subgraph)
    end
    for {g1_v_id, g1_v_out} <- es1 do
      g1_v_out_full = g1_v_out |> Enum.map(&Map.get(ids1, &1)) |> MapSet.new
      g2_v_id = Map.get(vs2, Map.get(ids1, g1_v_id))
      g2_v_out_full = Map.get(es2, g2_v_id, MapSet.new) |> Enum.map(&Map.get(ids2, &1)) |> MapSet.new
      unless MapSet.subset?(g1_v_out_full, g2_v_out_full) do
        throw :not_subgraph
      end
    end
    true
  catch
    :throw, :not_subgraph ->
      false
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
  defdelegate get_shortest_path(g, a, b), to: Graph.Pathing, as: :shortest_path

  @doc """
  Builds a list of paths between vertex `a` and vertex `b`.

  The algorithm used here is a depth-first search, which evaluates the whole
  graph until all paths are found. Order is guaranteed to be deterministic,
  but not guaranteed to be in any meaningful order (i.e. shortest to longest).

  Example usages can be found in the test suite.
  """
  @spec get_paths(t, vertex, vertex) :: [[vertex]]
  defdelegate get_paths(g, a, b), to: Graph.Pathing, as: :all

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
      [%Graph.Edge{v1: :a, v2: :c}, %Graph.Edge{v1: :b, v2: :c}]

  """
  @spec edges(t) :: [Edge.t]
  def edges(%__MODULE__{out_edges: edges, edges_meta: edges_meta, ids: ids}) do
    edges
    |> Enum.flat_map(fn {source_id, out_neighbors} ->
      source = Map.get(ids, source_id)
      for out_neighbor <- out_neighbors do
        meta = Map.get(edges_meta, {source_id, out_neighbor})
        Edge.new(source, Map.get(ids, out_neighbor), meta)
      end
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
  Like `add_vertex/2`, but takes a list of vertices to add to the graph.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :a])
      ...> Graph.vertices(g)
      [:a, :b]
  """
  @spec add_vertices(t, [vertex]) :: t
  def add_vertices(%__MODULE__{} = g, vs) when is_list(vs) do
    Enum.reduce(vs, g, &add_vertex(&2, &1))
  end

  @doc """
  Replaces `vertex` with `new_vertex` in the graph.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b]) |> Graph.add_edge(:a, :b)
      ...> [:a, :b] = Graph.vertices(g)
      ...> g = Graph.replace_vertex(g, :a, :c)
      ...> [:b, :c] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :c, v2: :b}]
  """
  @spec replace_vertex(t, vertex, vertex) :: t | {:error, :no_such_vertex}
  def replace_vertex(%__MODULE__{vertices: vs, ids: ids} = g, v, rv) do
    with {:ok, v_id} <- Graph.Impl.find_vertex_id(g, v),
           vs  <- Map.put(Map.delete(vs, v), rv, v_id),
           ids <- Map.put(ids, v_id, rv) do
      %__MODULE__{g | vertices: vs, ids: ids}
    else
      _ -> {:error, :no_such_vertex}
    end
  end

  @doc """
  Removes a vertex from the graph, as well as any edges which refer to that vertex. If the vertex does
  not exist in the graph, it is a no-op.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a) |> Graph.add_vertex(:b) |> Graph.add_edge(:a, :b)
      ...> [:a, :b] = Graph.vertices(g)
      ...> [%Graph.Edge{v1: :a, v2: :b}] = Graph.edges(g)
      ...> g = Graph.delete_vertex(g, :b)
      ...> [:a] = Graph.vertices(g)
      ...> Graph.edges(g)
      []
  """
  @spec delete_vertex(t, vertex) :: t
  def delete_vertex(%__MODULE__{out_edges: oe, in_edges: ie, edges_meta: em, vertices: vs, ids: ids} = g, v) do
    with {:ok, v_id} <- Graph.Impl.find_vertex_id(g, v),
           oe <- Map.delete(oe, v_id),
           ie <- Map.delete(ie, v_id),
           vs <- Map.delete(vs, v),
           ids <- Map.delete(ids, v_id) do
      oe = for {id, ns} <- oe, do: {id, MapSet.delete(ns, v_id)}, into: %{}
      em = for {{id1, id2}, _} = e <- em, v_id != id1 && v_id != id2, do: e, into: %{}
      %__MODULE__{g |
                  vertices: vs,
                  ids: ids,
                  out_edges: oe,
                  in_edges: ie,
                  edges_meta: em}
    else
      _ -> g
    end
  end

  @doc """
  Like `delete_vertex/2`, but takes a list of vertices to delete from the graph.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.delete_vertices([:a, :b])
      ...> Graph.vertices(g)
      [:c]
  """
  @spec delete_vertices(t, [vertex]) :: t
  def delete_vertices(%__MODULE__{} = g, vs) when is_list(vs) do
    Enum.reduce(vs, g, &delete_vertex(&2, &1))
  end

  @doc """
  Like `add_edge/3` or `add_edge/4`, but takes a `Graph.Edge` struct created with
  `Graph.Edge.new/2` or `Graph.Edge.new/3`.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(Graph.Edge.new(:a, :b))
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b}]
  """
  @spec add_edge(t, Edge.t) :: t
  def add_edge(%__MODULE__{} = g, %Edge{v1: v1, v2: v2} = edge) do
    add_edge(g, v1, v2, Edge.to_meta(edge))
  end

  @doc """
  Adds an edge connecting `a` to `b`. If either `a` or `b` do not exist in the graph,
  they are automatically added. Adding the same edge more than once does not create multiple edges,
  each edge is only ever stored once.

  Edges have a default weight of 1, and an empty (nil) label. You can change this by passing options
  to this function, as shown below.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(:a, :b)
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: nil, weight: 1}]

      iex> g = Graph.new |> Graph.add_edge(:a, :b, label: :foo, weight: 2)
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo, weight: 2}]
  """
  @spec add_edge(t, vertex, vertex) :: t
  @spec add_edge(t, vertex, vertex, Edge.edge_opts) :: t | {:error, {:invalid_edge_option, term}}
  def add_edge(%__MODULE__{} = g, a, b, opts \\ []) do
    %__MODULE__{in_edges: ie, out_edges: oe, edges_meta: es_meta, vertices: vs} = g =
      g |> add_vertex(a) |> add_vertex(b)

    a_id = Map.get(vs, a)
    b_id = Map.get(vs, b)
    out_neighbors =
      case Map.get(oe, a_id) do
        nil -> MapSet.new([b_id])
        ms  -> MapSet.put(ms, b_id)
      end
    in_neighbors =
      case Map.get(ie, b_id) do
        nil -> MapSet.new([a_id])
        ms  -> MapSet.put(ms, a_id)
      end
    meta = Edge.options_to_meta(opts)
    %__MODULE__{g |
      in_edges: Map.put(ie, b_id, in_neighbors),
      out_edges: Map.put(oe, a_id, out_neighbors),
      edges_meta: Map.put(es_meta, {a_id, b_id}, meta)
    }
  catch
    _, {:error, {:invalid_edge_option, _}} = err ->
      err
  end

  @doc """
  Like `add_edge/3`, but takes a list of `Graph.Edge` structs, and adds an edge to the graph for each pair.

  See the docs for `Graph.Edge.new/2` or `Graph.Edge.new/3` for more info.

  ## Examples

      iex> alias Graph.Edge
      ...> edges = [Edge.new(:a, :b), Edge.new(:b, :c, weight: 2)]
      ...> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edges(edges)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b}, %Graph.Edge{v1: :b, v2: :c, weight: 2}]

      iex> Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edges([:a, :b])
      {:error, {:invalid_edge, :a}}
  """
  @spec add_edges(t, [Edge.t]) :: t | {:error, {:invalid_edge, term}}
  def add_edges(%__MODULE__{} = g, es) when is_list(es) do
    Enum.reduce(es, g, fn
      %Edge{} = edge, acc ->
        add_edge(acc, edge)
      {v1, v2}, acc ->
        add_edge(acc, v1, v2)
      bad_edge, _acc ->
        throw {:error, {:invalid_edge, bad_edge}}
    end)
  catch
    :throw, {:error, {:invalid_edge, _}} = err ->
      err
  end

  @doc """
  Splits the edge between `v1` and `v2` by inserting a new vertex, `v3`, deleting
  the edge between `v1` and `v2`, and inserting an edge from `v1` to `v3` and from
  `v3` to `v2`.

  The two resulting edges from the split will share the same weight and label.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :c]) |> Graph.add_edge(:a, :c, weight: 2)
      ...> g = Graph.split_edge(g, :a, :c, :b)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, weight: 2}, %Graph.Edge{v1: :b, v2: :c, weight: 2}]
  """
  @spec split_edge(t, vertex, vertex, vertex) :: t | {:error, :no_such_edge}
  def split_edge(%__MODULE__{in_edges: ie, out_edges: oe, edges_meta: em} = g, v1, v2, v3) do
    with {:ok, v1_id}  <- Graph.Impl.find_vertex_id(g, v1),
         {:ok, v2_id}  <- Graph.Impl.find_vertex_id(g, v2),
         {:ok, v1_out} <- Graph.Impl.find_out_edges(g, v1_id),
         {:ok, v2_in}  <- Graph.Impl.find_in_edges(g, v2_id),
          true   <- MapSet.member?(v1_out, v2_id),
          meta   <- Map.get(em, {v1_id, v2_id}),
          v1_out <- MapSet.delete(v1_out, v2_id),
          v2_in  <- MapSet.delete(v2_in, v1_id) do
      %__MODULE__{g |
                  in_edges: Map.put(ie, v2_id, v2_in),
                  out_edges: Map.put(oe, v1_id, v1_out)}
      |> add_vertex(v3)
      |> add_edge(v1, v3, meta)
      |> add_edge(v3, v2, meta)
    else
      _ -> {:error, :no_such_edge}
    end
  end

  @doc """
  Updates the metadata (weight/label) for an edge using the provided options.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(:a, :b)
      ...> [%Graph.Edge{v1: :a, v2: :b, label: nil, weight: 1}] = Graph.edges(g)
      ...> %Graph{} = g = Graph.update_edge(g, :a, :b, weight: 2, label: :foo)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo, weight: 2}]
  """
  @spec update_edge(t, vertex, vertex, Edge.edge_opts) :: t | {:error, :no_such_edge}
  def update_edge(%__MODULE__{edges_meta: em} = g, v1, v2, opts) when is_list(opts) do
    with {:ok, v1_id} <- Graph.Impl.find_vertex_id(g, v1),
         {:ok, v2_id} <- Graph.Impl.find_vertex_id(g, v2),
          opts when is_map(opts) <- Edge.options_to_meta(opts) do
      case Map.get(em, {v1_id, v2_id}) do
        nil ->
          %__MODULE__{g | edges_meta: Map.put(em, {v1_id, v2_id}, opts)}
        meta ->
          %__MODULE__{g | edges_meta: Map.put(em, {v1_id, v2_id}, Map.merge(meta, opts))}
      end
    else
      _ -> g
    end
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
  def delete_edge(%__MODULE__{in_edges: ie, out_edges: oe, edges_meta: meta} = g, a, b) do
    with {:ok, a_id}  <- Graph.Impl.find_vertex_id(g, a),
         {:ok, b_id}  <- Graph.Impl.find_vertex_id(g, b),
         {:ok, a_out} <- Graph.Impl.find_out_edges(g, a_id),
         {:ok, b_in}  <- Graph.Impl.find_in_edges(g, b_id) do
      a_out = MapSet.delete(a_out, b_id)
      b_in  = MapSet.delete(b_in, a_id)
      meta  = Map.delete(meta, {a_id, b_id})
      %__MODULE__{g |
                  in_edges: Map.put(ie, b_id, b_in),
                  out_edges: Map.put(oe, a_id, a_out),
                  edges_meta: meta}
    else
      _ -> g
    end
  end

  @doc """
  Like `delete_edge/3`, but takes a list of vertex pairs, and deletes the corresponding
  edge from the graph, if it exists.

  ## Examples

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b)
      ...> g = Graph.delete_edges(g, [{:a, :b}])
      ...> Graph.edges(g)
      []

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b)
      ...> Graph.delete_edges(g, [:a])
      {:error, {:invalid_edge, :a}}
  """
  @spec delete_edges(t, [{vertex, vertex}]) :: t | {:error, {:invalid_edge, term}}
  def delete_edges(%__MODULE__{} = g, es) when is_list(es) do
    Enum.reduce(es, g, fn
      {v1, v2}, acc ->
        delete_edge(acc, v1, v2)
      bad_edge, _acc ->
        throw {:error, {:invalid_edge, bad_edge}}
    end)
  catch
    :throw, {:error, {:invalid_edge, _}} = err ->
      err
  end

  @doc """
  The transposition of a graph is another graph with the direction of all the edges reversed.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b) |> Graph.add_edge(:b, :c)
      ...> g |> Graph.transpose |> Graph.edges
      [%Graph.Edge{v1: :b, v2: :a}, %Graph.Edge{v1: :c, v2: :b}]
  """
  @spec transpose(t) :: t
  def transpose(%__MODULE__{in_edges: ie, out_edges: oe, edges_meta: es_meta} = g) do
    es_meta2 =
      es_meta
      |> Enum.reduce(%{}, fn {{v1, v2}, meta}, acc -> Map.put(acc, {v2, v1}, meta) end)
    %__MODULE__{g | in_edges: oe, out_edges: ie, edges_meta: es_meta2}
  end

  @doc """
  Returns a topological ordering of the vertices of graph `g`, if such an ordering exists, otherwise it returns false.
  For each vertex in the returned list, no out-neighbors occur earlier in the list.
  """
  @spec topsort(t) :: [vertex]
  defdelegate topsort(g), to: Graph.Impl, as: :topsort

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
  defdelegate components(g), to: Graph.Impl

  @doc """
  Returns a list of strongly connected components, where each component is a list of vertices.

  A *strongly connected component* is a maximal subgraph such that there is a path between each pair of vertices.

  See `components/1` for the definitions of *subgraph* and *maximal subgraph* if you are unfamiliar with the
  terminology.

  See the test suite for an example of how this is used.
  """
  @spec strong_components(t) :: [[vertex]]
  defdelegate strong_components(g), to: Graph.Impl

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path in the graph from some vertex of `vs` to `v`.

  As paths of length zero are allowed, the vertices of `vs` are also included in the returned list.
  """
  @spec reachable(t, [vertex]) :: [[vertex]]
  defdelegate reachable(g, vs), to: Graph.Impl

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path in the graph of length one or more from some vertex of `vs` to `v`.

  As a consequence, only those vertices of `vs` that are included in some cycle are returned.
  """
  @spec reachable_neighbors(t, [vertex]) :: [[vertex]]
  defdelegate reachable_neighbors(g, vs), to: Graph.Impl

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path from `v` to some vertex of `vs`.

  As paths of length zero are allowed, the vertices of `vs` are also included in the returned list.
  """
  @spec reaching(t, [vertex]) :: [[vertex]]
  defdelegate reaching(g, vs), to: Graph.Impl

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path of length one or more from `v` to some vertex of `vs`.

  As a consequence, only those vertices of `vs` that are included in some cycle are returned.
  """
  @spec reaching_neighbors(t, [vertex]) :: [[vertex]]
  defdelegate reaching_neighbors(g, vs), to: Graph.Impl

  @doc """
  Returns all vertices of graph `g`. The order is given by a depth-first traversal of the graph,
  collecting visited vertices in preorder.
  """
  @spec preorder(t) :: [vertex]
  defdelegate preorder(g), to: Graph.Impl

  @doc """
  Maps a function over all the vertices in a graph using a depth-first traversal

  You can perform a breadth-first traversal instead by passing the option `algorithm: :breadth_first`.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 4]) |> Graph.add_edge(1, 2) |> Graph.add_edge(2, 4)
      ...> Graph.map(g, fn v -> v * 2 end)
      [2, 4, 8]
  """
  @type mapper_fun :: (vertex -> term)
  @spec map(t, mapper_fun) :: [term]
  @spec map(t, mapper_fun, opts :: [walk_opt]) :: [term]
  def map(%__MODULE__{} = g, fun, opts \\ []) when is_function(fun, 1) do
    res = reduce(g, [], fn v, acc ->
      [fun.(v)|acc]
    end, opts)
    Enum.reverse(res)
  end

  @doc """
  Applies a reducer over all the vertices in a graph using a depth-first traversal.
  The reducer function receives the current vertex, and the accumulator, and must return
  a new accumulator.

  You can perform a breadth-first traversal instead by passing the option `algorithm: :breadth_first`.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 4]) |> Graph.add_edge(1, 2) |> Graph.add_edge(2, 4)
      ...> Graph.reduce(g, 0, fn v, acc -> acc + v end)
      7
  """
  @type reducer_fun :: (vertex, term -> term)
  @spec reduce(t, term, reducer_fun) :: term
  @spec reduce(t, term, reducer_fun, opts :: [walk_opt]) :: term
  def reduce(%__MODULE__{} = g, acc, fun, opts \\ []) when is_function(fun, 2) do
    walk(g, acc, fn v, _out, _in, acc ->
      {:next, fun.(v, acc)}
    end, opts)
  end

  @doc """
  Walks the graph by starting with a depth-first traversal, the walk function receives
  the current vertex, it's out-neighbors and in-neighbors (as lists of Edge structs), and the accumulator.
  You can return one of the following to control the walk:

  - `{:next, acc}`, continues the depth-first traversal, passing along the accumulator
  - `{:next, v, acc}`, continues the traversal from `v`, passing along the accumulator
  - `{:skip, acc}`, skips traversal of the current vertex's out-neighbors, passing along the accumulator
  - `{:halt, acc}`, stops the traversal, returning the accumulator

  You can use this function to implement your own traversals of the graph, and as a foundation for reducers of your
  own design.

  NOTE: If you take control over the direction of the traversal, take care that you handle
  cycles correctly, or you may end up following cycles indefinitely.

  ## Options

  `walk/3` can also be provided with an options list, the current options available are:

  - `algorithm: :breadth_first | :depth_first`, performs the traversal using the selected
    algorithm. The default is `:depth_first`.
  """
  @type walk_opt :: {:algorithm, :breadth_first}
  @type walker_fun :: (vertex, [vertex], [vertex], term -> {:next, term})
    | {:next, vertex, term}
    | {:skip, term}
    | {:halt, term}
  @spec walk(t, term, walker_fun) :: term
  @spec walk(t, term, walker_fun, opts :: [walk_opt]) :: term
  defdelegate walk(g, acc, fun, opts \\ []), to: Graph.Reducer

  @doc """
  Returns all vertices of graph `g`. The order is given by a depth-first traversal of the graph,
  collecting visited vertices in postorder. More precisely, the vertices visited while searching from an
  arbitrarily chosen vertex are collected in postorder, and all those collected vertices are placed before
  the subsequently visited vertices.
  """
  @spec postorder(t) :: [vertex]
  defdelegate postorder(g), to: Graph.Impl

  @doc """
  Returns a list of vertices from graph `g` which are included in a loop.
  """
  @spec loop_vertices(t) :: [vertex]
  defdelegate loop_vertices(g), to: Graph.Impl

  @doc """
  Returns the in-degree of vertex `v` of graph `g`.

  The *in-degree* of a vertex is the number of edges directed inbound towards that vertex.
  """
  def in_degree(%__MODULE__{} = g, v) do
    with {:ok, v_id} <- Graph.Impl.find_vertex_id(g, v),
         {:ok, v_in} <- Graph.Impl.find_in_edges(g, v_id) do
      MapSet.size(v_in)
    else
      _ -> 0
    end
  end

  @doc """
  Returns the out-degree of vertex `v` of graph `g`.

  The *out-degree* of a vertex is the number of edges directed outbound from that vertex.
  """
  @spec out_degree(t, vertex) :: non_neg_integer
  def out_degree(%__MODULE__{} = g, v) do
    with {:ok, v_id}  <- Graph.Impl.find_vertex_id(g, v),
         {:ok, v_out} <- Graph.Impl.find_out_edges(g, v_id) do
      MapSet.size(v_out)
    else
      _ -> 0
    end
  end

  @doc """
  Returns a list of vertices which all have edges coming in to the given vertex `v`.
  """
  @spec in_neighbors(t, vertex) :: [vertex]
  def in_neighbors(%Graph{ids: ids} = g, v) do
    with {:ok, v_id} <- Graph.Impl.find_vertex_id(g, v),
         {:ok, v_in} <- Graph.Impl.find_in_edges(g, v_id) do
      Enum.map(v_in, &Map.get(ids, &1))
    else
      _ -> []
    end
  end

  @doc """
  Returns a list of `Graph.Edge` structs representing the in edges to vertex `v`.
  """
  @spec in_edges(t, vertex) :: Edge.t
  def in_edges(%__MODULE__{ids: ids, edges_meta: em} = g, v) do
    with {:ok, v_id} <- Graph.Impl.find_vertex_id(g, v),
         {:ok, v_in} <- Graph.Impl.find_in_edges(g, v_id) do
      Enum.map(v_in, fn id ->
        v2 = Map.get(ids, id)
        meta = Map.get(em, {id, v_id}, [])
        Edge.new(v2, v, meta)
      end)
    else
      _ -> []
    end
  end

  @doc """
  Returns a list of vertices which the given vertex `v` has edges going to.
  """
  @spec out_neighbors(t, vertex) :: [vertex]
  def out_neighbors(%__MODULE__{ids: ids} = g, v) do
    with {:ok, v_id} <- Graph.Impl.find_vertex_id(g, v),
         {:ok, v_out} <- Graph.Impl.find_out_edges(g, v_id) do
      Enum.map(v_out, &Map.get(ids, &1))
    else
      _ -> []
    end
  end

  @doc """
  Returns a list of `Graph.Edge` structs representing the out edges from vertex `v`.
  """
  @spec out_edges(t, vertex) :: Edge.t
  def out_edges(%__MODULE__{ids: ids, edges_meta: es_meta} = g, v) do
    with {:ok, v_id} <- Graph.Impl.find_vertex_id(g, v),
         {:ok, v_out} <- Graph.Impl.find_out_edges(g, v_id) do
      Enum.map(v_out, fn id ->
        v2 = Map.get(ids, id)
        meta = Map.get(es_meta, {v_id, id}, [])
        Edge.new(v, v2, meta)
      end)
    else
      _ -> []
    end
  end

  @doc """
  Builds a maximal subgraph of `g` which includes all of the vertices in `vs` and the edges which connect them.

  See the test suite for example usage.
  """
  @spec subgraph(t, [vertex]) :: t
  def subgraph(%__MODULE__{vertices: vertices, ids: ids, out_edges: oe, edges_meta: es_meta}, vs) do
    allowed =
      vs
      |> Enum.map(&Map.get(vertices, &1))
      |> Enum.reject(&is_nil/1)
      |> MapSet.new

    Enum.reduce(allowed, Graph.new, fn v_id, sg ->
      v = Map.get(ids, v_id)
      sg = Graph.add_vertex(sg, v)
      oe
      |> Map.get(v_id, MapSet.new)
      |> MapSet.intersection(allowed)
      |> Enum.reduce(sg, fn v2_id, sg ->
        v2 = Map.get(ids, v2_id)
        meta = Map.get(es_meta, {v_id, v2_id})
        Graph.add_edge(sg, v, v2, meta)
      end)
    end)
  end
end
