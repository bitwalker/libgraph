# libgraph

[![Main](https://github.com/bitwalker/libgraph/actions/workflows/elixir.yml/badge.svg?branch=main)](https://github.com/bitwalker/libgraph/actions/workflows/elixir.yml)
[![Hex.pm Version](http://img.shields.io/hexpm/v/libgraph.svg?style=flat)](https://hex.pm/packages/libgraph)

[Documentation](https://hexdocs.pm/libgraph)

## About

This library provides:

- An implementation of a graph datastructure, `Graph`, designed for both directed and undirected graphs. The API supports
  undirected graphs, but I'm still getting the tests updated to cover properties of undirected graphs.
- A priority queue implementation `PriorityQueue`, oriented towards graphs (it prioritizes lower integer values over high),
  it is the fastest priority queue I know of which allows arbitrary priorities, and is more or less at parity with
  `pqueue3` from [the pqueue library](https://github.com/okeuday/pqueue/), which supports priorities from 0 to 65535.
- An idiomatic Elixir API for creating, modifying, and querying its graph structure. Creating and modifying a graph
  can be done in a single pipeline, and all queries take a Graph as their first parameter (one of my complaints with `:digraph`
  is that there is some inconsistency with the API between `:digraph` and `:digraph_utils` for no apparent reason).
- Two "Reducer" implementations for mapping/reducing over a graph. I am trying to figure out the best way to make these
extendable and part of the API, so that you can drop in your own shortest path algorithms, etc - but I have yet to come up with an
approach that feels good on that front.
- A `Serializer` behaviour, for defining custom serialization of graphs, with a Graphviz DOT format serializer
  provided out of the box.

It is backed by a large suite of tests, including StreamData properties for the graph model. Its
API shares some similarity with `:digraph`, but diverges in favor of a more idiomatic Elixir interface. In
addition, over time I'm adding new functions to query the graph in ways not previously supported via `:digraph`,
and introducing support for classifying a graph as undirected if so desired, so that queries over such graphs
become easier.

If you are interested in reading more about how you can make use of `libgraph`, 
there is an [excellent blog post](https://medium.com/@tonyhammond/native-graph-data-in-elixir-8c0bb325d451) written by Tony Hammond
which is a very helpful walkthrough of the library and what can be built with it.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `libgraph` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:libgraph, "~> 0.16.0"}]
end
```

## Vertex identity

By default, libgraph identifies vertices using a 32-bit hash. Hash collisions can
silently treat distinct vertices as one, including their incident edges. To identify
vertices by their exact values instead:

```elixir
graph = Graph.new(vertex_identifier: fn vertex -> vertex end)
```

For structured vertices, you can instead supply a stable unique application ID,
such as `fn %{id: id} -> id end`. Identifiers are exact map keys: `1` and `1.0`
remain distinct. Adding a value with an existing identifier keeps the first stored
vertex and its labels. Subgraphs and extracted cores preserve the callback.

Undirected edges order their endpoints by vertex value, so use consistent vertex
values for each logical identifier when adding and querying edges. DOT and Mermaid
serializers currently use identifiers directly as node names; tuple or map IDs
require a custom serializer or serialization-friendly IDs.

Compare build time, lookup time, retained vertex counts, and worker memory after GC for
hashed and identity keys with `mix run bench/vertex_identity.exs`. The benchmark
includes reused and reconstructed structured endpoints; workload-specific sharing
can materially affect memory use. See `Graph.new/1` for the identifier contract.

## Parallel edges

Edge identity includes both endpoints and the label. Different labels distinguish
parallel edges; adding the same endpoints and label again updates its weight:

```elixir
graph =
  Graph.new()
  |> Graph.add_edge(:a, :b, label: :reads)
  |> Graph.add_edge(:a, :b, label: :writes)
  |> Graph.add_edge(:a, :b, label: :reads, weight: 5)

Graph.num_edges(graph) # 2
Graph.neighbors(graph, :a) # [:b]
```

Edge enumeration and degrees count labeled edges; neighbor lists contain each
neighbor once. Undirected endpoint reversal addresses the same labeled edge.
There are no separate edge IDs for multiple edges with identical labels.

DOT export preserves parallel edges by omitting `strict` when needed. Labels are
display text, not arbitrary Graphviz attributes: a label containing a color value
does not set edge color. Styling requires a custom serializer or editing the DOT.

## Bulk vertex labels

Reduce `Graph.add_vertex/3` over records that you explicitly interpret as vertex
and labels:

```elixir
labeled = [{:a, [:start]}, {:b, [:finish]}]

graph = Enum.reduce(labeled, Graph.new(), fn {vertex, labels}, graph ->
  Graph.add_vertex(graph, vertex, labels)
end)
```

Use the same reduction with a common label for every vertex if needed. Empty input
keeps the initial graph; duplicate identifiers keep the first vertex and labels.
`Graph.add_vertices/2` continues to treat every item as a vertex, so tuples and
lists are never implicitly interpreted as labeling instructions.

## Multigraphs

Libgraph supports multigraphs — graphs where multiple edges with different labels can exist between
the same pair of vertices. When `multigraph: true` is enabled, an edge adjacency index is maintained
that lets partition queries start from matching endpoint pairs, avoiding scans of unrelated
edges. Query cost depends on the selected pairs and their parallel labels. The index uses
additional memory and adds maintenance work to edge mutations.

### Creating a multigraph

```elixir
g =
  Graph.new(multigraph: true)
  |> Graph.add_edges([
    {:a, :b, label: :uses},
    {:a, :b, label: :contains},
    {:b, :c, label: :uses},
    {:b, :c, label: :owns, weight: 3}
  ])
```

### Querying by partition

By default, edges are partitioned by their label (via `Graph.Utils.by_edge_label/1`). You can
query edges belonging to a specific partition:

```elixir
# Get only :uses edges
Graph.edges(g, by: :uses)
#=> [%Graph.Edge{v1: :a, v2: :b, label: :uses}, %Graph.Edge{v1: :b, v2: :c, label: :uses}]

# Get out edges from :a with label :contains
Graph.out_edges(g, :a, by: :contains)
#=> [%Graph.Edge{v1: :a, v2: :b, label: :contains}]

# Filter edges with a predicate
Graph.edges(g, where: fn edge -> edge.weight > 2 end)
#=> [%Graph.Edge{v1: :b, v2: :c, label: :owns, weight: 3}]
```

### Custom partition functions

You can provide a custom `partition_by` function to control how edges are indexed:

```elixir
g = Graph.new(multigraph: true, partition_by: fn edge -> [edge.weight] end)
|> Graph.add_edges([{:a, :b, weight: 1}, {:b, :c, weight: 2}])

Graph.edges(g, by: 1)
#=> [%Graph.Edge{v1: :a, v2: :b, weight: 1}]
```

### Partition-filtered traversals

BFS, DFS, Dijkstra, A*, and Bellman-Ford all support a `by:` option to restrict traversal to
edges in specific partitions:

```elixir
g =
  Graph.new(multigraph: true)
  |> Graph.add_edges([
    {:a, :b, label: :fast, weight: 1},
    {:a, :c, label: :slow, weight: 10},
    {:b, :d, label: :fast, weight: 1},
    {:c, :d, label: :slow, weight: 1}
  ])

# Shortest path using only :fast edges
Graph.dijkstra(g, :a, :d, by: :fast)
#=> [:a, :b, :d]

# BFS following only :fast edges
Graph.Reducers.Bfs.map(g, & &1, by: :fast)
#=> [:a, :b, :d]
```

### Edge properties

Edges support an arbitrary `properties` map for storing additional metadata. The existing
`%{label => weight}` edge records are retained; properties are stored separately in
`edge_properties` only when populated:

```elixir
g = Graph.new()
|> Graph.add_edge(:a, :b, label: :link, properties: %{color: "red", style: :dashed})

[edge] = Graph.edges(g)
edge.properties
#=> %{color: "red", style: :dashed}
```

## Rationale

The original motivation for me to start working on this library is the fact that `:digraph` requires a
minimum of 3 ETS tables per graph, and up to 6 depending on the operations you are performing on the graph.
If you are working with a lot of graphs concurrently, as I am, this means you can find yourself in a situation
where you hit the system limit for the maximum number of ETS table, and bring your system down. Seeing as how
it is ridiculous that trying to use a simple graph could potentially kill my system, and not wanting to hack
around the problem, I decided to see if I could build an alternative which was competitive performance-wise,
without requiring any ETS tables at all.

The result turned out better than I hoped - it is possible to build a graph datastructure without ETS that
is both equally performant (and in many of my benchmarks, better performing), and supports all of the same
functionality.

Additionally, I also had a few other things I wanted to address:

- Inconsistency with argument order in the API between `:digraph` and `:digraph_utils`
- The fact that there are two modules to work with the same datastructure to begin with, and trying to remember
  what lives where.
- The lack of extensibility, for example, there is no API with which you can implement your own
  traversal algorithms. This means you are stuck with whatever way the Erlang maintainers decided was
  ideal, regardless of whether it suits your use case or not. A great example is single-source shortest path
  algorithms, where you may want a simple breadth-first search, or perhaps you want to use Dijkstra's algorithm -
  you are stuck with just one approach with `:digraph`, which as I understand it, is a breadth-first search.
- `:digraph` as the name implies, only supports directed graphs
- `:digraph` graphs are unweighted, with no way to supported weighted graphs
- `:digraph` graphs are not "inspect-friendly", you get a tuple with the underlying ETS table ids, but that's it,
  not necessarily a big deal, but it's nice for playing around in the shell if you can see how your code affects the
  structure.
  
My speculation as to why `:digraph` is the way it is, is that when `:digraph` was originally written, there was
no efficient key/value datastructure in Erlang that could support large numbers of keys. At that time, maps
weren't even a speck in the eye of the language maintainers. Even after the initial introduction of maps in OTP 18,
maps still weren't efficient enough to work with large numbers of keys. It wasn't until OTP 19 that the performance
of maps with millions of keys became reasonable. So, it's not that `:digraph` sucks - it was the best possible implementation
at the time; but now that the language has come so far, we can take advantage of some of the new hotness and reinvent
it from the ground up :).

## Benchmarks

Feel free to take a look under the `bench` folder in the project root. There a few benchmarks I threw together to
keep an eye on a few key areas I wanted to ensure parity with `:digraph` on. You can run them yourself as well, but
I would encourage you to use them as a template to construct a benchmark based on your own use case, and compare them
that way, as it will give you a better basis to make your decision on. However, if you do find that `libgraph` is behind
`:digraph` with a benchmark, please let me know so that I can improve the library!

NOTE: While this library is primarily focused on the `Graph` data structure it defines, it also contains an implementation
of a priority queue (you can find it under the `PriorityQueue` module), designed for use with graphs specifically, as it
considers lower integer values higher priority, which is perfect for the kinds of graph algorithms you need a priority queue for.

## Diagnosing path searches

`Graph.get_paths/3` eagerly enumerates paths. A small layered graph can have an
exponential number of paths, and an unreachable target can still require extensive
exploration. Applying `Enum.take/2` to the returned list does not bound that work.
Use `Graph.get_shortest_path/3` or `Graph.dijkstra/3` when one shortest path suffices.

Run `mix run bench/all_paths.exs` to compare chains, branching trees, cycles,
layered DAGs and disconnected targets. The fixed fixtures have a ten-second search
deadline and report vertex/edge counts, maximum reachable depth, result count,
time, reductions and process memory. Sampled memory peaks are lower bounds;
process memory is not RSS or total off-heap binary storage.

## Diagnosing label memory

Keep only the latest graph when replacing labels unless historical snapshots are
intentional. `Graph.label_vertex/3` appends labels; use `remove_vertex_labels/2`
before labeling when replacing the complete list. Old graph values, queued
messages, or references elsewhere in the application can retain earlier labels.

Run `mix run bench/label_memory.exs` to compare direct and minimal GenServer label
updates, owned binaries, sub-binaries, and deliberately retained graph history.
It reports graph/label counts, mailbox size, process memory and referenced binary
storage before and after diagnostic garbage collection. Checkpoint samples are
not peak allocation or RSS, and process memory excludes off-heap binary storage.

A small sub-binary can retain a much larger parent; compare `byte_size/1` with
`:binary.referenced_byte_size/1`. Copy a value at the caller boundary with
`:binary.copy/1` when measurements demonstrate unwanted parent retention. Forced
GC is useful as a diagnostic control, not a general label-update policy. The
isolated benchmark cannot rule out retention in application wrappers or callers.

## Contributing

The library supports Elixir 1.10 and later. CI checks the 1.10/OTP 23 floor in a digest-pinned container and representative Elixir/OTP pairs through the current release. Runtime versions and image digests are recorded in `.github/workflows/elixir.yml`.

Use [mise](https://mise.jdx.dev/) to select a compatible local pair. For the canonical development checks:

```sh
mise exec erlang@27.3.4.19 elixir@1.18.4-otp-27 -- mix local.hex --force
mise exec erlang@27.3.4.19 elixir@1.18.4-otp-27 -- mix deps.get --check-locked
mise exec erlang@27.3.4.19 elixir@1.18.4-otp-27 -- mix test
```

Tests include StreamData properties; no proprietary QuickCheck installation is needed. Run `mix test --cover` for built-in coverage. On the canonical toolchain, also run `mix format --check-formatted`, `mix compile --warnings-as-errors`, `mix dialyzer`, `mix docs`, and `mix hex.build --unpack`. Generated packages are for local inspection; publishing remains a manual maintainer action.

Workflow changes must pass actionlint, zizmor, and action-lock verification. Actions use full commit SHAs with version comments, and `.github/workflows/actions.lock` locks their provenance and transitive dependencies. After reviewing an action update, run:

```sh
gh extension install github/gh-actions-lock --pin v0.4.0
gh actions-lock --no-narrow
gh actions-lock --verify --no-narrow
actionlint
zizmor .github
```

Review the workflow and generated lockfile together; do not edit the lockfile by hand. Dependabot proposes weekly action updates after a seven-day cooldown. Runtime/tool downloads and Docker images are outside action locking; pin images by digest and verify directly downloaded legacy tooling. CI runs fork PRs with read-only permissions and no publication credentials.

If you have changes in mind that are significant or potentially time consuming, please open a RFC-style PR first, where we
can discuss your plans first. I don't want you to spend all your time crafting a PR that I ultimately reject because I don't
think it's a good fit or is too large for me to review. Not that I plan to reject PRs in general, but I have to be careful to
balance features with maintenance burden, or I will quickly be unable to manage the project.

Please ensure that you adhere to a commit style where logically related changes are in a single commit, or broken up in a way that
eases review if necessary. Keep commit subject lines informative, but short, and provide additional detail in the extended message text
if needed. If you can, mention relevant issue numbers in either the subject or the extended message.

## Roadmap

Please open an issue if you have a feature request!

## License

MIT (See the LICENSE file)
