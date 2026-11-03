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
