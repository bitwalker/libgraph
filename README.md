# libgraph

[![Master](https://travis-ci.org/bitwalker/libgraph.svg?branch=master)](https://travis-ci.org/bitwalker/libgraph)
[![Hex.pm Version](http://img.shields.io/hexpm/v/libgraph.svg?style=flat)](https://hex.pm/packages/libgraph)

[Documentation](https://hexdocs.pm/libgraph)

## About

This library provides an alternative directed graph implementation (i.e. replaces `:digraph`), a priority
queue implementation (designed for use with graphs, as it prioritizes low integer values over high), and
eventually an undirected graph implementation.

I created this library because one of my current projects has a need for doing processing on large
numbers of graphs concurrently. As `:digraph` requires a minimum of 3 ETS tables per graph, and up to 6
depending on what queries you are running on it, this means you can very easily hit the max ETS tables
system limit and crash a node. So, the need for a non-ETS backed graph structure became evident.

I'm not attempting to make an API that matches `:digraph`, though they will be similar for obvious reasons.
I'm likely also only going to implement the operations that I feel are common or that I need myself, but feel
free to open an issue if there is something missing that you would like to have, and I'll do my best to get
it implemented if I feel it belongs here.

If you are curious, there is a test suite, including QuickCheck-style tests, and some benchmarks which compare
some common scenarios with both `:digraph` and `libgraph`. So far, I've been able to get `libgraph` to outperform
`:digraph`, but the benchmarks are by no means comprehensive, so I would recommend doing some of your own which
reflect your use case. That said, *please* let me know if you find any pathological performance issues with this
library, as I would like to ensure that as much as possible, `libgraph` either matches the performance of `:digraph`,
or exceeds it.

NOTE: While this library is primarily focused on the `Graph` data structure it defines, it also contains an implementation
of a priority queue (you can find it under the `PriorityQueue` module), designed for use with graphs specifically, as it
considers lower integer values higher priority, which is perfect for the kinds of graph algorithms you need a priority queue for.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `libgraph` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:libgraph, "~> 0.5"}]
end
```

## Roadmap

The following are items I plan to implement soon - they are not in any particular order.

- [ ] Support for undirected graphs

## License

MIT (See the LICENSE file)
