defmodule Graph.Tree do
  defmodule Node do
    @moduledoc """
    A container to hold arbitrary data within a tree.
    An implementation detail: Every node is given a unique identifier so that 
    duplicate data (for example, two nodes that have data as 4) will not be considered identical nodes.
    """
    defstruct id: nil, data: nil
    @type t :: %__MODULE__{}

    def init(data), do: %__MODULE__{id: make_ref(), data: data}
  end

  @moduledoc """
  An abstraction over a graph, this tree implementation can have an arbitrary number of children.
  This module can be used as a base for more complex data transformations on data structures that
  if you squint hard enough can look like trees (e.g: maps)
  """
  alias __MODULE__.Node

  @doc """
  A stuct representing a Tree. Under the hood, it uses a graph to store the nodes and edges, as a tree is a 
  special type of graph. Also has the option to define if the tree was created using a `virtual_root` which is
  just a word I invented. But to explain it:

  A lot of data structures don't quite follow a tree-like structure. Take this map:
  %{"foo" => "bar", "baz" => "bux"}

  If we wanted to process this map as a tree, we would have to add some top-level key to it such that it can act as the
  root. Like this:
  %{"root" => %{
      "foo" => "bar",
      "baz" => "bux"
    }
  }

  That's kind of a pain in the tuckus if you have to do it a lot, plus you have to strip off the root when you're done with
  processing to get back to your original data structure. A *`virtual_root`* provides this top-level key behind the scenes
  so you can kind of coerce your data structure into a tree, but the guarantee provided by this module is that the virtual root
  will not be included in any processing/iterating/reducing/etc. So you can have all the benefits of using this module without 
  worrying too much about if you have a well-formed tree when you pass the data into a constructor for a `Tree.t()`
  """
  defstruct root: nil, graph: nil, has_virtual_root?: false
  @type t :: %__MODULE__{}
  @typedoc """
  A keyword of atoms that form a tree, e.g: [a: [b: [:c, :d]]].
  For the purposes of great humor, I have named this subset of keyword lists
  treeword lists.
  """
  @type treeword :: keyword(treeword()) | list(atom())
  @doc """
  A quick check to see if something might be a treeword. Should not be relied upon over-much, as it does not
  fully check to see if something is a proper treeword. This guard should just be used as a hueristic to make sure
  the input is not a completely different type
  """
  # You can't pattern match inside guards, so here's what this is supposed to mean:
  # [{key, _value}] when is_atom(key)
  defguard maybe_treeword?(list)
           when is_list(list) and is_tuple(hd(list)) and is_atom(elem(hd(list), 0)) and
                  tl(list) == []

  @doc """
  Takes a treeword list.
  Returns a tree with atoms as data and there is a parent-child relationship 
  between atom key and sublist.
  """
  @spec from_treeword!(treeword() | keyword(atom()), opts :: keyword()) :: t() | no_return()
  def from_treeword!(_, opts \\ [virtual_root: false])

  def from_treeword!(keyword, [virtual_root: true] = opts) do
    parent = Node.init(:root)
    initializer = &do_keyword_init(&1, parent, keyword)
    do_init(parent, initializer, opts)
  end

  def from_treeword!([{key, value}], [virtual_root: false] = opts) do
    parent = Node.init(key)
    initializer = &do_keyword_init(&1, parent, value)
    do_init(parent, initializer, opts)
  end

  def from_treeword!(_, _) do
    raise ArgumentError, """
    Treewords can only have one element as the root and must
    be comprised of only atoms as the leaves
    """
  end

  defp do_init(parent, initializer, opts) do
    Graph.new()
    |> Graph.add_vertex(parent)
    |> initializer.()
    |> then(&%__MODULE__{root: parent, graph: &1, has_virtual_root?: opts[:virtual_root]})
  end

  defp do_keyword_init(graph, parent, {key, rest}) do
    child = Node.init(key)

    graph
    |> Graph.add_edge(parent, child)
    |> do_keyword_init(child, rest)
  end

  defp do_keyword_init(graph, parent, children) when is_list(children) do
    Enum.reduce(children, graph, fn child, acc -> do_keyword_init(acc, parent, child) end)
  end

  defp do_keyword_init(graph, parent, child_data) when is_atom(child_data) do
    Graph.add_edge(graph, parent, Node.init(child_data))
  end

  defp do_keyword_init(_graph, _parent, _child_data) do
    raise ArgumentError, "Treewords can only contain atoms as leaves"
  end

  @doc """
  Given a node N and tree T, returns true if N is a leaf of T and false otherwise.
  """
  @spec is_leaf?(t(), Node.t()) :: boolean()
  def is_leaf?(%__MODULE__{} = t, %__MODULE__.Node{} = l) do
    Graph.out_degree(t.graph, l) == 0
  end

  @doc """
  Iterates through all of a tree's nodes. For exemplar node (A):
  - (A) is guaranteed to be processed before its children
  - (A)'s children will be processed before its siblings are processed
  - (A)'s children will be processed in an arbitraty order
  This is similar to a DFS of the tree, except the nodes visited along the way to the leaves are processed before the leaf nodes.

  The reducer is a little different than most reducers you might see. The first argument is the current Node.t() being processed.
  The second argument is a list of the ancestors of the current node. The immediate parent is the first element of this list. The
  root of the tree is the last element. This format forms a stack. The third argument is the accumulator value. The reducer must return {:next | acc} to continue
  or {:halt, acc} to stop processing.
  """
  @spec reduce(
          tree :: t(),
          acc :: term(),
          reducer ::
            (Node.t(), path_stack :: [], acc :: term() ->
               {:next, acc :: term()} | {:halt, acc :: term()})
        ) :: term()
  def reduce(%__MODULE__{has_virtual_root?: false} = t, initial_acc, reducer) do
    t.graph
    |> reduce(t.root, {:next, initial_acc}, [], reducer)
    |> elem(1)
  end

  def reduce(%__MODULE__{has_virtual_root?: true} = t, initial_acc, reducer) do
    t.graph
    |> Graph.out_neighbors(t.root)
    |> Enum.reduce({:next, initial_acc}, fn %Node{} = n, acc ->
      reduce(t.graph, n, acc, [], reducer)
    end)
    |> elem(1)
  end

  defp reduce(%Graph{} = g, %Node{} = current_node, {:next, acc}, path_stack, reducer) do
    case reducer.(current_node, path_stack, acc) do
      {:next, _new_acc} = res ->
        g
        |> Graph.out_neighbors(current_node)
        |> Enum.reduce(res, fn %Node{} = n, acc ->
          reduce(g, n, acc, [current_node | path_stack], reducer)
        end)

      {:halt, _new_acc} = res ->
        res
    end
  end

  defp reduce(%Graph{}, %Node{}, {:halt, acc}, _path_stack, _reducer), do: {:halt, acc}

  @doc """
  A convience function to do a reduce where the reducer is only called when 
  traversing over a leaf node.
  """
  @spec leaf_reduce(
          t(),
          acc :: term(),
          (Node.t(), path_stack :: list(), acc :: term() ->
             {:next, acc :: term()} | {:halt, acc :: term()})
        ) :: term()
  def leaf_reduce(%__MODULE__{} = t, initial_acc, reducer) do
    modified_reducer = fn node, path_stack, acc ->
      if is_leaf?(t, node) do
        reducer.(node, path_stack, acc)
      else
        {:next, acc}
      end
    end

    reduce(t, initial_acc, modified_reducer)
  end

  @doc """
  Map each path to a leaf to something else using `mapper` 
  """
  @spec leaf_map(
          t(),
          (current_path :: list(), mapper :: list() -> term())
        ) :: term()
  def leaf_map(%__MODULE__{} = t, mapper) do
    reducer = fn node, path_stack, acc ->
      current_path = current_path(node, path_stack)
      res = mapper.(current_path)
      {:next, [res | acc]}
    end

    leaf_reduce(t, [], reducer)
  end

  @doc """
  A convience function. Given a node that's currently being visited and a path_stack,
  this function will return a list of the current path (starting with the root and
  ending with the current element). Further, it will strip the `Node`s in the path
  to just their data elements. This can be useful when used in conjunction with 
  reduce and treeword lists do use things like get_in.
  """
  def current_path(%Node{} = n, path_stack) do
    [n | path_stack]
    |> Enum.map(& &1.data)
    |> Enum.reverse()
  end
end
