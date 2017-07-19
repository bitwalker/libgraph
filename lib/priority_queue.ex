defmodule PriorityQueue do
  @moduledoc """
  This module defines a priority queue datastructure, intended for use with graphs, as it prioritizes
  lower priority values over higher priority values (ideal for priorities based on edge weights, etc.).

  This implementation makes use of `:gb_trees` under the covers. It is also very fast, even for a very large
  number of distinct priorities. Other priority queue implementations I've looked at are either slow when working
  with large numbers of priorities, or restrict themselves to a specific number of allowed priorities, which is
  why I've ended up writing my own.
  """
  defstruct priorities: nil

  @opaque t :: %__MODULE__{}

  @doc """
  Create a new priority queue
  """
  @spec new() :: t
  def new do
    %__MODULE__{}
  end

  @doc """
  Push a new element into the queue with the given priority.

  Priorities must be integer values.

  ## Example

      iex> pq = PriorityQueue.new
      ...> pq = PriorityQueue.push(pq, :foo, 1)
      ...> {result, _} = PriorityQueue.pop(pq)
      ...> result
      {:value, :foo}

      iex> pq = PriorityQueue.new
      ...> pq = PriorityQueue.push(pq, :foo, 1)
      ...> {{:value, :foo}, pq} = PriorityQueue.pop(pq)
      ...> pq = PriorityQueue.push(pq, :bar, 1)
      ...> {result, _} = PriorityQueue.pop(pq)
      ...> result
      {:value, :bar}
  """
  @spec push(t, term, integer) :: t
  def push(%__MODULE__{priorities: {size, _} = tree} = pq, term, priority) when is_integer(priority) and size > 0 do
    case :gb_trees.lookup(priority, tree) do
      :none ->
        %__MODULE__{pq | priorities: :gb_trees.insert(priority, :queue.in(term, :queue.new), tree)}
      {:value, q} ->
        %__MODULE__{pq | priorities: :gb_trees.update(priority, :queue.in(term, q), tree)}
    end
  end
  def push(%__MODULE__{priorities: {0, _} = tree} = pq, term, priority) when is_integer(priority) do
    %__MODULE__{pq | priorities: :gb_trees.insert(priority, :queue.in(term, :queue.new), tree)}
  end
  def push(%__MODULE__{priorities: nil} = pq, term, priority) when is_integer(priority) do
    %__MODULE__{pq | priorities: :gb_trees.insert(priority, :queue.in(term, :queue.new), :gb_trees.empty)}
  end

  @doc """
  This function returns the value at the top of the queue. If the queue is empty, `:empty`
  is returned, otherwise `{:value, term}`. This function does not modify the queue.

  ## Example

      iex> pq = PriorityQueue.new |> PriorityQueue.push(:foo, 1)
      ...> {:value, :foo} = PriorityQueue.peek(pq)
      ...> {{:value, val}, _} = PriorityQueue.pop(pq)
      ...> val
      :foo
  """
  @spec peek(t) :: :empty | {:value, term}
  def peek(%__MODULE__{priorities: {size, {_min_pri, q, _, _}}}) when size > 0 do
    case :queue.out(q) do
      {:empty, _} -> :empty
      {{:value, _} = val, _} -> val
    end
  end
  def peek(%__MODULE__{priorities: _}) do
    :empty
  end

  @doc """
  Pops an element from the queue with the lowest integer value priority.

  Returns `{:empty, PriorityQueue.t}` if there are no elements left to dequeue.

  Returns `{{:value, term}, PriorityQueue.t}` if the dequeue is successful

  This is equivalent to the `extract-min` operation described in priority queue theory.

  ## Example

      iex> pq = PriorityQueue.new
      ...> pq = Enum.reduce(Enum.shuffle(0..4), pq, fn i, pq -> PriorityQueue.push(pq, ?a+i, i) end)
      ...> {{:value, ?a}, pq} = PriorityQueue.pop(pq)
      ...> {{:value, ?b}, pq} = PriorityQueue.pop(pq)
      ...> {{:value, ?c}, pq} = PriorityQueue.pop(pq)
      ...> {{:value, ?d}, pq} = PriorityQueue.pop(pq)
      ...> {{:value, ?e}, pq} = PriorityQueue.pop(pq)
      ...> {result, _} = PriorityQueue.pop(pq)
      ...> result
      :empty
  """
  @spec pop(t) :: {:empty, t} | {{:value, term}, t}
  def pop(%__MODULE__{priorities: {size, _} = tree} = pq) when size > 0 do
    {min_pri, q, tree2} = :gb_trees.take_smallest(tree)
    case :queue.out(q) do
      {:empty, _} ->
        pop(%__MODULE__{pq | priorities: tree2})
      {{:value, _} = val, q2} ->
        {val, %__MODULE__{pq | priorities: :gb_trees.update(min_pri, q2, tree)}}
    end
  end
  def pop(%__MODULE__{priorities: _} = pq) do
    {:empty, pq}
  end

  defimpl Inspect do
    def inspect(%PriorityQueue{priorities: {size, _} = tree}, opts) when size > 0 do
      items =
        tree
        |> :gb_trees.to_list
        |> Enum.flat_map(fn {_priority, q} -> :queue.to_list(q) end)
      count =
        Enum.count(items)
      doc =
        Inspect.Algebra.to_doc(items, opts)
      Inspect.Algebra.concat(["#PriorityQueue<size: #{count}, queue: ", doc, ">"])
    end
    def inspect(%PriorityQueue{}, _opts) do
      "#PriorityQueue<size: 0, queue: []>"
    end
  end
end
