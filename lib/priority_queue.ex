defmodule PriorityQueue do
  @moduledoc """
  This module defines a priority queue datastructure, which uses a min heap structure to support pulling
  the values with the lowest priority out first. This is optimized for use with graph search algorithms where
  the smallest in/out degree or lowest edge weight/cost should be evaluated before those with higher values.
  Values with the same priority are dequeued in the order they were originally queued.

  This implementation exploits the fact that tuple access times are extremely fast, by storing priorities as
  buckets of `{priority, :queue.t()}`, and nesting them such that the lowest priority is always on the left,
  e.g. `{{1, :queue.t()}, {{3, :queue.t()}, nil}}`. We use `nil` to mark that there have been no priorities defined
  greater than the one on the left, and is where we insert new largest priorities. Inserting a new priority in the
  middle is just a matter of recursively navigating the heap until we reach the tuple where the left hand is less than
  the priority we're inserting, and the right hand is greater, and creating a new nested tuple on the right.
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
  """
  @spec push(t, term, integer) :: t
  def push(%__MODULE__{priorities: nil} = q, term, priority) when is_integer(priority) do
    %__MODULE__{q | priorities: {new_priority(priority, term), nil}}
  end
  # Optimize single priority
  def push(%__MODULE__{priorities: {{min_pri, pq}, nil}} = q, term, min_pri) when is_integer(min_pri) do
    %__MODULE__{q | priorities: {{min_pri, :queue.in(term, pq)}, nil}}
  end
  def push(%__MODULE__{priorities: ps} = q, term, priority) when is_integer(priority) and is_tuple(ps) do
    %__MODULE__{q | priorities: add_to_priority(ps, priority, term)}
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
  def pop(%__MODULE__{priorities: nil} = q) do
    {:empty, q}
  end
  def pop(%__MODULE__{priorities: ps} = q) when is_tuple(ps) do
    case extract_min(ps) do
      {:empty, ps1} ->
        {:empty, %__MODULE__{q | priorities: ps1}}
      {{:value, _} = val, ps1} ->
        {val, %__MODULE__{q | priorities: ps1}}
    end
  end

  ## Private

  defp new_priority(priority, term) do
    {priority, :queue.in(term, :queue.new)}
  end

  defp add_to_priority({{priority, pq}, next_p}, priority, term) do
    {{priority, :queue.in(term, pq)}, next_p}
  end
  defp add_to_priority({{last_priority, _} = last, nil}, priority, term) when priority > last_priority do
    {last, {new_priority(priority, term), nil}}
  end
  defp add_to_priority({{last_priority, _} = last, next_p}, priority, term) when priority > last_priority do
    {last, add_to_priority(next_p, priority, term)}
  end
  defp add_to_priority({{last_priority, _}, _} = next, priority, term) when priority < last_priority do
    {new_priority(priority, term), next}
  end

  defp extract_min({{priority, pq}, nil}) do
    case :queue.out(pq) do
      {:empty, _pq1} ->
        {:empty, nil}
      {{:value, _} = res, pq1} ->
        {res, {{priority, pq1}, nil}}
    end
  end
  defp extract_min({{priority, pq}, next}) do
    case :queue.out(pq) do
      {:empty, _pq1} ->
        extract_min(next)
      {{:value, _} = res, pq1} ->
        {res, {{priority, pq1}, next}}
    end
  end
end
