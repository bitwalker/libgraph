defmodule Graph.Test.TopsortConsumer do
  @moduledoc false

  @spec ordered(Graph.t()) :: {:ok, [Graph.vertex()]} | :cyclic
  def ordered(graph) do
    case Graph.topsort(graph) do
      false -> :cyclic
      vertices when is_list(vertices) -> {:ok, vertices}
    end
  end

  @spec batch_sizes(Graph.t()) :: [non_neg_integer()] | :cyclic
  def batch_sizes(graph) do
    case Graph.batch_topsort(graph) do
      false -> :cyclic
      batches when is_list(batches) -> Enum.map(batches, &length/1)
    end
  end
end
