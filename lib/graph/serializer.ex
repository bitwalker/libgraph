defmodule Graph.Serializer do
  @moduledoc """
  This module defines the Serializer behavior for graphs.
  """
  @callback serialize(Graph.t) :: {:ok, binary} | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Graph.Serializer
    end
  end
end
