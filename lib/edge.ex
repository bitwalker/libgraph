defmodule Graph.Edge do
  @moduledoc """
  This module defines the struct used to represent edges and associated metadata about them.

  Used internally, `v1` and `v2` typically hold vertex ids, not the vertex itself, but all
  public APIs which return `Graph.Edge` structs, return them with the actual vertices.
  """
  defstruct v1: nil,
            v2: nil,
            weight: 1,
            label: nil

  @type t :: %__MODULE__{
    v1: Graph.vertex,
    v2: Graph.vertex,
    weight: integer,
    label: term
  }
  @type edge_opt :: {:weight, integer}
                  | {:label, term}
  @type edge_opts :: [edge_opt]

  @doc """
  Defines a new edge with a weight of 1 and no label.
  """
  @spec new(Graph.vertex, Graph.vertex) :: t
  def new(v1, v2) do
    %__MODULE__{v1: v1, v2: v2}
  end

  @doc """
  Defines a new edge and accepts optional values for weight and label.
  The defaults of a weight of 1 and no label will be used if the options do
  not specify otherwise.
  """
  @spec new(Graph.vertex, Graph.vertex, [edge_opt]) :: t
  def new(v1, v2, opts) when is_list(opts) do
    %__MODULE__{
      v1: v1,
      v2: v2,
      weight: Keyword.get(opts, :weight, 1),
      label: Keyword.get(opts, :label)
    }
  end
end
