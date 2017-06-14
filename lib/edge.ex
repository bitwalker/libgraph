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
  def new(v1, v2, opts) when is_map(opts) do
    %__MODULE__{
      v1: v1,
      v2: v2,
      weight: Map.get(opts, :weight, 1),
      label: Map.get(opts, :label)
    }
  end
  def new(v1, v2, nil) do
    %__MODULE__{v1: v1, v2: v2}
  end

  @doc false
  def options_to_meta(opts) when is_list(opts) do
    label = Keyword.get(opts, :label)
    weight = Keyword.get(opts, :weight, 1)
    case {label, weight} do
      {_, w} = meta when is_integer(w) ->
        meta
      {_, w} ->
        throw {:error, {:invalid_edge_option, {:weight, w}}}
    end
  end
  def options_to_meta(opts) when is_map(opts) do
    label = Map.get(opts, :label)
    weight = Map.get(opts, :weight, 1)
    case {label, weight} do
      {_, w} = meta when is_integer(w) ->
        meta
      {_, w} ->
        throw {:error, {:invalid_edge_option, {:weight, w}}}
    end
  end
  def options_to_meta(nil), do: nil

  @doc false
  def to_meta(%__MODULE__{label: label, weight: weight}), do: {label, weight}
end
