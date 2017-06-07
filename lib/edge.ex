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
    case Keyword.split(opts, [:weight, :label]) do
      {[], _} -> nil
      {valid_opts, _} ->
        case Map.new(valid_opts) do
          %{weight: w} = meta when is_integer(w) ->
            meta
          %{weight: w} ->
            throw {:error, {:invalid_edge_option}, {:weight, w}}
          meta ->
            meta
        end
    end
  end
  def options_to_meta(opts) when is_map(opts) do
    case {Map.get(opts, :weight), Map.get(opts, :label)} do
      {nil, nil} ->
        nil
      {nil, l} ->
        %{label: l}
      {w, nil} when is_integer(w) ->
        %{weight: w}
      {w, l} when is_integer(w) ->
        %{weight: w, label: l}
      {w, _} ->
        throw {:error, {:invalid_edge_option, {:weight, w}}}
    end
  end
  def options_to_meta(nil), do: nil

  @doc false
  def to_meta(%__MODULE__{weight: 1, label: nil}), do: nil
  def to_meta(%__MODULE__{weight: weight, label: nil}), do: %{weight: weight}
  def to_meta(%__MODULE__{weight: 1, label: label}), do: %{label: label}
  def to_meta(%__MODULE__{weight: weight, label: label}), do: %{weight: weight, label: label}
end
