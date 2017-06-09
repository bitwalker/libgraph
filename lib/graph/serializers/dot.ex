defmodule Graph.Serializers.DOT do
  @moduledoc """
  This serializer converts a Graph to a DOT file, which can then be converted
  to a great many other formats using Graphviz, e.g. `dot -Tpng out.dot > out.png`.
  """
  use Graph.Serializer

  def serialize(%Graph{} = g) do
    result =
      "strict digraph {\n" <>
        serialize_nodes(g) <>
        serialize_edges(g) <>
      "}"
    {:ok, result}
  end

  defp serialize_nodes(%Graph{vertices: vertices} = g) do
    Enum.reduce(vertices, "", fn {id, v}, acc ->
      acc <> indent(1) <> get_vertex_label(g, id, v) <> "\n"
    end)
  end

  defp get_vertex_label(%Graph{vertex_labels: vl}, id, v) do
    encode_label(Map.get(vl, id, v))
  end

  defp encode_label(label) when is_binary(label), do: label
  defp encode_label(label) when is_integer(label), do: Integer.to_string(label)
  defp encode_label(label) when is_float(label), do: Float.to_string(label)
  defp encode_label(label) when is_atom(label), do: Atom.to_string(label)
  defp encode_label(label), do: quoted("#{inspect label}")

  defp quoted(str) do
    <<?", escape_quotes(str) ,?">>
  end
  defp escape_quotes(str) do
    escape_quotes(str, "")
  end
  defp escape_quotes(<<>>, acc), do: acc
  defp escape_quotes(<<?\\, ?\", rest::binary>>, acc) do
    escape_quotes(rest, <<acc::binary, ?\\, ?\">>)
  end
  defp escape_quotes(<<?\", rest::binary>>, acc) do
    escape_quotes(rest, <<acc::binary, ?\\, ?\">>)
  end

  defp serialize_edges(%Graph{vertices: vertices, out_edges: oe, edges_meta: em} = g) do
    edges = Enum.reduce(vertices, [], fn {id, v}, acc ->
      v_label = get_vertex_label(g, id, v)
      edges =
        oe
        |> Map.get(id, MapSet.new)
        |> Enum.map(fn id2 ->
          v2_label = get_vertex_label(g, id2, Map.get(vertices, id2))
          case Map.get(em, {id, id2}) do
            %{weight: w, label: label} ->
              {v_label, v2_label, w, encode_label(label)}
            %{weight: w} ->
              {v_label, v2_label, w}
            _ ->
              {v_label, v2_label}
          end
        end)
      case edges do
        [] -> acc
        _ -> acc ++ edges
      end
    end)
    Enum.reduce(edges, "", fn
      {v_label, v2_label, weight, edge_label}, acc ->
        acc <> indent(1) <> v_label <> " -> " <> v2_label <> " [" <> "label=#{edge_label}; weight=#{weight}" <> "]\n"
      {v_label, v2_label, weight}, acc ->
        acc <> indent(1) <> v_label <> " -> " <> v2_label <> " [" <> "weight=#{weight}" <> "]\n"
      {v_label, v2_label}, acc ->
        acc <> indent(1) <> v_label <> " -> " <> v2_label <> "\n"
    end)
  end

  defp indent(tabs), do: String.duplicate(" ", tabs*4)
end
