defmodule Graph.Serializers.DOT do
  @moduledoc """
  This serializer converts a Graph to a DOT file, which can then be converted
  to a great many other formats using Graphviz, e.g. `dot -Tpng out.dot > out.png`.
  """
  use Graph.Serializer

  def serialize(%Graph{type: type} = g) do
    type = if type == :directed, do: "digraph", else: "graph"
    result =
      "strict #{type} {\n" <>
        serialize_nodes(g) <>
        serialize_edges(g) <>
      "}\n"
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
    <<?", escape_quotes(str)::binary, ?">>
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
  defp escape_quotes(<<c::utf8, rest::binary>>, acc) do
    escape_quotes(rest, <<acc::binary, c::utf8>>)
  end

  defp serialize_edges(%Graph{type: type, vertices: vertices, out_edges: oe, edges: em} = g) do
    edges = Enum.reduce(vertices, [], fn {id, v}, acc ->
      v_label = get_vertex_label(g, id, v)
      edges =
        oe
        |> Map.get(id, MapSet.new)
        |> Enum.flat_map(fn id2 ->
          v2_label = get_vertex_label(g, id2, Map.get(vertices, id2))
          Enum.map(Map.fetch!(em, {id, id2}), fn
            {nil, weight} ->
              {v_label, v2_label, weight}
            {label, weight} ->
              {v_label, v2_label, weight, encode_label(label)}
          end)
        end)
      case edges do
        [] -> acc
        _ -> acc ++ edges
      end
    end)
    arrow = if type == :directed, do: "->", else: "--"
    Enum.reduce(edges, "", fn
      {v_label, v2_label, weight, edge_label}, acc ->
        acc <> indent(1) <> v_label <> " #{arrow} " <> v2_label <> " [" <> "label=#{edge_label}; weight=#{weight}" <> "]\n"
      {v_label, v2_label, weight}, acc ->
        acc <> indent(1) <> v_label <> " #{arrow} " <> v2_label <> " [" <> "weight=#{weight}" <> "]\n"
    end)
  end

  defp indent(tabs), do: String.duplicate(" ", tabs*4)
end
