defimpl Inspect, for: Graph do
  def inspect(g, %{structs: false} = opts) do
    Inspect.Algebra.to_doc(g, opts)
  end
  # For graphs with less than 100 vertices, we'll try to pretty print it,
  # however we should avoid doing so with larger graphs, as it will likely cause outrageous
  # memory consumption, not to mention be expensive to calculate, and the pretty form is not
  # very useful at that size anyway
  def inspect(%Graph{vertices: vs, out_edges: es}, opts) when map_size(vs) < 100 do
    # The goal here is to strip out the ids map, convert the vertices map to a list of vertices
    # and convert the map of edges to their reified forms (i.e. the actual vertex term is used in place of ids)
    # we also want to respect the inspect options as much as possible, so we do this all the hard way by
    # constructing the inspect algebra by hand
    vs_doc = Inspect.Algebra.to_doc(Map.values(vs), opts)
    doc = Inspect.Algebra.concat([Inspect.Algebra.empty, "#Graph<vertices:", " ", vs_doc, ",", " ", "edges: [", ""])
    doc = Stream.map(es, fn {v_id, out_neighbors} ->
      v = Inspect.Algebra.to_doc(Map.get(vs, v_id), opts)
      ns = Enum.map(out_neighbors, &Map.get(vs, &1))
      [v, " -> ", Inspect.Algebra.to_doc(ns, opts)]
    end)
    |> Enum.intersperse(", ")
    |> Enum.reduce(doc, fn
      doc_part, doc when is_list(doc_part) ->
        Inspect.Algebra.concat([doc|doc_part])
      doc_part, doc ->
        Inspect.Algebra.concat(doc, doc_part)
    end)
    Inspect.Algebra.concat(doc, "]>")
  end
  # For large graphs, just print summary information about the graph
  def inspect(%Graph{} = g, _opts) do
    %{num_vertices: num_vertices,
      num_edges: num_edges,
      size_in_bytes: size_in_bytes} = Graph.info(g)
    "#Graph<num_vertices: #{num_vertices}, num_edges: #{num_edges}, size_in_bytes: #{size_in_bytes}>"
  end
end
