defmodule Graph.Common do
  def reachable(%Graph{type: :directed} = g, vs) when is_struct(g) and is_list(vs) do
    Graph.Directed.reachable(g, vs)
  end

  def reachable(%Graph{type: :undirected} = g, vs) when is_struct(g) and is_list(vs) do
    Graph.Undirected.reachable(g, vs)
  end
end
