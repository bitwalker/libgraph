defmodule Graph.SerializerTests do
  use ExUnit.Case, async: true

  test "to_dot/1" do
    g =
      Graph.new
      |> Graph.add_vertices([:a, :b, :c, :d])
      |> Graph.add_edges([{:a, :b, weight: 3}, {:b, :c, label: 5}, {:b, :d, label: 1.0}, {:c, :d}])
      |> Graph.label_vertex(:a, :start)
      |> Graph.label_vertex(:b, {:complex, :label})
      |> Graph.label_vertex(:d, "finish")
      |> Graph.update_labelled_edge(:b, :d, 1.0, weight: 3)
    assert {:ok, """
    strict digraph {
        start
        "{:complex, :label}"
        c
        finish
        start -> "{:complex, :label}" [weight=3]
        "{:complex, :label}" -> c [label=5; weight=1]
        "{:complex, :label}" -> finish [label=1.0; weight=3]
        c -> finish [weight=1]
    }
    """} = Graph.to_dot(g)
  end
end
