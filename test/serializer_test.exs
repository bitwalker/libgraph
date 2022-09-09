defmodule Graph.SerializerTests do
  use ExUnit.Case, async: true

  test "to_dot/1" do
    g = kitchen_sink_graph()

    assert {:ok,
            """
            strict digraph {
                "start"
                "{:complex, :label}"
                "c"
                "finish"
                "start" -> "{:complex, :label}" [weight=3]
                "{:complex, :label}" -> "c" [label=5; weight=1]
                "{:complex, :label}" -> "finish" [label=1.0; weight=3]
                "c" -> "finish" [weight=1]
            }
            """} = Graph.to_dot(g)
  end


  test "to_png/1" do
    test_file = "test"
    g = kitchen_sink_graph()

    assert {:ok, ""} = Graph.to_png(g, test_file)
    assert """
    strict digraph {
        "start"
        "{:complex, :label}"
        "c"
        "finish"
        "start" -> "{:complex, :label}" [weight=3]
        "{:complex, :label}" -> "c" [label=5; weight=1]
        "{:complex, :label}" -> "finish" [label=1.0; weight=3]
        "c" -> "finish" [weight=1]
    }
    """ = File.read! "#{test_file}.dot"
    assert File.exists? "#{test_file}.png"
    File.rm "#{test_file}.dot"
    File.rm "#{test_file}.png"
  end

  test "to_edgelist" do
    g = kitchen_sink_graph()

    {:ok, actual} = Graph.to_edgelist(g)

    expected = """
    "start" "{:complex, :label}"
    "{:complex, :label}" "c"
    "{:complex, :label}" "finish"
    "c" "finish"
    """

    assert actual == expected
  end

  defp kitchen_sink_graph do
    Graph.new()
    |> Graph.add_vertices([:a, :b, :c, :d])
    |> Graph.add_edges([{:a, :b, weight: 3}, {:b, :c, label: 5}, {:b, :d, label: 1.0}, {:c, :d}])
    |> Graph.label_vertex(:a, :start)
    |> Graph.label_vertex(:b, {:complex, :label})
    |> Graph.label_vertex(:d, "finish")
    |> Graph.update_labelled_edge(:b, :d, 1.0, weight: 3)
  end
end
