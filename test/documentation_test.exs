defmodule Graph.DocumentationTest do
  use ExUnit.Case, async: true

  test "depth-first diagrams preserve backslashes and line boundaries" do
    {:docs_v1, _, _, _, _, _, docs} = Code.fetch_docs(Graph)

    for name <- [:preorder, :postorder] do
      {{:function, ^name, 1}, _, _, %{"en" => text}, _} =
        Enum.find(docs, fn {entry, _, _, _, _} -> entry == {:function, name, 1} end)

      lines = String.split(text, "\n")
      assert Enum.count(lines, &String.contains?(&1, <<92>>)) == 2
      assert Enum.any?(lines, &(String.trim(&1) == ":b"))
    end
  end
end
