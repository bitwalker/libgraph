defmodule Graph.TreeTest do
  use ExUnit.Case, async: true
  alias Graph.Tree
  alias Graph.Tree.Node

  describe "from_treeword!/1" do
    test "initializes a Tree from a treeword list" do
      %Tree{root: %Node{data: :a}} =
        t = Tree.from_treeword!(a: [b: [:b_1, :b_2], c: [:c_1, :c_2, :c_3]])

      t.graph
      |> Graph.vertices()
      |> Enum.map(& &1.data)
      |> assert_list_equal([:a, :b, :b_1, :b_2, :c, :c_1, :c_2, :c_3])

      assert Graph.is_arborescence?(t.graph)
      assert Graph.arborescence_root(t.graph) == t.root

      get_child = &get_child(t, &1, &2)
      get_children_data = &get_children_data(t, &1)

      assert [:b_1, :b_2] ==
               t.root
               |> get_child.(:b)
               |> get_children_data.()

      assert [:c_1, :c_2, :c_3] ==
               t.root
               |> get_child.(:c)
               |> get_children_data.()
    end

    test "can accept an non-treeword list if told to add a virtual root" do
      t = Tree.from_treeword!([b: [:b_1, :b_2], c: [:c_1, :c_2, :c_3]], virtual_root: true)
      assert %Node{data: :root} = t.root

      get_child = &get_child(t, &1, &2)
      get_children_data = &get_children_data(t, &1)

      assert [:b_1, :b_2] ==
               t.root
               |> get_child.(:b)
               |> get_children_data.()

      assert [:c_1, :c_2, :c_3] ==
               t.root
               |> get_child.(:c)
               |> get_children_data.()
    end

    test "raises if passed in a list with multiple roots" do
      assert_raise(ArgumentError, fn -> Tree.from_treeword!(a: [:b], c: [:d]) end)
    end

    test "raises if passed in an empty list" do
      assert_raise(ArgumentError, fn -> Tree.from_treeword!([]) end)
    end

    test "raises if passed malformed treeword" do
      assert_raise(ArgumentError, fn -> Tree.from_treeword!(a: [4]) end)
      assert_raise(ArgumentError, fn -> Tree.from_treeword!({"something", [:a, :b]}) end)
    end
  end

  describe "reduce/3" do
    test "processes a node and then its children" do
      t = Tree.from_treeword!(a: [b: [:b_1, :b_2], c: [:c_1]])

      reduction =
        t
        |> Tree.reduce([], fn node, _path_stack, acc ->
          {:next, [node.data | acc]}
        end)
        |> Enum.reverse()

      assert [root, second | _] = reduction
      assert root == :a
      assert second == :b or second == :c

      find_index = fn seeking -> Enum.find_index(reduction, &(&1 == seeking)) end

      case second do
        :c ->
          last_child_index = find_index.(:c_1)

          [:b, :b_1, :b_2]
          |> Enum.each(&assert find_index.(&1) > last_child_index)

        :b ->
          last_child_index = max(find_index.(:b_1), find_index.(:b_2))

          [:c, :c_1]
          |> Enum.each(&assert find_index.(&1) > last_child_index)
      end
    end

    test "passes in the path to the reducer as a stack-like list (root last)" do
      t = Tree.from_treeword!(a: [b: [:b_1, :b_2], c: [:c_1]])

      reduction =
        t
        |> Tree.reduce([], fn node, path_stack, acc ->
          full_path = [node.data | node_data(path_stack)]
          {:next, [full_path | acc]}
        end)

      expected_paths = [
        [:c_1, :c, :a],
        [:b_1, :b, :a],
        [:b_2, :b, :a],
        [:b, :a],
        [:c, :a],
        [:a]
      ]

      assert_list_equal(expected_paths, reduction)
    end

    test "does not include a virtual root in the path stack" do
      t = Tree.from_treeword!([b: [:b_1, :b_2], c: [:c_1]], virtual_root: true)

      reduction =
        t
        |> Tree.reduce([], fn node, path_stack, acc ->
          full_path = [node.data | node_data(path_stack)]
          {:next, [full_path | acc]}
        end)

      expected_paths = [
        [:c_1, :c],
        [:b_1, :b],
        [:b_2, :b],
        [:b],
        [:c]
      ]

      assert_list_equal(expected_paths, reduction)
    end

    test "processes no more stuff after given a halt signal" do
      t = Tree.from_treeword!(a: [b: [:b_1, :b_2], c: [:c_1]])

      reduction =
        t
        |> Tree.reduce([], fn %Node{data: data}, _path_stack, acc ->
          if data == :c do
            {:halt, acc}
          else
            {:next, [data | acc]}
          end
        end)
        |> Enum.reverse()

      assert [root | _] = reduction
      assert root == :a
      assert Enum.count(reduction) <= 4
      assert :c not in reduction
    end
  end

  describe "leaf_reduce/3" do
    test "only processes leaves" do
      t = Tree.from_treeword!(a: [b: [:b_1, :b_2], c: [:c_1]])

      reduction =
        Tree.leaf_reduce(t, [], fn node, path_stack, acc ->
          cur_path =
            [node | path_stack]
            |> Enum.map(& &1.data)
            |> Enum.reverse()

          {:next, [cur_path | acc]}
        end)

      assert_list_equal(reduction, [
        [:a, :b, :b_1],
        [:a, :b, :b_2],
        [:a, :c, :c_1]
      ])
    end
  end

  describe "leaf_map/2" do
    test "yields current path to leaf to map function" do
      t = Tree.from_treeword!(a: [b: [:b_1, :b_2], c: [:c_1]])

      reduction = Tree.leaf_map(t, &Function.identity/1)

      assert_list_equal(reduction, [
        [:a, :b, :b_1],
        [:a, :b, :b_2],
        [:a, :c, :c_1]
      ])
    end
  end

  describe "current_path/2" do
    test "when given node and path stack, returns only data elements" do
      t = Tree.from_treeword!(a: [b: [:b_1]])

      child = Graph.out_neighbors(t.graph, t.root) |> hd()
      path_stack = [child, t.root]

      leaf =
        t.graph
        |> Graph.vertices()
        |> Enum.find(&(&1.data == :b_1))

      assert [:a, :b, :b_1] == Tree.current_path(leaf, path_stack)
    end

    test "when path stack is empty, return current node" do
      t = Tree.from_treeword!(a: [b: [:b_1]])

      assert [:a] == Tree.current_path(t.root, [])
    end
  end

  defp node_data(nodes) do
    Enum.map(nodes, & &1.data)
  end

  defp get_child(tree, node, data) do
    tree.graph
    |> Graph.out_edges(node)
    |> Enum.find(&(&1.v2.data == data))
    |> then(& &1.v2)
  end

  defp get_children_data(tree, node) do
    tree.graph
    |> Graph.out_edges(node)
    |> Enum.map(& &1.v2.data)
    |> Enum.sort()
  end

  defp assert_list_equal(list_1, list_2), do: assert Enum.sort(list_1) == Enum.sort(list_2)
end
