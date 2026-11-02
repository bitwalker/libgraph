defmodule Graph.UtilsTest do
  use ExUnit.Case, async: true

  defp sizeof(term) do
    Graph.Utils.sizeof(term)
  end

  test "sizeof/1" do
    wordsize = :erlang.system_info(:wordsize)
    assert sizeof({1, :foo, "bar"}) == 8 * wordsize
    assert sizeof([]) == wordsize
    assert sizeof([1 | 2]) == 3 * wordsize
    assert sizeof([1, 2, 3]) == 7 * wordsize

    binary = String.duplicate("bar", 128)
    larger_binary = String.duplicate("bar", 256)
    assert sizeof(binary) > byte_size(binary)
    assert sizeof(larger_binary) - sizeof(binary) == byte_size(binary)
    assert sizeof({binary}) > sizeof(binary)
  end
end
