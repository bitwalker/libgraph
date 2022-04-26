Code.require_file(Path.join([__DIR__, "..", "test", "support", "parser.ex"]))

alias Graph.Test.Fixtures.Parser

opts = [
  time: 10,
  inputs: %{
    "Enron emails" => Path.join([__DIR__, "..", "test", "fixtures", "email-Enron.txt"]),
  }
]

Benchee.run(
  %{
      "libgraph" => fn path ->
        g = Parser.parse(path)
        43 = Graph.degeneracy(g)
      end
  },
  opts
)
