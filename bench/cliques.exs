Code.require_file(Path.join([__DIR__, "..", "test", "support", "parser.ex"]))

alias Graph.Test.Fixtures.Parser

opts = [
  time: 10,
  inputs: %{
    "Enron emails" => Path.join([__DIR__, "..", "test", "fixtures", "email-Enron.txt"]),
  }
]

# This currently takes ages, only uncomment when you really want to run it
Benchee.run(
  # %{
  #   "libgraph" => fn path ->
  #     g = Parser.parse(path)
  #     _ = Graph.cliques(g)
  #   end
  # },
  opts
)
