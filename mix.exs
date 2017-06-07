defmodule Graph.Mixfile do
  use Mix.Project

  def project do
    [app: :libgraph,
     version: "0.3.0",
     elixir: "~> 1.4 or >= 1.3.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),
     description: "A pure Elixir implementation of a directed graph data structure",
     package: package()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    # Specify extra applications you'll use from Erlang/Elixir
    [extra_applications: []]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [{:benchee, "~> 0.8", only: :dev},
     {:propcheck, "~> 0.0.1", only: :test},
     {:ex_doc, ">= 0.0.0", only: :dev}]
  end

  defp package do
    [files: ["lib", "mix.exs", "README.md", "LICENSE"],
     maintainers: ["Paul Schoenfelder"],
     licenses: ["MIT"],
     links: %{"GitHub": "https://github.com/bitwalker/libgraph"}]
  end
end
