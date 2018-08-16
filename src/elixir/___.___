#
# src/elixir/mix.exs
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Cowboy-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

defmodule Dnsresolvd.MixProject do
    use Mix.Project

    def project do [
        app:             :dnsresolvd,
        version:         "0.1.0",
        elixir:          "~> 1.6",
        start_permanent: Mix.env() == :prod,
        deps:            deps(),
    ] end

    # -------------------------------------------------------------------------
    # --- Run "mix help compile.app" to learn about applications. -------------
    # -------------------------------------------------------------------------
    def application do [
#       extra_applications: [:logger]
    ] end

    # -------------------------------------------------------------------------
    # --- Run "mix help deps" to learn about dependencies. --------------------
    # -------------------------------------------------------------------------
    defp deps do [
#       {
#           :dep_from_hexpm, "~> 0.3.0"
#       },
#       {
#           :dep_from_git,
#           git: "https://github.com/elixir-lang/my_dep.git",
#           tag: "0.1.0"
#       },
    ] end
end

# vim:set nu ts=4 sw=4:
