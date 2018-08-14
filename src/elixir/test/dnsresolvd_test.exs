defmodule DnsresolvdTest do
  use ExUnit.Case
  doctest Dnsresolvd

  test "greets the world" do
    assert Dnsresolvd.hello() == :world
  end
end
