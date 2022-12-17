defmodule NewAppTest do
  use ExUnit.Case
  doctest NewApp

  test "greets the world" do
    assert NewApp.hello() == :world
  end
end
