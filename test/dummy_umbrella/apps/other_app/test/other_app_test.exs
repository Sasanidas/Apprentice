defmodule OtherAppTest do
  use ExUnit.Case
  doctest OtherApp

  test "greets the world" do
    assert OtherApp.hello() == :world
  end
end
