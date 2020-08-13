defmodule BackWeb.DraftController do
  use BackWeb, :controller

  def view(conn, _params) do
    json conn, [%{title: "Foo", size: 4, choices: ["bar", "baz", "quux", "quuux"]}]
  end

  def store(conn, _params) do
	# stocke
  end
end
