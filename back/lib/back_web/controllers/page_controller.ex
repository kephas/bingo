defmodule BackWeb.PageController do
  use BackWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
