defmodule BackWeb.DraftController do
  use BackWeb, :controller
  alias Back.Bingodraft

  def view(conn, _params) do
    json conn, Bingodraft.serializable_list
  end

  def store(conn, _params) do
	# stocke
  end
end
