defmodule BackWeb.StateController do
  use BackWeb, :controller
  alias Back.Bingoboard
  alias Back.Bingodraft
  alias Back.Bingostate
  import Jason, only: [decode: 1]

  def view(conn, %{"id" => id}) do
	if Back.User.exists(id) do
      json conn, %{boards: Bingoboard.serializable_list(id),
				   drafts: Bingodraft.serializable_list(id)}
	else
	  put_status(conn, :not_found)
	end
  end

  def store(conn, %{"id" => id} = params) do
	if Back.User.exists(id) do
	  case Bingostate.replace(params) do
		:ok ->
		  conn
		  |> put_status(:ok)
		  |> json(true)
		  
		  :err ->
		  conn
		  |> put_status(:internal_server_error)
		  |> json(%{status: "err"})
	  end
	else
	  put_status(conn, :not_found)
	end
  end
end
