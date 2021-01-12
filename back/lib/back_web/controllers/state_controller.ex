defmodule BackWeb.StateController do
  use BackWeb, :controller
  alias Back.Bingoboard
  alias Back.Bingodraft
  alias Back.Bingostate
  alias Back.Game
  import Jason, only: [decode: 1]

  def view(conn, %{"id" => id}) do
	if Back.User.exists(id) do
      json conn, %{boards: Bingoboard.serializable_list(id),
				   drafts: Bingodraft.serializable_list(id),
				   new_game_endpoint: BackWeb.Router.Helpers.state_url(BackWeb.Endpoint, :new_game, id)}
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

  def new_game(conn, %{"id" => id, "draft" => draft} = params) do
	if Back.User.exists(id) do
	  case Game.create(draft) do
		{:ok, game} ->
		  json conn, %{game: BackWeb.Router.Helpers.game_url(BackWeb.Endpoint, :view, game.zcap)}

		_ ->
		  conn
		  |> put_status(:internal_server_error)
		  |> json(%{status: "err"})
	  end
	else
	  put_status(conn, :not_found)
	end
  end
end
