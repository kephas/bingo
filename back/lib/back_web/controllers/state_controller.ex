defmodule BackWeb.StateController do
  use BackWeb, :controller
  alias Back.Bingoboard
  alias Back.Bingodraft
  alias Back.Bingostate
  import Jason, only: [decode: 1]

  def view(conn, _params) do
    json conn, %{boards: Bingoboard.serializable_list,
				 drafts: Bingodraft.serializable_list}
  end

  def store(conn, state) do
	case Bingostate.replace(state) do
	  :ok ->
		conn
		|> put_status(:ok)
		|> json(true)

	  :err ->
		conn
		|> put_status(:internal_server_error)
		|> json(%{status: "err"})
	end
  end
end
