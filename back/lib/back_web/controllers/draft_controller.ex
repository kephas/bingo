defmodule BackWeb.DraftController do
  use BackWeb, :controller
  alias Back.Bingodraft
  import Jason, only: [decode: 1]

  def view(conn, _params) do
    json conn, Bingodraft.serializable_list
  end

  def store(conn, params) do
	%{"_json" => drafts} = params
	case Bingodraft.replace_all(drafts) do
	  :ok ->
		conn
		|> put_status(:ok)
		|> json(%{status: "ok"})

	  :err ->
		conn
		|> put_status(:internal_server_error)
		|> json(%{status: "err"})
	end
  end
end
