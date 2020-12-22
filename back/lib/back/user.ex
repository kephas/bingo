defmodule Back.User do
  use Ecto.Schema
  import Ecto.Query, only: [from: 2]

  schema "users" do
	field :userid, :string
  end

  def exists(id) do
	not Enum.empty?(Back.Repo.all(from u in Back.User, where: u.userid == ^id))
  end

  def create() do
	Back.Repo.insert %Back.User{userid: Ocaps.make_id()}
  end
end
