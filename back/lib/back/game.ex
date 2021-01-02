defmodule Back.Game do
  use Ecto.Schema
  import Ecto.Query, only: [from: 2]

  schema "games" do
	field :zcap, :string
  end

  def exists(zcap) do
	not Enum.empty?(Back.Repo.all(from g in Back.Game, where: g.zcap == ^zcap))
  end

  def create() do
	Back.Repo.insert %Back.Game{zcap: Ocaps.make_id()}
  end
end
