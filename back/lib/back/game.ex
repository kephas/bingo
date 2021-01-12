defmodule Back.Game do
  alias Back.Repo
  alias Back.Game
  alias Back.Bingodraft
  use Ecto.Schema
  import Ecto.Query, only: [from: 2]

  schema "games" do
	field :zcap, :string
  end

  def exists(zcap) do
	not Enum.empty?(Repo.all(from g in Game, where: g.zcap == ^zcap))
  end

  def create(draft_skeleton) do
	Repo.transaction(fn ->
	  {:ok, game} = Repo.insert %Game{zcap: Ocaps.make_id()}
	  Bingodraft.unserialize("game", game.zcap, draft_skeleton)
	  game
	end)
  end
end
