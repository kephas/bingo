defmodule Back.Repo.Migrations.CreateGames do
  use Ecto.Migration

  def change do
	create table(:games) do
	  add :zcap, :string
	end
  end
end
