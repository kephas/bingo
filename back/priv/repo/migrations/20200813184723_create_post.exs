defmodule Back.Repo.Migrations.CreatePost do
  use Ecto.Migration

  def change do
	create table(:posts) do
	  add :title, :string
	end
  end
end
