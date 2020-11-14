defmodule Back.Repo.Migrations.CreateBingoboardcells do
  use Ecto.Migration

  def change do
	create table(:bingoboardcells) do
	  add :text, :string
	  add :ticked, :boolean
	  add :bingoboard_id, references(:bingoboards, on_delete: :delete_all)
	end
  end
end
