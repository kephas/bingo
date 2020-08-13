defmodule Back.Repo.Migrations.ContentBelongsToDraft do
  use Ecto.Migration

  def change do
	alter table(:bingodraftcontents) do
	  add :bingodraft_id, references(:bingodrafts, on_delete: :delete_all)
	end
  end
end
