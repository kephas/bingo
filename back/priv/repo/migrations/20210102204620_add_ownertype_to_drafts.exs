defmodule Back.Repo.Migrations.AddOwnertypeToDrafts do
  use Ecto.Migration

  def change do
	rename table(:bingodrafts), :userid, to: :ownerid
	alter table(:bingodrafts) do
	  add :ownertype, :string
	end
  end
end
