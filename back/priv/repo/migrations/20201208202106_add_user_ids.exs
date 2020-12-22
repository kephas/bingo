defmodule Back.Repo.Migrations.AddUserIds do
  use Ecto.Migration

  def change do
	create table(:users) do
	  add :userid, :string
	end
	
	alter table(:bingodrafts) do
	  add :userid, :string
	end
	
	alter table(:bingoboards) do
	  add :userid, :string
	end
  end
end
