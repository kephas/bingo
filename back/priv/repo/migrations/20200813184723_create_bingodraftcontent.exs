defmodule Back.Repo.Migrations.CreateBingoDraftContent do
  use Ecto.Migration

  def change do
	create table(:bingodraftcontents) do
	  add :content, :string
	end
  end
end
