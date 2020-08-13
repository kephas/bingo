defmodule Back.Repo.Migrations.CreateBingoDraft do
  use Ecto.Migration

  def change do
	create table(:bingodrafts) do
      add :title, :string
	  add :size, :integer
	end
  end
end
