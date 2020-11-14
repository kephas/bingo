defmodule Back.Repo.Migrations.CreateBingoboards do
  use Ecto.Migration

  def change do
	create table(:bingoboards) do
	  add :title, :string
	  add :size, :integer
	end
  end
end
