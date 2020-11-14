defmodule Back.Bingoboardcell do
  use Ecto.Schema

  schema "bingoboardcells" do
	field :ticked, :boolean
	field :text, :string
	belongs_to :bingoboard, Back.Bingoboard
  end

  def to_struct(cell) do
	%{ticked: cell.ticked, text: cell.text}
  end

  def unserialize(board, cell) do
	Back.Repo.insert(Ecto.build_assoc(board, :cells,
		  ticked: Map.get(cell, "ticked"),
		  text: Map.get(cell, "text")))
  end
end
