defmodule Back.Bingoboard do
  use Ecto.Schema
  alias Back.Repo

  schema "bingoboards" do
	field :title, :string
	field :size, :integer
	has_many :cells, Back.Bingoboardcell
  end

  def to_struct(board) do
	%{title: board.title, size: board.size, cells: Enum.map(board.cells, &Back.Bingoboardcell.to_struct/1)}
  end

  def serializable_list() do
	Repo.all(Back.Bingoboard) |> Repo.preload(:cells) |> Enum.map(&to_struct/1)
  end

  def replace_all(new_boards) do
	result = Repo.transaction(fn ->
	  Repo.delete_all(Back.Bingoboard)
	  Enum.map(new_boards, &unserialize/1)
	end)

	case result do
	  {:ok, _} ->
		:ok

	  _ ->
		:err
	end
  end

  def unserialize(skeleton) do
	{:ok, board} = Repo.insert %Back.Bingoboard{title: Map.get(skeleton, "title"), size: Map.get(skeleton, "size")}
	Map.get(skeleton, "cells")
	|> Enum.map(&(Back.Bingoboardcell.unserialize(board, &1)))
  end
end
