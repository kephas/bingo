defmodule Back.Bingoboard do
  use Ecto.Schema
  alias Back.Repo
  import Ecto.Query, only: [from: 2]
  
  schema "bingoboards" do
	field :title, :string
	field :size, :integer
	field :userid, :string
	has_many :cells, Back.Bingoboardcell
  end

  def to_struct(board) do
	%{title: board.title, size: board.size, userid: board.userid, cells: Enum.map(board.cells, &Back.Bingoboardcell.to_struct/1)}
  end

  def user_boards(userid) do
	from b in Back.Bingoboard, where: b.userid == ^userid
  end

  def serializable_list(userid) do
	Repo.all(user_boards userid) |> Repo.preload(:cells) |> Enum.map(&to_struct/1)
  end

  def replace_all(userid, new_boards) do
	result = Repo.transaction(fn ->
	  Repo.delete_all(user_boards userid)
	  Enum.map(new_boards, &unserialize(userid, &1))
	end)

	case result do
	  {:ok, _} ->
		:ok

	  _ ->
		:err
	end
  end

  def unserialize(userid, skeleton) do
	{:ok, board} = Repo.insert %Back.Bingoboard{title: Map.get(skeleton, "title"), size: Map.get(skeleton, "size"), userid: userid}
	Map.get(skeleton, "cells")
	|> Enum.map(&(Back.Bingoboardcell.unserialize(board, &1)))
  end
end
