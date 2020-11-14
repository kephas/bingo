defmodule Back.Bingostate do
  alias Back.Repo
  alias Back.Bingoboard
  alias Back.Bingodraft

  def replace(new_state) do
	%{"boards" => new_boards, "drafts" => new_drafts} = new_state
	result = Repo.transaction(fn ->
	  Bingoboard.replace_all(new_boards)
	  Bingodraft.replace_all(new_drafts)
	end)

	case result do
	  {:ok, _} ->
		:ok

	  _ ->
		:err
	end
  end
end
