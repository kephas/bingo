defmodule Back.Bingostate do
  alias Back.Repo
  alias Back.Bingoboard
  alias Back.Bingodraft

  def replace(new_state) do
	%{"id" => userid, "boards" => new_boards, "drafts" => new_drafts} = new_state
	result = Repo.transaction(fn ->
	  Bingoboard.replace_all(userid, new_boards)
	  Bingodraft.replace_all(userid, new_drafts)
	end)

	case result do
	  {:ok, _} ->
		:ok

	  _ ->
		:err
	end
  end
end
