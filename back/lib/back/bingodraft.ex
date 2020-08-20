defmodule Back.Bingodraft do
  use Ecto.Schema
  alias Back.Repo

  schema "bingodrafts" do
	field :title, :string
	field :size, :integer
	has_many :contents, Back.Bingodraftcontent
  end

  def flatten(draft) do
	%{title: draft.title, size: draft.size, choices: Enum.map(draft.contents, &(&1.content))}
  end

  def serializable_list() do
	Repo.all(Back.Bingodraft) |> Repo.preload(:contents) |> Enum.map(&flatten/1)
  end

  def replace_all(new_drafts) do
	result = Repo.transaction(fn ->
	  Repo.delete_all(Back.Bingodraft)
	  new_drafts
	  |> Enum.map(&unserialize/1)
	  |> Enum.map(&Repo.insert/1)
	end)

	case result do
	  {:ok, _} ->
		:ok

	  _ ->
		:err
	end
  end

  def unserialize(skeleton) do
	%Back.Bingodraft{title: Map.get(skeleton, "title"), size: Map.get(skeleton, "size")}
  end
end
