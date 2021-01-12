defmodule Back.Bingodraft do
  use Ecto.Schema
  alias Back.Repo
  import Ecto.Query, only: [from: 2]

  schema "bingodrafts" do
	field :title, :string
	field :size, :integer
	field :ownertype, :string
	field :ownerid, :string
	has_many :contents, Back.Bingodraftcontent
  end

  def flatten(draft) do
	%{title: draft.title, size: draft.size, choices: Enum.map(draft.contents, &(&1.content))}
  end

  def user_drafts(userid) do
	from d in Back.Bingodraft, where: d.ownertype == "user" and d.ownerid == ^userid
  end

  def serializable_list(userid) do
	Repo.all(user_drafts userid) |> Repo.preload(:contents) |> Enum.map(&flatten/1)
  end

  def replace_all(userid, new_drafts) do
	result = Repo.transaction(fn ->
	  Repo.delete_all(user_drafts userid)
	  Enum.map(new_drafts, &unserialize("user", userid, &1))
	end)

	case result do
	  {:ok, _} ->
		:ok

	  _ ->
		:err
	end
  end

  def unserialize(ownertype, ownerid, skeleton) do
	{:ok, draft} = Repo.insert %Back.Bingodraft{title: Map.get(skeleton, "title"), size: Map.get(skeleton, "size"), ownertype: ownertype, ownerid: ownerid}
	Map.get(skeleton, "choices")
	|> Enum.map(&(Back.Bingodraftcontent.unserialize(draft, &1)))
  end
end
