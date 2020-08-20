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
end
