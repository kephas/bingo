defmodule Back.Bingodraftcontent do
  use Ecto.Schema

  schema "bingodraftcontents" do
	field :content, :string
	belongs_to :bingodraft, Back.Bingodraft
  end

  def unserialize(draft, content) do
	Back.Repo.insert(Ecto.build_assoc(draft, :contents, content: content))
  end
end
