defmodule Back.Bingodraftcontent do
  use Ecto.Schema

  schema "bingodraftcontents" do
	field :content, :string
	belongs_to :bingodraft, Back.Bingodraft
  end
end
