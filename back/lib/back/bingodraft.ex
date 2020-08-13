defmodule Back.Bingodraft do
  use Ecto.Schema

  schema "bingodrafts" do
	field :title, :string
	field :size, :integer
	has_many :contents, Back.Bingodraftcontent
  end
end
