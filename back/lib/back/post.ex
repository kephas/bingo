defmodule Back.Post do
  use Ecto.Schema

  schema "posts" do
	field :title, :string
	belongs_to :user, Back.User
  end
end
