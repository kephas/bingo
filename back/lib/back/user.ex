defmodule Back.User do
  use Ecto.Schema

  schema "users" do
	field :name, :string
	has_many :posts, Back.Post
  end
end
