defmodule Ocaps do
  def random_binary_string(remaining \\ 16, acc \\ <<>>) do
	if remaining == 0 do
	  acc
	else
	  random_binary_string(remaining - 1, <<:rand.uniform(256) - 1>> <> acc)
	end
  end
	
  def make_id() do
	Base.url_encode64(random_binary_string(), padding: false)
  end
end
