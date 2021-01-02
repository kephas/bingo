defmodule BackWeb.Router do
  use BackWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", BackWeb do
	pipe_through :api

	get "/states/:id", StateController, :view
	post "/states/:id", StateController, :store
	post "/states/:id/game", StateController, :new_game
  end

  # Other scopes may use custom stacks.
  # scope "/api", BackWeb do
  #   pipe_through :api
  # end
end
