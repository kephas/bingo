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
    pipe_through :browser

    get "/", PageController, :index
  end

  scope "/states", BackWeb do
	pipe_through :api

	get "/:id", StateController, :view
	post "/:id", StateController, :store
  end

  # Other scopes may use custom stacks.
  # scope "/api", BackWeb do
  #   pipe_through :api
  # end
end
