# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :back,
  ecto_repos: [Back.Repo]

# Configures the endpoint
config :back, BackWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "eUIhGpcS+eXK69RzPJKgCKy0YRFJAfqzQB0qlcIfzZ0vWeW3weaoMbANEr4u2p/L",
  render_errors: [view: BackWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Back.PubSub, adapter: Phoenix.PubSub.PG2],
  live_view: [signing_salt: "rThPK1R8"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
