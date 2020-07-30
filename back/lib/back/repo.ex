defmodule Back.Repo do
  use Ecto.Repo,
    otp_app: :back,
    adapter: Ecto.Adapters.Postgres
end
