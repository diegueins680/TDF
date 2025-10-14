# TDF HQ (Haskell, Servant + Persistent + PostgreSQL)

A minimal, type-safe skeleton for TDF's internal app: CRM, Scheduling, Packages, Invoicing & Inventory.

## Prereqs

- PostgreSQL 16 (local) OR Docker (optional).
- Haskell **Stack** (recommended).

## Configure

Copy `config/default.env` to `.env` (or just export them in your shell):

```
DB_HOST=127.0.0.1
DB_PORT=5432
DB_USER=postgres
DB_PASS=postgres
DB_NAME=tdf_hq
APP_PORT=8080
```

## Build & Run (Stack)

```bash
# from project root
set -a; source config/default.env; set +a
stack setup
stack build
stack run
# database seeds now run automatically (set SEED_DB=false to skip)
```

Server starts on `http://localhost:8080`.

## Docker Compose (Postgres + App)

Build and run both services:

```bash
make up       # builds the image and starts Postgres + app
make logs     # follow combined logs
make health   # check /health endpoint
make seed     # seed demo data (uses admin-token)
make down     # stop services (keep volumes)
make clean    # stop and remove volumes
```

Compose sets DB env for the app; the server listens on `localhost:8080`.
