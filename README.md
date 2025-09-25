# TDF HQ (Haskell, Servant + Persistent + PostgreSQL)

A minimal, type-safe skeleton for TDF's internal app: CRM, Scheduling, Packages, Invoicing & Inventory.

> **zsh tip:** In interactive zsh, `#` is **not** a comment unless `setopt interactivecomments` is enabled. Either enable it once:
> ```bash
> echo 'setopt interactivecomments' >> ~/.zshrc && source ~/.zshrc
> ```
> or simply **do not paste lines that start with `#`**.

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
# seed initial data (temporary endpoint)
curl -X POST http://localhost:8080/admin/seed
```

Server starts on `http://localhost:8080`.
