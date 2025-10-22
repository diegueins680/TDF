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


---

## Trial Teacher Availability

The UI expects subject↔room mappings and teacher availability windows. Apply the
schema helpers and seed data:

```bash
./scripts/migrate_trial_availability.sh
stack run   # seeds create default subjects, room preferences, and time slots
```

## Lessons, Packages, Payments & Receipts

This repo's backend is now prepared for lessons and receipts via:

1. **DB schema & seeds** – see `sql/2025-10-21_packages_lessons_receipts.sql`.
   Apply it with:

   ```bash
   ./scripts/migrate_lessons.sh
   ```

2. **OpenAPI** – see `docs/openapi/lessons-and-receipts.yaml` which defines the
   endpoints used by the UI (teachers, students, packages, enrollments, lessons,
   materials, payments, receipts). Wire these to your Servant server or codegen.

3. **Receipts** – receipts are created against a `payment_id` and store line
   items as JSONB. The frontend generates downloadable PDFs (client-side) using
   the logo that already lives in the UI repo.

### Next steps (server handlers)

Implement handlers that conform to the OpenAPI doc and back them with the schema
above (Persistent or SQL). If your app already exposes a `Pool SqlBackend`,
each handler typically looks like:

```haskell
listStudentsHandler :: AppM [StudentDTO]
listStudentsHandler = runDB $ do
  xs <- selectList [] [Asc StudentName]
  pure (map toDTO xs)
```

Where `runDB` uses `runSqlPool` under the hood. Use views or joins for
cross-entity summaries (e.g. lessons by teacher, students by teacher).

> Tip: Keep receipt number generation monotonic (e.g., `R-YYYY-NNNN`) using a
> sequence, and enforce uniqueness at DB level (`unique` index already present).

### Logo

The frontend expects a logo at `public/tdf-logo.svg`. If your UI repo already
has a specific file (e.g. `TDF UI LOGO 1 colorline.svg`), either copy it to
`public/tdf-logo.svg` or update the import in `ReceiptPDF.tsx` accordingly.
