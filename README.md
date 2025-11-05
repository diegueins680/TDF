# TDF HQ - Backend API

[![GitHub](https://img.shields.io/badge/GitHub-tdf--app-blue)](https://github.com/diegueins680/tdf-app)
[![Haskell](https://img.shields.io/badge/Haskell-Stack-purple)](https://docs.haskellstack.org/)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-16-blue)](https://www.postgresql.org/)

A Haskell (Servant + Persistent) backend that powers TDF's internal HQ app for CRM, scheduling, lesson packages, invoicing, inventory, and trial management. The service exposes a JSON API secured with bearer tokens, generates PDFs for operational checklists, and boots with opinionated seeds so the UI can be exercised immediately.

**Part of the [TDF Records Management Platform](https://github.com/diegueins680/tdf-app)** - See the main repository for complete project documentation.

## Architecture at a Glance

- **Entry point**: [`app/Main.hs`](app/Main.hs) wires configuration, database pooling, migrations, seeding, and the Servant application with CORS middleware.
- **Environment**: [`TDF.Config`](src/TDF/Config.hs) loads `APP_PORT`, database credentials, and flags for resetting/seeding the database, plus the optional seed trigger token.
- **Database**: [`TDF.DB`](src/TDF/DB.hs) creates the PostgreSQL pool and exposes the runtime `Env` consumed across handlers. Persistent models live in [`TDF.Models`](src/TDF/Models.hs), [`TDF.ModelsExtra`](src/TDF/ModelsExtra.hs), and [`TDF.Trials.Models`](src/TDF/Trials/Models.hs) with migrations executed on startup.
- **API surface**: [`TDF.API`](src/TDF/API.hs) defines the main Servant routes. Individual feature areas (CRM parties, bookings, packages, invoices, receipts, bands, inventory, rooms, pipelines, trial lessons, etc.) live in `TDF.API.*` and `TDF.Server*` modules.
- **Server implementation**: [`TDF.Server`](src/TDF/Server.hs) composes all handlers, enforces seed tokens, renders PDF input lists, and hoists the authenticated sub-API. Trials endpoints (`/trials/*`) are mounted alongside the primary API.
- **Authentication**: [`TDF.Auth`](src/TDF/Auth.hs) resolves bearer tokens to roles and module permissions, backing Servant's `AuthProtect` machinery. Admin utilities in [`TDF.ServerAdmin`](src/TDF/ServerAdmin.hs) provide seeded dropdowns and user management.
- **DTOs & contracts**: [`TDF.DTO`](src/TDF/DTO.hs), `TDF.Contracts.*`, and the OpenAPI documents in [`docs/`](docs) define payload shapes shared with the frontend.

### Repository layout

| Path | Purpose |
| --- | --- |
| `app/` | Executable entrypoint and CORS setup. |
| `src/TDF/` | Core library modules (API types, server logic, models, auth, DTOs, seeds, feature servers). |
| `src/TDF/Trials/` | Trial lesson-specific API, models, seeds, and server composition. |
| `scripts/` | Helper scripts for dev (`dev_run.sh`), smoke checks, LaTeX packaging, and DB migrations. |
| `sql/` | Raw SQL migrations for lessons, packages, payments, and receipts. |
| `docs/` | API references, product notes, and OpenAPI specs. |
| `templates/` | LaTeX templates used for generated PDFs (e.g., input list sessions). |

## Getting Started

> **Note:** This is the backend service. For complete setup including frontend and mobile apps, see the [main repository README](https://github.com/diegueins680/tdf-app#readme).

### Prerequisites

- **Haskell Stack**: GHC toolchain via [Stack](https://docs.haskellstack.org/)
  ```bash
  curl -sSL https://get.haskellstack.org/ | sh
  ```
- **PostgreSQL 16**: Either local installation or Docker
  ```bash
  # macOS
  brew install postgresql@16
  
  # Or use Docker Compose (see below)
  ```
- **Development tools**: `make`, `curl`, `jq`
- **Optional**: LaTeX toolchain for PDF generation (`scripts/latex`)

### Quick Start

1. **Configure environment**
   
   Copy the default configuration to `.env`:
   ```bash
   cp config/default.env .env
   ```
   
   Edit `.env` with your settings:
   ```env
   DB_HOST=127.0.0.1
   DB_PORT=5432
   DB_USER=postgres
   DB_PASS=your_secure_password
   DB_NAME=tdf_hq
   APP_PORT=8080
   RESET_DB=false
   SEED_DB=true
   SEED_TRIGGER_TOKEN=tdf-bootstrap-seed
   ```

2. **Create database**
   ```bash
   createdb tdf_hq
   # Or use psql: CREATE DATABASE tdf_hq;
   ```

3. **Build and run**
   ```bash
   stack setup    # First run only
   stack build    # Compile
   stack run      # Start server
   ```

   The API will be available at `http://localhost:8080`

**Environment Variables:**

| Variable | Description | Default |
|----------|-------------|---------|
| `DB_HOST` | PostgreSQL host | `127.0.0.1` |
| `DB_PORT` | PostgreSQL port | `5432` |
| `DB_USER` | Database user | `postgres` |
| `DB_PASS` | Database password | - |
| `DB_NAME` | Database name | `tdf_hq` |
| `APP_PORT` | Server port | `8080` |
| `RESET_DB` | Drop/recreate schema on startup | `false` |
| `SEED_DB` | Load seed data on startup | `true` |
| `SEED_TRIGGER_TOKEN` | Token for `/seed` endpoint | - |
| `ALLOW_ORIGINS` | CORS allowed origins (comma-separated) | - |

## Running the Application

### Development Mode

```bash
# Load environment and run
source .env
stack run

# Or use the dev script
./scripts/dev_run.sh
```

The API will listen on `http://localhost:8080`. On startup:
- Migrations run automatically (base schema, extra entities, trial lessons)
- Seed data loads if `SEED_DB=true`
- CORS is configured for development

### Production Build

```bash
# Build optimized binary
stack build --copy-bins

# The binary will be in ~/.local/bin/
~/.local/bin/tdf-hq-exe
```

### Docker Compose

A `docker-compose.yml` is provided to run PostgreSQL and the app together:

```bash
make up       # build images and start db + app
make logs     # follow combined logs
make health   # hit /health for a quick status
make seed     # POST /admin/seed with the admin token
make down     # stop services (preserve volumes)
make clean    # stop services and remove volumes
```

Override `APP_BASE_URL` when using the `version` Make target, or export environment overrides before `make up` to reconfigure the containerised app.

## Database, migrations, and seeds

- Startup runs `resetSchema` when `RESET_DB=true`, executes migrations from `TDF.Models`, `TDF.ModelsExtra`, and `TDF.Trials.Models`, then optionally `seedAll` for fixtures used by the UI.
- Trial availability helpers live in `scripts/migrate_trial_availability.sh`; lesson/package/receipt migrations live in `scripts/migrate_lessons.sh` and corresponding SQL files.
- Admin-only seed endpoints are exposed under `/admin/seed` and respect the bearer token auth plus `ModuleAdmin` gate.
- The unauthenticated `/seed` endpoint is protected by `X-Seed-Token` and can be disabled via config.

## Feature overview

- **CRM & parties**: Manage parties, roles, and tokens with module-based access control.
- **Scheduling**: Bookings, sessions, trial lesson flows, and PDF input lists (`/input-list/sessions` + PDF rendering).
- **Packages & invoicing**: Package catalog, purchases, invoices, and receipts tied to payments (see `docs/openapi/lessons-and-receipts.yaml`).
- **Inventory & rooms**: Track assets, room setups, and inventory seeding utilities.
- **Pipelines & bands**: Sales/production pipelines and performance band management.
- **Admin tooling**: Dropdown management, user provisioning, role detail enumeration, and global seeding.
- **Metadata endpoints**: `/version`, `/meta/*`, and `/health` for operational visibility.

## Authentication & authorization

- Endpoints under `AuthProtect "bearer-token"` require an `Authorization: Bearer <token>` header.
- Tokens resolve to parties and active roles; modules are derived from roles via `modulesForRoles`.
- Handlers enforce module gates using helpers like `hasModuleAccess` and `ensureModule` (see `TDF.ServerAdmin`).

## Documentation & contracts

- `docs/api.md`, `docs/CalendarAPI.md`, and `docs/CONTRACTS_API.md` describe key integration points.
- REST payloads are defined in `src/TDF/DTO.hs` and `src/TDF/API/Types.hs`, mirroring database entities in `TDF.Models*`.
- The lessons & receipts OpenAPI spec (`docs/openapi/lessons-and-receipts.yaml`) drives the UI integration.
- PDF output uses `templates/invoice.dark.tex`, rendered through helpers in `TDF.Handlers.InputList`.

## Development utilities

- `scripts/dev_run.sh`: export environment variables, build, and run the server in one step.
- `scripts/smoke.sh`: lightweight curl-based smoke tests against local or remote deployments.
- `scripts/latex/*`: build artifacts required for LaTeX/PDF generation if the template changes.

## API Endpoints

### Health & Metadata

```bash
GET  /health                    # Health check
GET  /version                   # API version
GET  /meta/roles                # Available roles
GET  /meta/modules              # Available modules
```

### Authentication Required

All other endpoints require `Authorization: Bearer <token>` header.

### Core Resources

```bash
# Parties (CRM)
GET    /parties                 # List parties
POST   /parties                 # Create party
GET    /parties/:id             # Get party
PUT    /parties/:id             # Update party
DELETE /parties/:id             # Delete party

# Bookings
GET    /bookings                # List bookings
POST   /bookings                # Create booking
GET    /bookings/:id            # Get booking
PUT    /bookings/:id            # Update booking

# Sessions
GET    /sessions                # List sessions
POST   /sessions                # Create session
GET    /input-list/sessions     # Get sessions input list (PDF)

# Packages
GET    /packages                # List packages
POST   /packages                # Create package
GET    /packages/:id            # Get package

# Invoices & Receipts
GET    /invoices                # List invoices
POST   /invoices                # Create invoice
GET    /receipts                # List receipts
POST   /receipts                # Create receipt

# See docs/openapi/ for complete API specification
```

## Testing

### Smoke Tests

```bash
# Test local server
./scripts/smoke.sh http://localhost:8080

# Test remote deployment
./scripts/smoke.sh https://api.tdfrecords.com
```

### Unit Tests (TODO)

A formal test suite is not yet implemented. To add:

1. Create `test/` directory
2. Add Hspec specs
3. Update `tdf-hq.cabal`:
   ```haskell
   test-suite tdf-hq-test
     type:             exitcode-stdio-1.0
     main-is:          Spec.hs
     hs-source-dirs:   test
     build-depends:    base, hspec, tdf-hq
   ```
4. Run with `stack test`

## Troubleshooting

- If you see database connection errors, verify credentials match a running PostgreSQL instance or use Docker Compose.
- To regenerate seeds from scratch, set `RESET_DB=true` and `SEED_DB=true` for a single `stack run` invocation, then revert to defaults.
- CORS origins can be extended via `ALLOW_ORIGINS` (comma-separated list) or `ALLOW_ORIGIN` environment variables without code changes.

## Contributing

See the main repository's [CONTRIBUTING.md](https://github.com/diegueins680/tdf-app/blob/main/CONTRIBUTING.md) for contribution guidelines.

### Adding New Endpoints

1. Define route in `src/TDF/API.hs`
2. Implement handler in `src/TDF/Server*.hs`
3. Add database models in `src/TDF/Models*.hs` if needed
4. Update OpenAPI spec in `docs/openapi/`
5. Wire into `src/TDF/Server.hs`
6. Update this README if it's a major feature

## Related Projects

- **[tdf-hq-ui](../tdf-hq-ui/)** - React web interface
- **[tdf-mobile](../tdf-mobile/)** - Expo mobile app
- **[Main Repository](https://github.com/diegueins680/tdf-app)** - Complete platform

## License

MIT License - See [LICENSE](../LICENSE) for details

---

**TDF Records** - Built with ❤️ using Haskell, Servant, and PostgreSQL

For questions or issues, please open an issue in the [main repository](https://github.com/diegueins680/tdf-app/issues).
