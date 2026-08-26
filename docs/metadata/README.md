# TDF Metadata Schema

This document describes the legacy `metadata_records` schema created by
[`db/migrations/2025-11-02_add_metadata_records.sql`](../../db/migrations/2025-11-02_add_metadata_records.sql).
Apply the migration through the repository's normal migration workflow; do not
run the SQL ad hoc against production.

## Core fields

- `catalog_id` (unique)
- `artist_name`
- `project_title`
- `session_type` (`studio`, `live-session`, `rehearsal`, or `interview`)
- `record_date` and `release_date`
- `location`
- `roles`, `recording_chain`, `rights_holder`, and `asset_links` (`JSONB`)
- `license_status`
- `bpm`
- `key`
- `genre`
- `mood` (`TEXT[]`)
- `notes`

The migration also creates lookup indexes and maintains `updated_at` with a
database trigger. Treat the migration itself as the source of truth when this
summary and the executable schema differ.
