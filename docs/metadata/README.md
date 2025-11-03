# TDF Metadata Schema

This doc defines the canonical metadata fields for sessions/assets in TDF.

## Core fields
- catalog_id (unique)
- artist_name
- project_title
- session_type (studio | live-session | rehearsal | interview)
- record_date (YYYY-MM-DD)
- release_date (YYYY-MM-DD)
- location
- roles (JSON)
- recording_chain (JSON)
- rights_holder (JSON)
- license_status (string)
- bpm (int)
- key (string)
- genre (string)
- mood (string[])
- asset_links (JSON)
- notes (text)

See seed example and API plan in this repository.
