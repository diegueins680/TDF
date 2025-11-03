# Contracts API (proposal)

**POST /contracts**: create from schema+payload -> stores row in contracts table.
**GET /contracts/{id}/pdf**: render LaTeX template -> PDF.
**POST /contracts/{id}/send**: email with PDF attachment.

DB: contracts(id, kind, json_payload, pdf_url, created_at).

Implementation notes (Haskell/Servant):
- Endpoint stubs, use Pandoc or tectonic for LaTeX compile.
- S3/GCS upload for PDFs.
- Email via SendGrid or SMTP.
