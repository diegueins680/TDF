# -------- Stage 1: build with Stack (no GHCR) ----------
FROM haskell:9.6-bullseye AS builder
WORKDIR /app

# System deps to build persistent-postgresql (libpq) + basic tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev pkg-config git curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Stack is already present in this image; print version and continue
RUN stack --version || true

# Copy project definition first for better layer caching
COPY stack.yaml tdf-hq.cabal ./
# If you also have package.yaml, uncomment the next line
# COPY package.yaml ./

# Pre-fetch resolver & deps (cached until these files change)
RUN stack setup && stack build --only-dependencies

# Copy sources
COPY app app
COPY src src
COPY config config

# Build and place binary at /out
RUN stack build --copy-bins --local-bin-path /out

# -------- Stage 2: slim runtime ----------
FROM debian:bookworm-slim
WORKDIR /app

# Runtime libs: libpq for Postgres + CA certs for Neon TLS
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /out/tdf-hq-exe /app/tdf-hq-exe

# Render sets PORT; map it to APP_PORT expected by the app
ENV APP_PORT=8080
CMD ["/bin/sh","-c","APP_PORT=${PORT:-8080} /app/tdf-hq-exe"]
