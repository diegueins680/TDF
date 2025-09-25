# -------- Stage 1: build with Haskell + Stack (no GHCR) ----------
FROM haskell:9.6-bullseye AS builder
WORKDIR /app

# System deps to build persistent-postgresql (libpq)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev pkg-config git curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Install Stack (from official script)
RUN curl -sSL https://get.haskellstack.org/ | sh

# Copy project definition files first for better layer caching
COPY stack.yaml tdf-hq.cabal ./
# If you have package.yaml, copy it too:
# COPY package.yaml ./

# Pre-fetch resolver and deps (caches if unchanged)
RUN /root/.local/bin/stack setup && \
    /root/.local/bin/stack build --only-dependencies

# Copy sources
COPY app app
COPY src src
COPY config config

# Build and place binary at /out
RUN /root/.local/bin/stack build --copy-bins --local-bin-path /out

# -------- Stage 2: slim runtime ----------
FROM debian:bookworm-slim
WORKDIR /app

# Runtime libs: libpq for Postgres + CA certs for Neon TLS
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /out/tdf-hq-exe /app/tdf-hq-exe

# Render provides PORT; map it to APP_PORT expected by the app
ENV APP_PORT=8080
CMD ["/bin/sh","-c","APP_PORT=${PORT:-8080} /app/tdf-hq-exe"]
