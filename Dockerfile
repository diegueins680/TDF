# -------- Stage 1: build with Stack ----------
FROM ghcr.io/commercialhaskell/stack:2.15.5 AS builder
WORKDIR /app

# System deps to build persistent-postgresql (libpq)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev pkg-config git && \
    rm -rf /var/lib/apt/lists/*

# Layer caching: copy project files progressively
COPY stack.yaml tdf-hq.cabal ./
# If you have a package.yaml, copy it too
# COPY package.yaml ./

# Pre-build deps
RUN stack setup && stack build --only-dependencies

# Copy sources last for better caching
COPY app app
COPY src src
COPY config config

# Build and place binary at /out
RUN stack build --copy-bins --local-bin-path /out

# -------- Stage 2: slim runtime ----------
FROM debian:bookworm-slim
WORKDIR /app
# Runtime libs (libpq) + certs for TLS (Neon)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /out/tdf-hq-exe /app/tdf-hq-exe

# Render sets PORT; map it to APP_PORT expected by the app
ENV APP_PORT=8080
CMD ["/bin/sh","-c","APP_PORT=${PORT:-8080} /app/tdf-hq-exe"]
