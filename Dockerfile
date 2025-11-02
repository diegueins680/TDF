# syntax=docker/dockerfile:1
FROM haskell:9 as build
RUN apt-get update && apt-get install -y texlive-latex-extra latexmk mustache
WORKDIR /app
COPY . .
# Assuming cabal project exists; you may adapt to stack
RUN cabal update && cabal build --only-dependencies && cabal build

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y texlive-latex-extra latexmk mustache && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=build /app /app
EXPOSE 8080
CMD ["/app/dist-newstyle/build/x86_64-linux/ghc-9/tdf-exe/x/tdf-exe/build/tdf-exe/tdf-exe"]
