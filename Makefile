.PHONY: build run docker-build docker-run smoke

build:
	cabal build

run:
	cabal run

docker-build:
	docker build -t tdf-backend .

# Monta templates/ y public/ en el contenedor para desarrollar más cómodo
docker-run:
	docker run -p 8080:8080 -v $(PWD)/templates:/app/templates -v $(PWD)/public:/app/public --name tdf-backend --rm tdf-backend

smoke:
	BACKEND_URL=http://localhost:8080 bash scripts/smoke.sh
