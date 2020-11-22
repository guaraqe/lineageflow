.DEFAULT_GOAL := help

################################################################################
# Server

all: server client ## Build the server and the client

server: ## Built the server
	nix-shell --command "echo Done"

.PHONY: server

client: ## Build the client
	nix-shell client.nix -A shells.ghcjs --command \
	"cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build lineageflow-client"

.PHONY: client

run-server: ## Run the server
	nix-shell --command "lf-server $$LINEAGEFLOW_DATABASE"

.PHONY: run-server

run-client: ## Run the client
	nix-shell -p chromium --command \
	"chromium-browser --disable-web-security --user-data-dir dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/lineageflow-client-0.1.0/c/lf-client/build/lf-client/lf-client.jsexe/index.html &>/dev/null"

.PHONY: run-client

################################################################################
# Help

help: ## Print the list of commands.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help
