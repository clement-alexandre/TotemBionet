IMAGE_NAME="totembionet"
CONTAINER_NAME="totembionet"

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build: ## Build the container
	docker build -t $(CONTAINER_NAME) totembionet

run: ## Run the container
	docker-compose up -d

up: build run ## Build and Run the container

stop: ## Stop and delete the container
	docker stop $(CONTAINER_NAME); docker rm $(CONTAINER_NAME)

remove: ## Remove the image
	docker rmi $(IMAGE_NAME)

test: ## Execute tests
	python -m unittest discover -s totembionet/src

clean: clean-build clean-pyc clean-hooks clean-backend clean-gv ## Remove compiled files

clean-build:
	rm -fr **/build/
	rm -fr **/dist/
	rm -fr **/.eggs/
	find . -name '*.egg-info' -exec rm -fr {} +
	find . -name '*.egg' -exec rm -f {} +

clean-pyc:
	find . -name '*.pyc' -exec rm -f {} +
	find . -name '*.pyo' -exec rm -f {} +
	find . -name '*~' -exec rm -f {} +
	find . -name '__pycache__' -exec rm -fr {} +

clean-hooks:
	find . -name '*.coverage' -exec rm -f {} +

clean-backend:
	rm -fr backendservices/**/target/

clean-gv:
	find . -name '*.gv' -exec rm -f {} +
	find . -name '*.gv.*' -exec rm -f {} +
