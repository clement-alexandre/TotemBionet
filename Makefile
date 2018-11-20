IMAGE_NAME="totembionet"
CONTAINER_NAME="totembionet"

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build: ## Build the container
	docker build -t $(CONTAINER_NAME) .

run: ## Run the container
	docker run -d --name $(CONTAINER_NAME) -p 8888:8888 $(IMAGE_NAME)

up: build run ## Build and Run the container

stop: ## Stop and delete the container
	docker stop $(CONTAINER_NAME); docker rm $(CONTAINER_NAME)

remove: ## Remove the image
	docker rmi $(IMAGE_NAME)

clean: clean-build clean-pyc ## Remove compiled files

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