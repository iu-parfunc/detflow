
TAG=image-0.7

docker: image
image:
	docker build -t parfunc/detmonad:$(TAG) .


.PHONY: image docker
