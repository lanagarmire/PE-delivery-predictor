VERfile="VERSION.txt"
GBOXfile="BASE_NAME.txt"
VER=`cat $(VERfile)`
GBOX=`cat $(GBOXfile)`:$(VER)
export

docker:
	docker build -t $(GBOX) .

docker-push:
	docker push $(GBOX)

shell:
	docker run --rm -it $(GBOX) /bin/bash
