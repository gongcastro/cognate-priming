build:
	docker build --file=./Dockerfile --build-arg GH_PAT=${GH_PAT} --build-arg ML_PASSWORD=${ML_USER} --build-arg ML_PASSWORD=${ML_PASSWORD} --tag=cognate-priming:1.0.0 .

run: build
	docker run -d -p 8787:8787 \
		-e DISABLE_AUTH=true \
		--name='gapminder-01-ct' \
		-v ${HOME}:/home/rstudio/hostdata \
		gapminder-01;

	sleep 3;
	firefox 127.0.0.1:8787;

stop:
	docker stop cognate-priming

start:
	docker start cognate-priming

remove: stop
	docker rm cognate-priming