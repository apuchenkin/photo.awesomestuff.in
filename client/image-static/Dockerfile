FROM alpine:latest

RUN apk add vips-dev  \
	--update-cache \
	--repository http://dl-cdn.alpinelinux.org/alpine/edge/testing \
	--allow-untrusted

RUN apk add --no-cache make gcc g++ python nodejs

RUN npm i pm2 -g

ADD lib /srv/lib
ADD image-static /srv/image-static
WORKDIR /srv/image-static

RUN npm install
RUN npm run build 

ENTRYPOINT ["pm2-docker", "dist/index.js"]
