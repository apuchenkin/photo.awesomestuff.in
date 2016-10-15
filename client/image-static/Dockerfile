FROM alpine:edge

RUN apk add vips-dev  \
	--update-cache \
	--repository http://dl-cdn.alpinelinux.org/alpine/edge/testing \
	--allow-untrusted

RUN apk add --no-cache make gcc g++ python nodejs
RUN apk cache clean
RUN rm -rf /var/cache/apk/*

ADD lib /srv/lib
ADD image-static /srv/image-static
WORKDIR /srv/image-static
VOLUME /srv/image-static/static

RUN ls -la
RUN npm install
RUN npm run build
ENV NODE_ENV production
RUN npm prune

RUN npm i pm2 -g
ENTRYPOINT ["pm2-docker", "dist/index.js"]
