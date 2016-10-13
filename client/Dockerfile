FROM alpine:edge

RUN apk add --no-cache nodejs
RUN npm i pm2 -g
RUN npm i gulp -g

ADD . /srv
WORKDIR /srv

RUN npm install
RUN gulp build --release

ENTRYPOINT ["pm2-docker", "dist/server.js"]
