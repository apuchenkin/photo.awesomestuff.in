FROM alpine:edge

RUN apk add --no-cache nodejs
RUN npm i pm2 -g
RUN npm i gulp -g

ADD . /srv
WORKDIR /srv

RUN npm install
RUN gulp build --release
ENV NODE_ENV production
RUN npm prune

RUN npm uninstall -g gulp

ENTRYPOINT ["pm2-docker", "dist/server.js"]
