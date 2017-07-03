FROM node:alpine

RUN apk add vips-dev --update-cache --repository https://dl-3.alpinelinux.org/alpine/edge/testing/

RUN apk add --no-cache make gcc g++ python fftw-dev

COPY ./static-server /srv/app
COPY ./common srv/common
WORKDIR /srv/app

RUN npm i yarn -g
RUN yarn install
RUN yarn run build
ENV NODE_ENV production
ENV PORT 3000
RUN yarn install --production
RUN npm rm yarn -g
RUN npm i pm2 -g

VOLUME /static
EXPOSE 3000
ENTRYPOINT ["pm2-docker", "dist/index.js"]
