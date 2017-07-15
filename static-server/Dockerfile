FROM node

# RUN apk add vips-dev --update-cache --repository https://dl-3.alpinelinux.org/alpine/edge/testing/
# RUN apk add --no-cache make gcc g++ python fftw-dev
# RUN apt-get update
# RUN apt-get install vips-dev make gcc g++ python

COPY ./static-server /srv/app
COPY ./common /srv/common
WORKDIR /srv/app

RUN yarn install
RUN yarn run build
ENV NODE_ENV production
ENV PORT 3000
RUN yarn install --production
RUN npm i pm2 -g

VOLUME /static
EXPOSE 3000
ENTRYPOINT ["pm2-docker", "dist/index.js"]
