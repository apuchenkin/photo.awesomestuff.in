FROM node:alpine

COPY ./admin /srv/admin
COPY ./common srv/common
COPY ./client srv/client

ARG BASENAME
ARG API_ENDPOINT
ARG STATIC_ENDPOINT

ENV BASENAME $BASENAME
ENV API_ENDPOINT $API_ENDPOINT
ENV STATIC_ENDPOINT $STATIC_ENDPOINT

WORKDIR /srv/admin

RUN npm i yarn -g
RUN yarn install
RUN yarn run build
ENV NODE_ENV production
RUN yarn install --production
RUN npm rm yarn -g

VOLUME /static
VOLUME /srv/admin/dist
