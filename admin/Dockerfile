FROM node:alpine

COPY ./admin /srv/admin
COPY ./common /srv/common
COPY ./client /srv/client
WORKDIR /srv/admin

ARG BASENAME
ARG API_ENDPOINT
ARG STATIC_ENDPOINT

ENV BASENAME $BASENAME
ENV API_ENDPOINT $API_ENDPOINT
ENV STATIC_ENDPOINT $STATIC_ENDPOINT

RUN npm i yarn -g
RUN yarn install
ENV NODE_ENV production
RUN yarn run build
RUN yarn install --production
RUN npm rm yarn -g
