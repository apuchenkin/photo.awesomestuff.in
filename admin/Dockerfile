FROM node:8-alpine

COPY . /srv
WORKDIR /srv

RUN npm install
ENV NODE_ENV production
RUN npm run build
RUN npm prune