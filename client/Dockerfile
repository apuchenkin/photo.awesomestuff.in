FROM node

ARG HOSTNAME
ARG ANALYTICS
ARG API_ENDPOINT
ARG STATIC_ENDPOINT

ENV PORT 3000
EXPOSE 3000

COPY ./client /srv/client
COPY ./common /srv/common
WORKDIR /srv/client
VOLUME /srv/client/dist/static

RUN npm install
RUN npm run build
ENV NODE_ENV production
RUN npm prune

RUN npm i pm2 -g
ENTRYPOINT ["pm2-docker", "dist/server.js"]
