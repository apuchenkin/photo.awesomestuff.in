FROM node

RUN npm i yarn -g
RUN npm i pm2 -g

ARG HOSTNAME
ARG ANALYTICS
ARG API_ENDPOINT
ARG STATIC_ENDPOINT

COPY ./client /srv/client
COPY ./common /srv/common
WORKDIR /srv/client

RUN yarn install
RUN yarn run build
ENV NODE_ENV production
ENV PORT 3000
RUN yarn install --production

EXPOSE 3000
VOLUME /srv/client/dist/static
ENTRYPOINT ["pm2-docker", "dist/server.js"]
