FROM node

ARG HOSTNAME
ARG ANALYTICS
ARG API_ENDPOINT
ARG STATIC_ENDPOINT

ENV HOSTNAME $HOSTNAME
ENV ANALYTICS $ANALYTICS
ENV API_ENDPOINT $API_ENDPOINT
ENV STATIC_ENDPOINT $STATIC_ENDPOINT

ENV PORT 3000
EXPOSE 3000

COPY ./client /srv/client
COPY ./common /srv/common
WORKDIR /srv/client

RUN npm i yarn -g
RUN yarn install
ENV NODE_ENV production
RUN yarn run build
RUN yarn install --production
RUN npm rm yarn -g

VOLUME /srv/client/dist/assets
RUN npm i pm2 -g
ENTRYPOINT ["pm2-docker", "dist/server.js"]
