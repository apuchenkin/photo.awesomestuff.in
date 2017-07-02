FROM node:alpine

RUN apk add --no-cache mysql-client

RUN npm i yarn -g
RUN npm i pm2 -g

COPY . /srv
WORKDIR /srv

RUN yarn install
RUN yarn run build
ENV NODE_ENV production
ENV PORT 3000
RUN yarn install --production

EXPOSE 3000
ENTRYPOINT ["pm2-docker", "dist/app.js"]
