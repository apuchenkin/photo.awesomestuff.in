FROM node

RUN apt-get update
RUN apt-get install mysql-client -y

COPY . /srv
WORKDIR /srv

RUN npm i yarn -g
RUN yarn install
RUN yarn run build
ENV NODE_ENV production
ENV PORT 3000
RUN yarn install --production
RUN npm rm yarn -g
RUN npm i pm2 -g
RUN chmod +x ./scripts/wait.sh

VOLUME /static
EXPOSE 3000
ENTRYPOINT ["pm2-docker", "dist/app.js"]
