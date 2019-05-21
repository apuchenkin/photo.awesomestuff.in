FROM node:slim
RUN apt-get update
RUN apt-get install mysql-client -y

COPY . /srv
WORKDIR /srv

RUN npm install
RUN npm run build
ENV NODE_ENV production
ENV PORT 3000
ENV TYPEORM_HOST db

VOLUME /srv/static
EXPOSE 3000
ENTRYPOINT ["npm", "run", "start:prod"]
