FROM node

ENV http_proxy http://proxy.merann.ru:8080
ENV https_proxy http://proxy.merann.ru:8080

RUN apt-get update
RUN apt-get install mysql-client -y

COPY . /srv
WORKDIR /srv

RUN npm install
RUN npm run build
ENV NODE_ENV production
ENV PORT 3000
ENV TYPEORM_HOST db
RUN npm prune

EXPOSE 3000
ENTRYPOINT ["npm", "run", "start:prod"]
