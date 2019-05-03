FROM node

RUN apt-get update
RUN apt-get install mysql-client -y

COPY . /srv
WORKDIR /srv

RUN npm install
RUN npm run build
ENV NODE_ENV production
ENV PORT 3000
ENV TYPEORM_HOST db
RUN npm install

EXPOSE 3000
ENTRYPOINT ["npm", "run", "start:prod"]
