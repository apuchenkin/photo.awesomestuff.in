FROM alpine:latest
RUN apk add --no-cache make gcc g++ python nodejs
RUN apk add --update \
  --repository https://s3.amazonaws.com/wjordan-apk \
  --allow-untrusted \
  libvips-dev

RUN npm i pm2 -g
WORKDIR /srv
ADD . /srv
run npm install
RUN npm run build 
ENTRYPOINT ["pm2-docker", "dist/index.js"]
