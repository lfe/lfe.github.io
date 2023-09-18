FROM node:20-alpine3.17

RUN apk add zola
RUN npm install -g npm@10.1.0
RUN yarn add tailwindcss@latest @tailwindcss/typography

CMD ["zola"]
ENTRYPOINT [ "zola" ]
