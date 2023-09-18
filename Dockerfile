FROM node:20-alpine3.17

RUN apk add zola

CMD ["zola"]
ENTRYPOINT [ "zola" ]
