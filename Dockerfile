FROM haskell:9.4

WORKDIR /app

COPY . .

RUN stack setup
RUN stack build

EXPOSE 3001

CMD ["stack", "exec", "ndvi-server"]
