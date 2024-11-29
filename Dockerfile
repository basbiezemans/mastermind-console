FROM haskell:8

WORKDIR /usr/src/app

COPY . .

RUN stack install

CMD ["mastermind-console"]