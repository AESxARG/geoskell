FROM haskell:9.6

WORKDIR /app

COPY Geoskell.hs .

CMD ["runghc", "Geoskell.hs"]