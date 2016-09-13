from haskell

EXPOSE 3000

# RUN apt update && apt upgrade -y && apt install -y libpq-dev

COPY ./scripts/loaddata.hs /usr/local/bin/loaddata
COPY ./Main.hs /usr/local/bin/Main.hs

CMD /usr/local/bin/Main.hs
