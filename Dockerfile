from debian

EXPOSE 3000

RUN apt update && apt upgrade -y && apt install -y libpq-dev

COPY ./bin/cp2ville /usr/local/bin
COPY ./bin/loaddata /usr/local/bin

CMD /usr/local/bin/cp2ville
