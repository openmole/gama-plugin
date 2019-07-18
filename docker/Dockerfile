
FROM openjdk:8-jdk-alpine AS build-openmole-sources

MAINTAINER Romain Reuillon <romain.reuillon@iscpif.fr>, Sebastien Rey-Coyrehourcq <sebastien.rey-coyrehourcq@univ-rouen.fr> Jonathan Passerat-Palmbach <j.passerat-palmbach@imperial.ac.uk>

RUN echo http://nl.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories
RUN apk upgrade --update-cache --available
RUN apk add --update && apk add --no-cache -f gnupg ca-certificates git git-lfs curl sbt nodejs python python-pycurl bash tar gzip ca-certificates-java openssh npm nodejs-legacy tree

WORKDIR /home/root

RUN mkdir -p .ssh && \
    chmod 0700 .ssh

COPY scripts/config .ssh/config

RUN git clone https://gitlab.openmole.org/openmole/gama-plugin.git

WORKDIR /home/root/gama-plugin

COPY scripts/compile.sh .
RUN ["sh", "compile.sh"]

FROM openjdk:8-jdk-alpine AS openmole

RUN echo http://nl.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories
RUN apk upgrade --update-cache --available
RUN apk add --update &&  apk add --no-cache -f ca-certificates ca-certificates-java su-exec shadow bash mlocate

ARG GID
ARG UID

RUN addgroup -g 1000 mole && adduser -h /home/mole -s /bin/sh -D -G mole -u 1000 mole

COPY ./scripts/docker-entrypoint.sh /usr/local/bin

RUN chmod +x /usr/local/bin/docker-entrypoint.sh

WORKDIR /home/mole/

COPY --from=build-openmole-sources --chown=mole:mole home/root/gama-plugin/openmole/openmole/bin/openmole/target/assemble/ .
COPY --from=build-openmole-sources --chown=mole:mole home/root/gama-plugin/get-gama-p2/build/p2asmaven/p2/p2/plugins/ ./gama-plugins

RUN rm /home/mole/gama-plugins/org.eclipse.osgi*
RUN chmod +x openmole

EXPOSE 8443
VOLUME /home/mole/workspace

ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["./openmole", "--mem","8G", "--port", "8443", "--remote", "--workspace", "/home/mole/workspace", "-p", "/home/mole/gama-plugins"]
