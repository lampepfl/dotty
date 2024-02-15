FROM ubuntu:22.04

# The default locale is "POSIX" which is just ASCII.
ENV LANG C.UTF-8
ENV DEBIAN_FRONTEND noninteractive
ENV TZ Europe/Zurich

# Add packages to image, set default JDK version
RUN apt-get update && \
    apt-get upgrade -y && \
    # Use a PPA to get Java 17
    apt-get install -y software-properties-common && add-apt-repository ppa:openjdk-r/ppa && \
    apt-get install -y bash curl git ssh htop openjdk-8-jdk-headless openjdk-17-jdk-headless \
                       openjdk-21-jdk-headless nano vim-tiny zile && \
    (curl -fsSL https://deb.nodesource.com/setup_18.x | bash -) && \
    apt-get install -y nodejs


# Install sbt
ENV SBT_HOME /usr/local/sbt
ENV PATH ${SBT_HOME}/bin:${PATH}
ENV SBT_VERSION 1.9.0
RUN curl -sL "https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz" | gunzip | tar -x -C /usr/local