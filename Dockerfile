# stack resolver 24.10 uses ghc 9.10.2 - when upgrading LTS version in stack.yaml, check Haskell version on https://www.stackage.org/ and check which Debian release is available on https://hub.docker.com/_/haskell/
FROM haskell:9.10.2-bullseye as build
RUN mkdir -p /tablebot/build
WORKDIR /tablebot/build

# system lib dependencies
RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3-dev build-essential pkg-config libicu-dev --fix-missing --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY . . 

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /tablebot/build/bin

# ensure this matches first FROM
FROM haskell:9.10.2-slim-bullseye as app

# system runtime deps - if this command fails, check libicu version (https://packages.debian.org/index) and upgrade if necessary
RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3 libicu67 --fix-missing --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /tablebot
WORKDIR /tablebot

COPY --from=build /tablebot/build/bin .
# apparently we need the .git folder
COPY .git .git 
# we need fonts for the roll stats
COPY fonts fonts
# resources folder
COPY resources resources
CMD /tablebot/tablebot-exe
