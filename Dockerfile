# stack resolver 18.18 uses ghc 9.12.2
FROM haskell:9.12.2-bookworm as build
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

FROM haskell:9.12.2-slim-bookworm as app

# system runtime deps
RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3 libicu63 --fix-missing --no-install-recommends && \
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
