FROM heroku/cedar

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 \
    && echo 'deb http://download.fpcomplete.com/debian jessie main'| tee /etc/apt/sources.list.d/fpco.list \
    && apt-get update &&  apt-get install stack -y
RUN stack setup --allow-different-user --compiler ghc-7.10.2
RUN apt-get install nodejs -y \
    && ln -s /usr/bin/nodejs /usr/bin/node
RUN stack setup --allow-different-user --compiler ghcjs-0.1.0.20150924_ghc-7.10.2


