FROM ubuntu

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 \
    && echo 'deb http://download.fpcomplete.com/debian jessie main'| tee /etc/apt/sources.list.d/fpco.list \
    && apt-get update &&  apt-get install stack -y
RUN stack setup --allow-different-user --compiler ghc-7.10.3
RUN apt-get install nodejs -y \
    && ln -s /usr/bin/nodejs /usr/bin/node
USER root
RUN  echo "compiler: ghcjs-0.2.0.820160417_ghc-7.10.3" >> stack.yaml \
     && echo "resolver: nightly-2016-04-17 " >> stack.yaml \
     && echo "compiler-check: match-exact" >> stack.yaml \
     && echo "setup-info:"   >> stack.yaml \
     && echo "  ghcjs:" >> stack.yaml \
     && echo "    source:" >> stack.yaml \
     && echo "      ghcjs-0.2.0.820160417_ghc-7.10.3:" >> stack.yaml \
     && echo "         url: \"https://tolysz.org/ghcjs/nightly-2016-04-17-820160417.tar.gz\""  >> stack.yaml \
     && cat stack.yaml
RUN  chown -R root /root
RUN  stack setup --allow-different-user



