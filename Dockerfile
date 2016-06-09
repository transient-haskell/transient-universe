FROM heroku/cedar

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 \
    && echo 'deb http://download.fpcomplete.com/debian jessie main'| tee /etc/apt/sources.list.d/fpco.list \
    && apt-get update &&  apt-get install stack -y
RUN stack setup --allow-different-user --compiler ghc-7.10.3
RUN apt-get install nodejs -y \
    && ln -s /usr/bin/nodejs /usr/bin/node
USER root
RUN  echo  "allow-different-user: true" > stack.yaml \
     && echo "compiler: ghcjs-0.2.0.20160414_ghc-7.10.3" >> stack.yaml \
     && echo "resolver: lts-6.0 " >> stack.yaml \
     && echo "compiler-check: match-exact" >> stack.yaml \
     && echo "setup-info:"   >> stack.yaml \
     && echo "  ghcjs:" >> stack.yaml \
     && echo "    source:" >> stack.yaml \
     && echo "      ghcjs-0.2.0.20160414_ghc-7.10.3:" >> stack.yaml \
     && echo "         url: https://s3.amazonaws.com/ghcjs/ghcjs-0.2.0.20160414_ghc-7.10.3.tar.gz"  >> stack.yaml \
     && echo "         sha1: 6d6f307503be9e94e0c96ef1308c7cf224d06be3"  >> stack.yaml \
     && cat stack.yaml \
     && chnow -R root /root/.stack/* \
     && stack setup --allow-different-user   --compiler ghcjs-0.2.0.20160414_ghc-7.10.3



