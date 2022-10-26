FROM amazonlinux:2.0.20221004.0

# dependencies
RUN yum -y install tar xz perl gcc make gmp gmp-devel zlib zlib-devel pkgconfig

# ghc and cabal
RUN mkdir -p $HOME/.ghcup/bin \
    && curl -sL https://downloads.haskell.org/ghcup/0.1.18.0/x86_64-linux-ghcup-0.1.18.0 > $HOME/.ghcup/bin/ghcup \
    && chmod a+x $HOME/.ghcup/bin/ghcup \
    && $HOME/.ghcup/bin/ghcup install ghc 9.2.4 || (cat $HOME/.ghcup/logs/*.* && false) \
    && $HOME/.ghcup/bin/ghcup install cabal 3.6.2.0 || (cat $HOME/.ghcup/logs/*.* && false) \
    && ln -s $($HOME/.ghcup/bin/ghcup whereis ghc) $HOME/.ghcup/bin/ghc
# ^ The above creation of ghc symlink seems like hack, but ghcup doesn't make ghc available on the path

ENV PATH=/root/.ghcup/bin:/root/.cabal/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# cabal-plan
RUN mkdir -p $HOME/.cabal/bin \
    && curl -sL https://github.com/haskell-hvr/cabal-plan/releases/download/v0.6.2.0/cabal-plan-0.6.2.0-x86_64-linux.xz > cabal-plan.xz \
    && echo 'de73600b1836d3f55e32d80385acc055fd97f60eaa0ab68a755302685f5d81bc  cabal-plan.xz' | sha256sum -c - \
    && xz -d < cabal-plan.xz > $HOME/.cabal/bin/cabal-plan \
    && rm -f cabal-plan.xz \
    && chmod a+x $HOME/.cabal/bin/cabal-plan

RUN ghc --version \ 
    && cabal --version \ 
    && cabal-plan --version \
