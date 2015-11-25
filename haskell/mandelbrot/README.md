Install ghc and cabal
    sudo apt-get update
    sudo apt-get install -y software-properties-common
    sudo add-apt-repository -y ppa:hvr/ghc
    sudo apt-get update
    sudo apt-get install -y cabal-install-1.22 ghc-7.10.3
    cat >> ~/.bashrc <<EOF
    export PATH="\$HOME/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.10.3/bin:\$PATH"
    EOF
    export PATH=~/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH


Install the following packages with apt:  libglib2.0-dev libcairo2-dev libpango1.0-dev libgtk2.0-dev
Install with cabal: gtk2hs-buildtools gtk
