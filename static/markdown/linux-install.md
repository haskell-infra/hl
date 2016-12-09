## Package-based install

### Ubuntu

Steps to setup ghc and cabal:

    sudo apt-get update
    sudo apt-get install -y software-properties-common
    sudo add-apt-repository -y ppa:hvr/ghc
    sudo apt-get update
    sudo apt-get install -y cabal-install-1.22 ghc-7.10.3
    cat >> ~/.bashrc <<EOF
    export PATH="\$HOME/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH"
    EOF
    export PATH=~/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH

Steps to setup stack are [on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu).

### Debian (jessie)

Steps to setup ghc and cabal:

    echo 'deb http://ftp.debian.org/debian/ jessie-backports main' | sudo tee /etc/apt/sources.list.d/backports.list
    sudo apt-get update && apt-get -t jessie-backports install ghc cabal-install
    cabal update && echo export PATH='$HOME/.cabal/bin:$PATH' >> $HOME/.bashrc

Steps to setup stack are [on the stack website](https://docs.haskellstack.org/en/stable/install_and_upgrade/#debian).

### Fedora 22

GHC 7.8.4 is in the official Fedora repo.

    sudo dnf install ghc
    sudo dnf install cabal-install

Steps to setup stack are
[on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#fedora).

### Fedora 21

To install GHC 7.8.4 from the unofficial repo:

    sudo yum-config-manager --add-repo https://copr.fedoraproject.org/coprs/petersen/ghc-7.8.4/repo/fedora-21/petersen-ghc-7.8.4-fedora-21.repo
    sudo yum install ghc cabal-install

As stated in [petersen/ghc-7.8.4 copr page](https://copr.fedoraproject.org/coprs/petersen/ghc-7.8.4/) this ghc cannot be installed in parallel with Fedora/EPEL ghc.

If you want to install from the official repo that uses an older version of ghc (7.6.x) and cabal-install (1.16.x), just run the install command without badding the unofficial repo.

Steps to setup stack are
[on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#fedora).

### Arch Linux

The official repos on Arch Linux contain packages `ghc`, `cabal-install`, `happy`, `alex`, `haddock`.  Install them with:

    sudo pacman -S ghc cabal-install happy alex haddock

Steps to setup stack are
[on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#arch-linux).

### Generic Tarballs

Generic minimal installers that work on most modern linux
distributions are available via the [Haskell Platform](https://www.haskell.org/platform/linux.html#linux-generic)
