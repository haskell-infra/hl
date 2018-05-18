## Package-based install

### Ubuntu

Steps to setup ghc and cabal:

    sudo apt-get update
    sudo apt-get install -y software-properties-common
    sudo add-apt-repository -y ppa:hvr/ghc
    sudo apt-get update
    sudo apt-get install -y cabal-install ghc
    cat >> ~/.bashrc <<EOF
    export PATH="\$HOME/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:\$PATH"
    EOF
    export PATH=~/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH

Steps to setup stack are [on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu).

### Debian (jessie)

Steps to setup ghc and cabal:

    echo 'deb http://ftp.debian.org/debian/ jessie-backports main' | sudo tee /etc/apt/sources.list.d/backports.list
    sudo apt-get update && sudo apt-get -t jessie-backports install ghc cabal-install
    cabal update && echo export PATH='$HOME/.cabal/bin:$PATH' >> $HOME/.bashrc

Steps to setup stack are [on the stack website](https://docs.haskellstack.org/en/stable/install_and_upgrade/#debian).

### Fedora

GHC and cabal-install are in the official Fedora repos, to install:

    sudo dnf install ghc cabal-install

- Fedora 26 has ghc-8.0.2 and cabal-install-1.24.0.2
- Fedora 25 has ghc-7.10.3 and cabal-install-1.22.9.0
- Fedora 24 has ghc-7.8.4 and cabal-install-1.18.1.0

There are also unofficial Fedora Copr ghc repos which include cabal-install:

- [petersen/ghc-8.0.2 Copr repo (F24,F25)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.0.2)
- [petersen/ghc-7.10.3 Copr repo (F24)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-7.10.3)

(earlier 7.10.x and 8.0.1 coprs are also available).

Note ghc from Copr cannot be installed in parallel with official Fedora ghc packages.

Steps to setup stack are
[on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#fedora).
You can also obtain stack from the fedora [petersen/stack Copr repo](https://copr.fedoraproject.org/coprs/petersen/stack/)

### EPEL for RHEL/CentOS/etc
- EPEL 7 has ghc-7.6.3 and cabal-install-1.16.1.0
- EPEL 5 and 6 have ghc-7.0.4 and cabal-install-0.10.2

To install these older versions of ghc and cabal-install from the official EPEL repo, just run the install command:

    sudo yum install ghc cabal-install

For newer versions of ghc you can use the unofficial Fedora Copr repos:

- [petersen/ghc-8.0.2 Copr repo (EL7)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.0.2)
- [petersen/ghc-7.10.3 Copr repo (EL7,EL6)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-7.10.3)
- [petersen/ghc-7.8.4 Copr repo (EL7,EL6)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-7.8.4)
- [petersen/ghc-7.4.2 Copr repo (EL6,EL5)](https://copr.fedorainfracloud.org/coprs/petersen/ghc-7.4.2)

Note the ghc packages from Copr cannot be installed in parallel with EPEL ghc.

Steps to setup stack are
[on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#fedora).
You can also obtain stack from the fedora [petersen/stack Copr repo](https://copr.fedoraproject.org/coprs/petersen/stack/)

### Arch Linux

The official repos on Arch Linux contain packages `ghc`, `cabal-install`, `happy`, `alex`, `haddock`.  Install them with:

    sudo pacman -S ghc cabal-install happy alex haddock

Steps to setup stack are
[on the stack website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#arch-linux).

### Generic Tarballs

Generic minimal installers that work on most modern linux
distributions are available via the [Haskell Platform](https://www.haskell.org/platform/linux.html#linux-generic)
