In Haskell, packages are configured and built with the Cabal package system built into GHC (and other compilers). For more specific details, see [The Cabal User Guide](https://www.haskell.org/cabal/users-guide/). The command line tools to download and install packages are either `cabal` or `stack`, each having different workflows. For details on their usage, see the documentation above.

### Hackage

Hackage is a repository of packages to which anyone can freely upload at any time. The packages are available immediately and documentation will be generated and hosted there. It can be used by cabal install.

You can install a package using cabal by running:

    $ cabal update
    $ cabal install the-package

Note that if you are not in a sandbox, this will install the package globally, which is often not what you want, so it is recommended to set up sandboxes in your project directory by running `cabal sandbox init`.

[Go to Hackage →](https://hackage.haskell.org/packages/)

### LTS Haskell

LTS Haskell is a stackage-based long-term support set of packages which build and pass tests together, with backported bug fixes.

[Get LTS Haskell →](http://www.stackage.org/lts)

### Stackage Nightly

Stackage is a nightly generated stable repository of snapshots of package sets in which only packages which build and pass tests together are bundled together into a snapshot.

[Get Stackage Nightly →](http://www.stackage.org/nightly)

### From source control repositories

Installing from a source repository is also possible. For example, to clone and install the network package from source, you would run:

    $ git clone https://github.com/haskell/network
    $ cabal install network/

Or:

    $ git clone https://github.com/haskell/network
    $ cd network
    $ cabal install

<br>
