### 1. Install GHC

GHC has its own web site with license information, FAQ, download links
and changelogs. Depending on your operating system, there should be a
package made for its package manager, otherwise (e.g. Windows) it will
be an installer.

You can also download the .tar.gz/.zip and unpack and install the
executables and so forth manually.

Or you can even install from source, for which
[there is documentation](https://ghc.haskell.org/trac/ghc/wiki/Building).

[Download GHC now →](https://www.haskell.org/ghc/download)

### 2. Install Cabal-install

After installing GHC, you will want the Haskell package manager:

[Get the Cabal archive →](http://hackage.haskell.org/package/cabal-install)

Download the tar.gz file, extract and inside the resulting directory,
run:

    $ sh ./bootstrap.sh

This will automatically download and install all the packages
necessary to setup Cabal install.

Once complete, you should add `$HOME/.cabal/bin` to your PATH. A
simple way to do this is to edit your `~/.bashrc` and place in there:

    export PATH=$HOME/.cabal/bin:$PATH

Now you should be able to run cabal:

    $ cabal --version
    cabal-install version 1.18.0.2
    using version 1.18.1.2 of the Cabal library

You can now update your package set:

    $ cabal update

And install packages into a sandbox so it doesn't conflict with other projects:

    $ mkdir my-project
    $ cd my-project
    $ cabal sandbox init
    $ cabal install the-package-name
