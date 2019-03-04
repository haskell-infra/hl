Outdated Repository
=====
You probably want the new repository at https://github.com/haskell-infra/www.haskell.org


hl
=====

Haskell.org web site.

## Editing pages

If you're just here to contribute a content change, read this section.

All pages that are produced by markdown
[are here](https://github.com/haskell-infra/hl/tree/master/static/markdown). To
contribute changes simply fork this repo and open a pull request. It
will be merged and redeployed in short order.

If you want to edit a page which has some custom code in it, you'll
want to see the next sections for building and running
instructions. Maybe also take a look at [architecture](#architecture).

If you want to include Haskell code samples in markdown, use:

    ``` haskell
    main = putStrLn "Hello, World!"
    ```

If you want to include Haskell code samples in Haskell code pages,
use:

``` haskell
haskellPre "main = print 123"
haskellCode "peyton `simon` jones"
```

Pre for `<pre>` block, code for `<code>` span snippet.

## Building

Clone the repo:

    $ git clone git@github.com:haskell-infra/hl.git

Set up sandbox:

    $ cabal sandbox init

Install dependencies and build:

    $ cabal install --only-dependencies
    $ cabal build

Done!

### CSS Assets

In `static/source`, run:

    $ npm install
    $ bower install

to install all the source libraries locally (you need [nodejs](https://nodejs.org) installed). Then:

    $ gulp css

to create a new CSS file (automatically creates a minified version as well).

## Running

(Note: from within a development env where you run out of the dist folder, you want to ensure that `development` in src/HL/Development.hs is set to "True").

It runs at: http://localhost:1990/

Manually running the binary:

    $ dist/build/hl/hl

Running from inside GHCi:

    > :l DevelMain
    > DevelMain.update

Run this every time you want to update the web handler in-place, as in
[this demo](https://github.com/chrisdone/ghci-reload-demo).

If you use Emacs, you can just bind it to a key:

``` lisp
(define-key html-mode-map [f12] 'haskell-process-reload-devel-main)
```

Just hit f12 to recompile and restart.

## Architecture

It uses Yesod and an MVC organization.

* HL.Model.* -- [models](https://github.com/haskell-infra/hl/tree/master/src/HL/Model)
* HL.View.* -- [views](https://github.com/haskell-infra/hl/tree/master/src/HL/View)
* HL.Controller.* -- [controllers](https://github.com/haskell-infra/hl/tree/master/src/HL/Controller)

Templates are written in
[Lucid](https://github.com/chrisdone/lucid). There is presently no
database.

## Style

We generally use
[this style](https://github.com/chrisdone/haskell-style-guide)
for this project. You don't have to use this style, we can reformat patches as
they come in.
