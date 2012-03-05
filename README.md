If you run this command, you can easily get a failure:

   cabal install foo && cabal install bar

 Whereas if you run this command it should almost always work:

   cabal install foo bar

cabal-meta facilitates installing everything at once.
This is *very* useful when you want cabal to install local directories, non-hackage sources, or packages otherwise not listed in your cabal file.

When invoked, it looks for a file `sources.txt`.
Each line of `sources.txt` is either a hackage package or a directory.
A directory is either a local cabal package or contains another `sources.txt` to recurse into.
cabal-meta automatically uses cabal-src-install for local packages (please install the cabal-src package).


# Example

To build a Yesod web application using the source from github, I have a sources.txt in my project consisting of

    ./
    sphinx -fone-one-beta
    path/to/yesod/sources

`./` means the current project

`sphinx` is a hackage package, and I have a build flag next to it. *warning*: a packge build flag will end up being applied to *all* packages

`path/to/yesod/sources` contains a `sources.txt` with:

    hamlet
    persistent
    wai
    yesod

Each of these directories has a `sources.txt` listing several dirctories containing cabal packages that will be installed.
Confused? It is just recursion, although we are interleaving IO :)


# TODO

* support github: this lets us do beta releases
