If you run this command, you can easily get a failure:

   cabal install foo && cabal install bar

 Whereas if you run this command it should almost always work:

   cabal install foo bar

cabal-meta facilitates installing everything at once.
When invoked, it looks for a file `sources.txt`.
Each line of `sources.txt` is either a directory to install or contains another `sources.txt` to recurse into.
cabal-meta won't continue to recurse if a cabal file is found.
cabal-meta automatically uses cabal-src-install also.


# Example

To build a Yesod web application using the source from github, I have a sources.txt in my project consisting of

    ./
    path/to/yesod/sources

`./` means the current project
`path/to/yesod/sources` contains a `sources.txt` with:

    hamlet
    persistent
    wai
    yesod

Each of these directories have a `sources.txt` listing several dirctories containing cabal packages.

Confused? It is just recursion, although we are interleaving IO :)


# TODO

* support github and tar.gz urls: this lets us do beta releases and work with hackage
