# cabal-meta

cabal-meta is a cabal wrapper that facilitates:

* installing repos not on hackage (local or remote repos)
* specifying build flags
* ensuring that if possible, an install will succeed

Lets explain the last point:
If you run this command, you can easily get a failure:

    cabal install foo && cabal install bar

Whereas if you run this command it should almost always work:

    cabal install foo bar

cabal-meta facilitates the second command: installing everything at once.
This is *very* useful when you want cabal to install packages not on hackage. You need not worry about how cabal interprets your version, instead you let cabal easily interpret a location.

When invoked, cabal-meta looks for a file `sources.txt`.
Each line of `sources.txt` is either a hackage package, a directory, or a github repo (which is cloned into a vendor/ directory).
A directory is either a local cabal package or contains another `sources.txt` to recurse into.

cabal-meta automatically uses cabal-src-install (if you have it installed) unless you are using cabal-dev (--dev option).
cabal-src-install is used to add local packages to your cabal package database: please see cabal-src documentation.
Please note that this is done after the package install. If there is a failure anywhere along the way, cabal-src-install will not be used.


## Usage

Run `cabal-meta` to see help output. Normal usage:

    cabal-meta install

cabal-dev support:

    cabal-meta --dev install

You can also supply arguments in an environment variable or a configuration file.


## Examples

Controlling cabal-meta is done through `sources.txt`.
`sources.txt` contains a repo location and optionally a branch name and build flags.
Build flags start with a dash.


    git://github.com/foo/bar ghc-7.6-compat -flag


### Darcsden

    darcs:http://darcs.net
    darcs:http://hub.darcs.net/simon/darcsden

### Yesod from github

To build a Yesod application using the latest code, create a sources.txt in the project directory with:

    ./
    https://github.com/yesodweb/yesod
    https://github.com/yesodweb/shakespeare
    https://github.com/yesodweb/persistent
    https://github.com/yesodweb/wai

Now just run: `cabal-meta install`

### Yesod from local

To build a Yesod web application using my already downloaded source from github, I have a sources.txt in my project consisting of

    ./
    sphinx -fone-one-beta
    path/to/yesod/sources

`./` refers to the current directory and thus your current project.

`sphinx` is a hackage package, and I have a build flag next to it that I don't have to worry about forgetting anymore. *warning*: a packge build flag will end up being applied to *all* packages

### Recursion of sources.txt

In the example above, `path/to/yesod/sources` contains a `sources.txt` with:

    ./hamlet
    ./persistent
    ./wai
    ./yesod

Each of these directories has a `sources.txt` listing several dirctories containing cabal packages that will be installed.
Confused? It is just recursion, although we are interleaving IO :)
