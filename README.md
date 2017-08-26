# anler.me #

My personal website https://anler.me

## Setup ##

If you want to run it locally you first need to install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and a [Less Compiler](https://lesscss.org) with:

```shell
curl -sSL https://get.haskellstack.org/ | sh
npm i -g less
```

Then build the project with:

``` shell
stack build --install-ghc
```

And then build the site with:

``` shell
stack exec site -- build # or watch, server, ...
```
