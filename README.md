# vg100-p1-breakout

This is a demo for VG100 project 1 Breakout bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).

Below you will find some information on how to perform basic tasks.
You can find the most recent version of this guide [here](https://github.com/halfzebra/create-elm-app/blob/master/template/README.md).

## Built with

* [Elm](https://elm-lang.org/)
* [Create Elm App](https://github.com/halfzebra/create-elm-app)
* [Elm-mdc](https://github.com/aforemny/elm-mdc)

## Setup

```shell script
npm i -g create-elm-app yarn
yarn
git submodule update --init --recursive
cd elm-mdc && make
cd ..
elm make
```

There is also a `Makefile` provided, so that you can setup and build the project directly.
```
npm i -g create-elm-app yarn
make
```


## Running

```shell script
elm-app start
```
Then you can play the game at  http://localhost:3000/

## Build

```shell script
elm-app build
```
To play the game, run:

```shell script
cd build
python3 -m http.server
```
You can now view it in the browser at http://localhost:8000/.
