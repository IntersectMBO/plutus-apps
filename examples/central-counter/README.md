# How to build

## Build the server

* `stack setup`
* `stack build`

## Generate the purescript code for the servant API

* `stack exec PSGenerator`

## Build the frontend

* `cd frontend`
* Install npm
* Install bower and pulp via npm
* Run `npm install`
* Run `npm run build`

## Run the server

* `stack exec central-counter`
* Visit http://localhost:8081/index.html in multiple browser windows and click `+` and `-`.
* Have fun!
