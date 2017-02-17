# How to build

* Install npm
* Install bower and pulp via npm
* Run `npm install` in the frontend directory
* Run `bower install` in the frontend directory
* Run `pulp browserify --to dist/app.js` in the frontend directory
* `cd ..` (central-counter directory)
* `stack build`
* `stack exec central-counter`
* Visit http://localhost:8081/index.html in multiple browser windows and click `+` and `-`.
* Have fun!
