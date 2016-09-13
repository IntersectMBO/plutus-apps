# How to build

* Install npm
* Install bower and pulp via npm
* Run `npm install react react-dom` in the project directory
* Run `bower install` in the frontend directory
* Run `pulp browserify --to dist/app.js`
* `cd ..` (central-counter directory)
* `stack build`
* `stack exec central-counter`
* Visit http://localhost:8081/index.html in multiple browser windows and clickt `+` and `-`.
* Have fun!
