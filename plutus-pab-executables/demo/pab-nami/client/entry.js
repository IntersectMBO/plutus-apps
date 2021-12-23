/*eslint-env node*/
/*global global*/
import './static/main.css';

import('./src/Main.purs')
  .then(m => m.main())
  .catch(err => console.log(err))
