/*eslint-env node*/
/*global global*/
import './static/main.css';

import('./output/Main')
  .then(m => m.main())
  .catch(err => console.log(err))
