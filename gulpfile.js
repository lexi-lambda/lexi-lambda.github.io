require('coffeescript/register');
// Patch coffeescript/register to enable transpilation
const coffee = require.extensions['.coffee'];
require.extensions['.coffee'] = (module, filename) => {
  if (!module.options) {
    module.options = {};
  }
  if (!module.options.transpile) {
    module.options.transpile = { filename };
  }
  return coffee(module, filename);
}
module.exports = require('./gulpfile.coffee');
