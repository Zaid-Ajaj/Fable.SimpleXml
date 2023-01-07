var path = require("path");

module.exports = function (evn, argv) {
  var mode = argv.mode || "development";
  var isProduction = mode === "production";
  console.log("Webpack mode: " + mode);

  return {
   mode: mode,
   devtool: isProduction ? false : "eval-source-map",
   entry: './sample/App.fs.js',
   output: {
     filename: 'bundle.js',
     path: path.join(__dirname, './sample'),
   },
   devServer: {
     contentBase: './sample',
     port: 8080
   },
   module: {
   }
  };
 }