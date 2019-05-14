exports.argv = process.argv;

exports._processExit = function(code) {
  return function() {
    process.exit(code);
  };
};
