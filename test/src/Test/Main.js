// module Test.Main

exports.inPhantom = function(action) {
    try {
        action();
    }
    finally {
        if (typeof phantom !== 'undefined') {
            phantom.exit();
        }
        else if (typeof process !== 'undefined') {
            process.exit();
        }
    }
};

