// jQuery extension for running multiple async methods in parallel
// and getting a callback with all results when all of them have completed.
//
// Each worker is a function that takes a callback as its only argument, and
// fires up an async process that calls this callback with its result.
//
// Example:
//      $.parallel(
//          function (callback) { $.get("form.htm", {}, callback, "html"); },
//          function (callback) { $.post("data.aspx", {}, callback, "json"); },
//          function (formHtml, dataJson) { 
//              // Handle success; each argument to this function is 
//              // the result of correlating ajax call above.
//          }
//      );

(function ($) {

    $.parallel = function (anyNumberOfWorkers, allDoneCallback) {

    var workers = [];
    var workersCompleteCallback = null;

    // To support any number of workers, use "arguments" variable to
    // access function arguments rather than the names above.
    var lastArgIndex = arguments.length - 1;
    $.each(arguments, function (index) {
        if (index == lastArgIndex) {
            workersCompleteCallback = this;
        } else {
            workers.push({ fn: this, done: false, result: null });
        }
    });

    // Short circuit this edge case
    if (workers.length == 0) {
        workersCompleteCallback();
        return;
    }

    // Fire off each worker process, asking it to report back to onWorkerDone.
    $.each(workers, function (workerIndex) {
        var worker = this;
        var callback = function () { onWorkerDone(worker, arguments); };
        worker.fn(callback);
    });

    // Store results and update status as each item completes.
    // The [0] on workerResultS below assumes the client only needs the first parameter
    // passed into the return callback. This simplifies the handling in allDoneCallback,
    // but may need to be removed if you need access to all parameters of the result.
    // For example, $.post calls back with success(data, textStatus, XMLHttpRequest).  If
    // you need textStatus or XMLHttpRequest then pull off the [0] below.
    function onWorkerDone(worker, workerResult) {
        worker.done = true;
        worker.result = workerResult[0]; // this is the [0] ref'd above.
        var allResults = [];
        for (var i = 0; i < workers.length; i++) {
            if (!workers[i].done) return;
            else allResults.push(workers[i].result);
        }
        workersCompleteCallback.apply(this, allResults);
    }
};

})(jQuery);