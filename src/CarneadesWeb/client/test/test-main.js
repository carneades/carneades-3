var tests = [];
for (var file in window.__karma__.files) {
    if (window.__karma__.files.hasOwnProperty(file)) {
        if (/Spec\.js$/.test(file)) {
            tests.push(file);
        }
    }
}

requirejs.config({
    baseUrl: '/base/src/app',

    paths: {
        'angular' : '/base/src/app/components/angular/angular',
        'angularResource': '/base/src/app/components/angular-resource/angular-resource',
        'angularMocks': '/base/src/app/components/angular-mocks/angular-mocks',
        'angular-ui-router' : '/base/src/app/components/angular-ui-router/release/angular-ui-router',
        'directives.questions': '/base/src/app/common/directives/questions/questions'
    },

    // ask Require.js to load these files (all our tests)
    deps: tests,

    // start test run, once Require.js is done
    callback: window.__karma__.start
});
