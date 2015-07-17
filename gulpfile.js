'use strict'

var gulp = require('gulp'),
    purescript = require('gulp-purescript'),
    run = require('gulp-run'),
    runSequence = require('run-sequence');



function sequence () {
    var args = [].slice.apply(arguments);
    return function() {
        runSequence.apply(null, args);
    };
}

var sources = [
    'src/**/*.purs',
    'bower_components/purescript-*/src/**/*.purs'
];
var foreigns = [
    'src/**/*.js',
    'bower_components/purescript-*/src/**/*.js'
];

var testSources = [
    'test/src/**/*.purs'
];
var testForeigns = [
    'test/src/**/*.js'
];

gulp.task('docs', function() {
    return purescript.pscDocs({
        src: sources,
        docgen: {
            "Text.Markdown.SlamDown": "docs/Text/Markdown/SlamDown.md",
            "Text.Markdown.SlamDown.Parser": "docs/Text/Markdown/SlamDown/Parser.md",
            "Text.Markdown.SlamDown.Pretty": "docs/Text/Markdown/SlamDown/Pretty.md"
        }
    });
});


gulp.task('make', function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

gulp.task('test-make', function() {
    return purescript.psc({
        src: sources.concat(testSources),
        ffi: foreigns.concat(testForeigns)
    });
});

gulp.task('test-bundle',['test-make'], function () {
    return purescript.pscBundle({
        src: 'output/**/*.js',
        main: 'Test.Main',
        output: 'dist/test.js'
    });
});

gulp.task('test', ['test-bundle'], function() {
    run('node dist/test.js').exec();
});


gulp.task('default', sequence('make', 'docs'));
