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
            "Text.Markdown.SlamDown.Syntax": "docs/Text/Markdown/SlamDown/Syntax.md",
            "Text.Markdown.SlamDown.Syntax.FormField": "docs/Text/Markdown/SlamDown/Syntax/FormField.md",
            "Text.Markdown.SlamDown.Syntax.Inline": "docs/Text/Markdown/SlamDown/Syntax/Inline.md",
            "Text.Markdown.SlamDown.Syntax.Block": "docs/Text/Markdown/SlamDown/Syntax/Block.md",
            "Text.Markdown.SlamDown.Traverse": "docs/Text/Markdown/SlamDown/Traverse.md",
            "Text.Markdown.SlamDown.Eval": "docs/Text/Markdown/SlamDown/Eval.md",
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

gulp.task('test', ['test-bundle'], function(cb) {
    // Need this verbosity setting here to prevent the test running from being
    // cut off, strangely.
    run('node dist/test.js', { verbosity: 3 }).exec(cb);
});

gulp.task('dotpsci', function () {
    return purescript.psci({ src: sources, ffi: foreigns })
        .pipe(gulp.dest('.'));
});

gulp.task('default', sequence('make', 'docs', 'dotpsci'));
