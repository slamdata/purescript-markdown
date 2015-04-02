'use strict'

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , browserify  = require('gulp-browserify')
  , run         = require('gulp-run')
  , runSequence = require('run-sequence')
  , jsValidate  = require('gulp-jsvalidate')
  ;

var paths = {
    src: 'src/**/*.purs',
    bowerSrc: [
      'bower_components/purescript-*/src/**/*.purs',
      'bower_components/purescript-*/src/**/*.purs.hs'
    ],
    dest: '',
    docs: {
        'Text.Markdown.SlamDown': {
            dest: 'docs/README.md',
            src: [
              'src/Text/Markdown/SlamDown.purs',
              'src/Text/Markdown/SlamDown/Parser.purs',
              'src/Text/Markdown/SlamDown/Pretty.purs',
              'src/Text/Markdown/SlamDown/Html.purs'
            ]
        }
    },
    exampleSrc: 'example/Main.purs',
    test: 'test/**/*.purs'
};

var options = {
    test: {
        main: 'Test.Main',
        output: 'output/test.js'
    }, 
    example: {
        main: 'Main',
        modules: ['Main']
    }
};

function compile (compiler, src, opts) {
    var psc = compiler(opts);
    psc.on('error', function(e) {
        console.error(e.message);
        psc.end();
    });
    return gulp.src(src.concat(paths.bowerSrc))
        .pipe(psc)
        .pipe(jsValidate());
};

function docs (target) {
    return function() {
        var docgen = purescript.docgen();
        docgen.on('error', function(e) {
            console.error(e.message);
            docgen.end();
        });
        return gulp.src(paths.docs[target].src)
            .pipe(docgen)
            .pipe(gulp.dest(paths.docs[target].dest));
    }
}

function sequence () {
    var args = [].slice.apply(arguments);
    return function() {
        runSequence.apply(null, args);
    }
}

gulp.task('browser', function() {
    return compile(purescript.psc, [paths.src, paths.exampleSrc].concat(paths.bowerSrc), options.example)
        .pipe(browserify({}))
        .pipe(gulp.dest('example'))
});

gulp.task('make', function() {
    return compile(purescript.pscMake, [paths.src].concat(paths.bowerSrc), {})
        .pipe(gulp.dest(paths.dest))
});

gulp.task('test', function() {
    return compile(purescript.psc, [paths.src, paths.test].concat(paths.bowerSrc), options.test)
        .pipe(run('node').exec());
});

gulp.task('Text.Markdown.SlamDown', docs('Text.Markdown.SlamDown'));

gulp.task('docs', ['Text.Markdown.SlamDown']);

gulp.task('watch-browser', function() {
    gulp.watch(paths.src, sequence('browser', 'docs'));
});

gulp.task('watch-make', function() {
    gulp.watch(paths.src, sequence('make', 'docs'));
});

gulp.task('default', sequence('make', 'docs', 'browser'));
