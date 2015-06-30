"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var jsvalidate = require("gulp-jsvalidate");
var run = require("gulp-run");
var rimraf = require("rimraf");

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("make", function() {
  return gulp.src(["src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"])
    .pipe(purescript.pscMake());
});

gulp.task("test", function() {
  return gulp.src(["src/**/*.purs", "test/**/*.purs", "bower_components/purescript-*/src/**/*.purs"])
    .pipe(purescript.psc({ main: "Test.Main", output: "test.js" }))
    .pipe(run("node"));
});

gulp.task("jsvalidate", ["make"], function () {
  return gulp.src("output/**/*.js")
    .pipe(jsvalidate());
});

var docTasks = [];

var docTask = function(name) {
  var taskName = "docs-" + name.toLowerCase();
  gulp.task(taskName, ["clean-docs"], function () {
    return gulp.src("src/" + name.replace(/\./g, "/") + ".purs")
      .pipe(purescript.pscDocs())
      .pipe(gulp.dest("docs/" + name + ".md"));
  });
  docTasks.push(taskName);
};

["Text.Markdown.SlamDown", "Text.Markdown.SlamDown.Parser", "Text.Markdown.SlamDown.Pretty"].forEach(docTask);

gulp.task("docs", docTasks);

gulp.task("default", ["jsvalidate", "docs", "make", "test"]);
