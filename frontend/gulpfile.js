/* jshint node: true */

"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var less = require("gulp-less");
var uglify = require("gulp-uglify");
var concat = require("gulp-concat");
var rimraf = require("rimraf");

var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function(cb) {
    rimraf("docs", cb);
});

gulp.task("clean-dist", function(cb) {
    rimraf("dist", cb);
});

gulp.task("clean", ["clean-docs", "clean-dist"]);

gulp.task("psc", function() {
    return purescript.psc({
            src: sources,
            ffi: foreigns,
            output: "output"
        });
});

gulp.task("bundle", ["psc"], function() {
    return purescript.pscBundle({
            src: "output/**/*.js",
            output: "dist/index.js",
            module: "Main",
            main: "Main"
        });
});

gulp.task("concat", ["bundle"], function() {
    return gulp.src([
            "bower_components/Sortable/Sortable.min.js",
            "dist/index.js"
        ])
        .pipe(concat("index.js"))
        .pipe(gulp.dest("dist"));
});

gulp.task("compress", ["concat"], function() {
    return gulp.src("dist/index.js")
        .pipe(uglify())
        .pipe(gulp.dest("dist"));
});

gulp.task("prod", ["clean", "bundle", "concat", "compress"]);
gulp.task("dev", ["bundle", "concat"]);
gulp.task("default", ["bundle", "concat"]);
