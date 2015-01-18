'use strict'

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , run         = require('gulp-run')
  , runSequence = require('run-sequence')
  , jsValidate  = require('gulp-jsvalidate')
  ;

var paths = {
    src: 'purs/**/*.purs',
    bowerSrc: [
      'static/components/purescript-*/src/**/*.purs',
      'static/components/purescript-*/src/**/*.purs.hs'
    ],
    dest: '',
    docs: {
        'all': {
            dest: 'MODULES.md',
            src: [
              'purs/**/*.purs'
            ]
        }
    },
    test: 'test/**/*.purs'
};

var options = {
    test: {
        main: 'Test.Main',
        output: 'output/test.js'
    },
    modules: [
      "Phb"
    ]
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
        var docgen = purescript.pscDocs();
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

gulp.task('psci', function() {
    return gulp.src([paths.src].concat(paths.bowerSrc)).pipe( purescript.dotPsci() );
});

gulp.task('browser', function() {
    return compile(purescript.psc, [paths.src].concat(paths.bowerSrc), { output: "phb.js" })
        .pipe(gulp.dest('static/js/'))
});

gulp.task('test', function() {
    return compile(purescript.psc, [paths.src, paths.test].concat(paths.bowerSrc), options.test)
        .pipe(run('node').exec());
});

gulp.task('docs', docs('all'));

gulp.task('watch', function() {
    gulp.watch(paths.src, sequence('browser', 'docs'));
});

gulp.task('default', sequence('docs', 'browser'));
