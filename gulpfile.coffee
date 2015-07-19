gulp         = require 'gulp'

autoprefixer = require 'gulp-autoprefixer'
bower        = require 'gulp-bower'
coffee       = require 'gulp-coffee'
concat       = require 'gulp-concat'
rename       = require 'gulp-rename'
sass         = require 'gulp-sass'
sourcemaps   = require 'gulp-sourcemaps'
uglify       = require 'gulp-uglify'
gutil        = require 'gulp-util'

bowerFiles   = require 'main-bower-files'

gulp.task 'default', ['build']
gulp.task 'build', ['bower', 'coffee', 'sass']
gulp.task 'bower', ['bower-install', 'bower-files']

gulp.task 'bower-install', -> bower()
gulp.task 'bower-files', ->
  gulp.src [], base: 'bower_components/'

gulp.task 'coffee', ->
  gulp.src './coffee/**/*.coffee'
    .pipe sourcemaps.init()
    .pipe(coffee()).on 'error', gutil.log
    .pipe concat 'application.js'
    .pipe uglify()
    .pipe rename extname: '.min.js'
    .pipe sourcemaps.write()
    .pipe gulp.dest './out/js/'

gulp.task 'sass', ->
  gulp.src './scss/**/*.scss'
    .pipe sourcemaps.init()
    .pipe sass().on 'error', sass.logError
    .pipe autoprefixer()
    .pipe rename extname: '.min.css'
    .pipe sourcemaps.write()
    .pipe gulp.dest './out/css/'

gulp.task 'watch', ->
  gulp.watch './coffee/**/*.coffee', ['coffee']
  gulp.watch './scss/**/*.scss', ['sass']
