gulp         = require 'gulp'

autoprefixer = require 'gulp-autoprefixer'
coffee       = require 'gulp-coffee'
concat       = require 'gulp-concat'
rename       = require 'gulp-rename'
sass         = require 'gulp-sass'
sourcemaps   = require 'gulp-sourcemaps'
uglify       = require 'gulp-uglify'
gutil        = require 'gulp-util'

gulp.task 'default', ['build']
gulp.task 'build', ['coffee', 'sass']

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
