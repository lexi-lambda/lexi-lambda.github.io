import gulp            from 'gulp'

import autoprefixer    from 'gulp-autoprefixer'
import compileCoffee   from 'gulp-coffee'
import concat          from 'gulp-concat'
import rename          from 'gulp-rename'
import uglify          from 'gulp-uglify'

import sass            from 'sass'
import makeCompileSass from 'gulp-sass'

compileSass = makeCompileSass(sass)

export js = ->
  gulp.src 'coffee/**/*.coffee', sourcemaps: true
    .pipe compileCoffee()
    .pipe concat 'application.js'
    .pipe uglify()
    .pipe rename extname: '.min.js'
    .pipe gulp.dest 'output/js/'

export css = ->
  gulp.src 'scss/**/*.scss', sourcemaps: true
    .pipe compileSass().on 'error', compileSass.logError
    .pipe autoprefixer()
    .pipe rename extname: '.min.css'
    .pipe gulp.dest 'output/css/'

export images = ->
  gulp.src 'images/**/*'
    .pipe gulp.dest 'output/img/'

export build = gulp.parallel js, css, images

export watch = ->
  gulp.watch 'coffee/**/*.coffee', js
  gulp.watch 'scss/**/*.scss', css
  gulp.watch 'images/**/*', images

export default build
