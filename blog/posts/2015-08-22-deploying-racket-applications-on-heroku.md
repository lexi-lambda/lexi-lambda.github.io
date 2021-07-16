    Title: Deploying Racket applications on Heroku
    Date: 2015-08-22T14:47:49
    Tags: racket, heroku, 12factor

[Heroku][heroku] is a "platform as a service" that provides an incredibly simple way to deploy simple internet applications, and I take liberal advantage of its free tier for testing out simple applications. It has support for a variety of languages built-in, but Racket is not currently among them. Fortunately, Heroku provides an interface for adding custom build processes for arbitrary types of applications, called “buildpacks”. I've built one for Racket apps, and with just a little bit of configuration, it’s possible to get a Racket webserver running on Heroku.

# Building the server

Racket's [web-server][racket-web-server] package makes building and running a simple server incredibly easy. Here's all the code we'll need to get going:

```racket
#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define (start req)
  (response/xexpr
   '(html (head (title "Racket Heroku App"))
          (body (h1 "It works!")))))

(serve/servlet start #:servlet-path "/")
```

Running the above file will start up the server on the default port, 8080. When running on Heroku, however, we're required to bind to the port that Heroku provides via the `PORT` environment variable. We can access this using the Racket `getenv`[racket] function.

Additionally, the Racket web server specifically binds to localhost, but Heroku doesn't allow that restriction, so we need to pass `#f` for the `#:listen-ip` argument.

```racket
(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8080))
(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port port)
```

Also, by default, `serve/servlet`[racket] will open a web browser automatically when the program is run, which is very useful for rapid prototyping within something like DrRacket, but we'll want to turn that off.

```racket
(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port port
               #:command-line? #t)
```

That's it! Now we have a Racket web server that can run on Heroku. Obviously it's not a very interesting application right now, but that's fine for our purposes.

# Setting up our app for Heroku

The next step is to actually create an app on Heroku. Don't worry—it's free! That said, explaining precisely how Heroku works is outside the scope of this article. Just make an account, then create an app. I called mine "racket-heroku-sample". Once you've created an app and set up Heroku's command-line tool, you can specify the proper buildpack:

```sh
$ git init
$ heroku git:remote -a racket-heroku-sample
$ heroku buildpacks:set https://github.com/lexi-lambda/heroku-buildpack-racket
```

We'll also need to pick a particular Racket version before we deploy our app. At the time of this writing, Racket 6.2.1 is the latest version, so I just set the `RACKET_VERSION` environment variable as follows:

```sh
$ heroku config:set RACKET_VERSION=6.2.1
```

Now there's just one thing left to do before we can push to Heroku: we need to tell Heroku what command to use to run our application. To do this, we use something called a "Procfile" that contains information about the process types for our app. Heroku supports multiple processes of different types, but we're just going to have a single web process.

Specifically, we just want to run our `serve.rkt` module. The Racket buildpack installs the repository as a package, so we can run `racket` with the `-l` flag to specify a module path, which will be more robust than specifying a filesystem path directly. Therefore, our Procfile will look like this:

```
web: racket -l sample-heroku-app/server
```

Now all that's left to do is push our repository to Heroku's git remote. Once the build completes, we can [navigate to our app's URL and actually see it running live][app-url]!

# Conclusion

That's all that's needed to get a Racket app up and running on Heroku, but it probably isn't the best way to manage a real application. Usually it's best to use a continuous integration service to automatically deploy certain GitHub branches to Heroku, after running the tests, of course. Also, a real application would obviously be a little more complicated.

That said, this provides the foundation and shell. If you'd like to see the sample app used in this post, you can [find it on GitHub here][app-repo]. For more details on the buildpack itself, [it's also available on GitHub here][buildpack-repo].


[app-repo]: https://github.com/lexi-lambda/racket-sample-heroku-app
[app-url]: https://racket-heroku-sample.herokuapp.com
[buildpack-repo]: https://github.com/lexi-lambda/heroku-buildpack-racket
[heroku]: https://www.heroku.com
[racket-web-server]: http://docs.racket-lang.org/web-server/index.html
