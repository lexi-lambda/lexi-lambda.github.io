    Title: Automatically deploying a Frog-powered blog to GitHub pages
    Date: 2015-07-18T19:09:01
    Tags: racket, frog, meta

So, I have a blog now. It's a simple static blog, but what's unique about it is that it's powered by Racket; specifically, it uses [Greg Hendershott][greghendershott]'s fantastic [Frog][frog] tool. I've taken this and moulded it to my tastes to build my blog, including configuring automatic deployment via [Travis CI][travis], so my blog is always up-to-date.

# Setting up Frog

I should note that Frog itself was wonderfully easy to drop in and get running. Just following the readme, a simple `raco pkg install frog` followed by `raco frog --init` and `raco frog -bp` created a running blog and opened it in my web browser. There was nothing more to it. Once that's done, all it takes to write a blog post is `raco frog -n "Post Title"`, and you're good to go.

By default, Frog uses Bootstrap, which provides a lot of the necessary scaffolding for you, but I opted to roll my own layout using flexbox. I also decided to use [Sass][sass] for my stylesheets, potentially with support for [CoffeeScript][coffeescript] later, so I wanted to have a good flow for compiling all the resources for deployment. To do that, I used [Gulp][gulp] in conjunction with [NPM][npm] for build and dependency management.

Going this route has a few advantages, primarily the fact that updating dependencies becomes much easier, and I can build and deploy my blog with just a couple of commands without needing to commit compiled or minified versions of my sources to version control.

# Configuring automatic deployment with Travis

Once Frog itself was configured and my styling was finished, I started looking into how to deploy my blog to a GitHub page without needing to check in any of the generated files to source control. I found a couple of resources, the most useful one being [this Gist](https://gist.github.com/domenic/ec8b0fc8ab45f39403dd), which describes how to set up deployment for any project. The basic idea is to create a deployment script which will automatically generate your project, initialize a git repository with the generated files, and push to GitHub's special `gh-pages` branch.

To make this easy, Frog can be configured to output to a separate directory via the `.frogrc` configuration file. I chose to output to the `out` directory:

```
output-dir = out
```

I also configured my Gulp build to output my CSS into the same output directory. Now, all that's necessary in order to deploy the blog to GitHub is to initialize a Git repository in the output directory, and push the files to the remote branch.

```
$ cd out
$ git init
$ git add .
$ git commit -m "Deploy to GitHub Pages"
$ git push --force "$REMOTE_URL" master:gh-pages
```

The next step is to configure Travis so that it can securely push to the GitHub repository with the required credentials. This can be done with Travis's [encryption keys][travis-encryption] along with a GitHub [personal access token][github-access-token]. Just install the Travis CLI client, copy the access token, and run a command:

```
$ gem install travis
$ travis encrypt GH_TOKEN=<access token...>
```

The output of that command is an encrypted value to be placed in an environment variable in the project's `.travis.yml` configuration file. The URL for the repository on GitHub will also need to be specified as well:

```yaml
env:
  global:
  - GH_REF: 'github.com/<gh-username>/<gh-repo>.git'
  - secure: <encrypted data...>
```

Now all that's left is configuring the `.travis.yml` to run Frog. Since Travis doesn't natively support Racket at the time of this writing, the choice of "language" is somewhat arbitrary, but since I want Pygments installed for syntax highlighting, I set my project type to `python`, then installed Racket and Frog as pre-installation steps.

```yaml
env:
  global:
  - GH_REF: 'github.com/<gh-username>/<gh-repo>.git'
  - secure: <encrypted data...>
  - RACKET_DIR: '~/racket'
  - RACKET_VERSION: '6.2'

before_install:
- git clone https://github.com/greghendershott/travis-racket.git
- cat travis-racket/install-racket.sh | bash
- export PATH="${RACKET_DIR}/bin:${PATH}"

install:
- raco pkg install --deps search-auto frog
```

(It might be worth noting that Greg Hendershott *also* maintains the repository that contains the above Travis build script!)

Finally, in my case, I wasn't deploying to a project-specific GitHub page. Instead, I wanted to deploy to my user page, which uses `master`, not `gh-pages`. Obviously, I didn't want Travis running on my `master` branch, since it would be deploying to that, so I added a branch whitelist:

```yaml
branches:
  only:
  - source
```

All that was left to do was to write up the actual deployment script to be used by Travis. Based on the one provided in the above Gist, mine looked like this:

```bash
#!/bin/bash
set -ev # exit with nonzero exit code if anything fails

# clear the output directory
rm -rf out || exit 0;

# build the blog files + install pygments for highlighting support
npm install
npm run build
pip install pygments
raco frog --build

# go to the out directory and create a *new* Git repo
cd out
git init

# inside this git repo we'll pretend to be a new user
git config user.name "Travis CI"
git config user.email "<your@email.here>"

# The first and only commit to this new Git repo contains all the
# files present with the commit message "Deploy to GitHub Pages".
git add .
git commit -m "Deploy to GitHub Pages"

# Force push from the current repo's master branch to the remote
# repo. (All previous history on the branch will be lost, since we are
# overwriting it.) We redirect any output to /dev/null to hide any sensitive
# credential data that might otherwise be exposed.
git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master > /dev/null 2>&1
```

For reference, my final `.travis.yml` looked like this:

```yaml
language: python
python:
- '3.4'

branches:
  only:
  - source

env:
  global:
  - GH_REF: 'github.com/lexi-lambda/lexi-lambda.github.io.git'
  - secure: <long secure token...>
  - RACKET_DIR: '~/racket'
  - RACKET_VERSION: '6.2'

before_install:
- git clone https://github.com/greghendershott/travis-racket.git
- cat travis-racket/install-racket.sh | bash
- export PATH="${RACKET_DIR}/bin:${PATH}"

install:
- raco pkg install --deps search-auto frog

script: bash ./deploy.sh
```

That's it! Now I have a working blog that I can publish just by pushing to the `source` branch on GitHub.

[coffeescript]: http://coffeescript.org
[frog]: https://github.com/greghendershott/frog
[github-access-token]: https://github.com/settings/tokens
[greghendershott]: http://www.greghendershott.com
[gulp]: http://gulpjs.com
[npm]: https://www.npmjs.com
[sass]: http://sass-lang.com
[travis]: https://travis-ci.org
[travis-encryption]: http://docs.travis-ci.com/user/encryption-keys/
