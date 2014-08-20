======================
# CarneadesWeb

> Web application front-end for argument mapping and evaluation application.

## Features
- Feature 1
- Feature 2

## Installation
Carneades installs itself on the first run of the `carneades` shell script; there is no
separate install script. Follow these instructions to install Carneades manually:

1. Make sure you have a Java JDK version 6 or later.
2. [Download the `carneades` script from the `editors-sebastian` branch](https://raw.githubusercontent.com/carneades/carneades/editors-sebastian/src/CarneadesWeb/scripts/carneades.sh)
 of this project.
3. Place it on your `$PATH`. (`~/bin` is a good choice if it is on your path.)
4. Set it to be executable. (`chmod 755 ~/bin/carneades.sh`)
5. Run it.

### Windows
There is no installer for Windows at the moment.

## Usage
The [tutorial](https://github.com/carneades/carneades/blob/stable/doc/TUTORIAL.md)
has a detailed walk-through of the steps involved in starting Carneades and setting up an intial database, but here are the commonly-used tasks:

    $ carneades start [PORT] # start carneades on port

    $ carneades init

    $ carneades repo fetch # grab the code from github

    $ carneades repo build #

    $ carneades repo deploy #

Use `carneades help` to see a complete list. `carneades help $TASK` shows the
usage for a specific task.

## Configuration

The `.carneades.clj` file in your home folder should look like this:

```clj
{
  :projects-directory "/home/[USERNAME]/[path_to_projects_root]"
}
```

## Build it on your own

### Requirements

- Ruby >=1.9 (2.x recommended)
- Node package manager (npm) provided by [node.js](http://nodejs.org/)


### Instructions
As root:

```
npm install -g grunt-cli
npm install -g coffee-script
npm install -g bower
gem update --system
gem install compass
gem install haml
```

# Server

Start the server with (development environment!): ```lein repl``` and type ```(go)```

# Client

Compile the client with

```
npm install
bower install
grunt build
```

# Notes

The front-end is using Angularjs, RequireJs and Bootstrap 3.
