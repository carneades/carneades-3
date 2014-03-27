======================
CarneadesWeb
======================

# Requirements

- Ruby >=1.9 (2.x recommended)
- npm (http://nodejs.org/)

# Installation

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
