## apug

apug is a version of the pugjs templating engine for ILE applications. It's primary purpose is for CGI applications.

#### More info

* [Available ILE APIs](https://github.com/WorksOfLiam/apug/wiki/APIs)
* [apug Syntax Documentation](https://github.com/WorksOfLiam/apug/wiki/Syntax-examples)

#### Building

To make use of apug, you need to build these modules:

1. `src/arraylist.rpgle`
2. `src/apug.rpgle`

You should then bind these modules to the programs that you want to use apug with. You can optionally make a service program too.