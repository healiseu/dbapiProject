# DBAPI
Database Application Programming Interface in Mathematica

### Overview
Packages in this project extend Wolfram Language with commands that are related to data management and especially generic database management. There is a basic `Utilities Package` that is used in transformations between Wolfram Language constructs such as [List][01], [Rule][02], [Association][03], and [Dataset][04], and external serialization standards such as [JSON][05] and comma-separated values (CSV) files. On top of that package we are porting RESTful APIs from various web services through [URLFetch][06] into Wolfram Language building commands with arguments that specify options with rules of the form name->value. An example of such a RESTful API is the [OrientDB HTTP API][07] and the `OrientDB package`.

### Features

* Enhanced [OrientDB HTTP RESTful API][07] functionality with powerful new commands for Mathematica users
* Fully documented functions and packages in the native documentation system of Wolfram Mathematica
* Build interactive demos, powerful presentations and applications in the vast environment of Wolfram Language with nearly 5,000 built-in functions at a fraction of time it would require to build, test, and support these, in other programming languages.
* Debug easily your applications by dynamic programming and interactive sessions
* Deploy your application anywhere thanks to [Wolfram Universal Deployment System][08]
* Create multiple visualization schemes, network topologies, data mining and search indexing algorithms powered by dynamically configured GUIs for the non-experienced user.

### Installation

* You can download the latest release of DBAPI from [here][13]
* Download and run this [Mathematica Notebook code][09] to test the installation of the package

### Testing

- The [Transformations on Wolfram expressions][10] Mathematica Notebook is a demonstration of functions in the Utilities package
- The OrientDB API Package can be tested with [Demo 1-Test API Commands][11] and [Demo 3-Schemaless vs Schemafull Operations][12] Notebooks


[01]:http://reference.wolfram.com/language/ref/List.html
[02]:http://reference.wolfram.com/language/ref/Rule.html
[03]:http://reference.wolfram.com/language/ref/Association.html
[04]:http://reference.wolfram.com/language/ref/Dataset.html
[05]:http://json.org
[06]:http://reference.wolfram.com/language/ref/URLFetch.html
[07]:http://orientdb.com/docs/2.1/OrientDB-REST.html
[08]:http://www.wolfram.com/universal-deployment-system

[09]: http://test.healis.eu/dbapi/Test/Installation.nb
[10]: http://test.healis.eu/dbapi/Test/Transformations_on_Expressions.nb
[11]: http://test.healis.eu/dbapi/Test/OrientDB%20API%20Demo%201%20-%20Test%20API%20Commands.nb
[12]: http://test.healis.eu/dbapi/Test/OrientDB%20API%20Demo%203%20-%20Schemaless%20vs%20Schemafull%20Operations.nb
[13]: http://test.healis.eu/dbapi/Release/dbapi-latest-release.zip
