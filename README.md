Confetti
----------------------------------------------------------------------------------

## Intro ##

Confetti is a simple, RESTful configuration publishing service. The mission of 
this service is to provide a cluster of collaborating applications with 
a simple, convenient central point of configuration. This service is intended 
to be used by an applications built with different platforms & languages, so 
configuration data accessible via platform agnostic stack HTTP/RESTful/JSON.

## Background ##

It is quite annoying to configure every application on every computer in a cluster 
with common settings especialy if some times admin guys deleting or missconfigure 
some ones. Also I believe that in some cases it is better to store configuration 
files under some version control. And also IMHO better to have config settings at 
a central computer (probably under replication) than dispersed ones over cluster.

## Design & implementation notes ##

Conceptually this service supports the notion of a storage backend, but currently 
only default one is implemented. Default backend store configuration data in a set 
of files organized via directories. The directory hierarchy forms contexts hierarchy 
and "*.json" files are subjects. For fast access all configuration data stored also 
into in-memory mnesia tables, i.e. it is some kind of index. The file system 
periodically rescanned and index actualized if required.

### Data model (end user view) ###

There are three fundamental notions: variable, subject and context. The contexts have 
a hierarchical organization. Each context have its path in a hierarchy i.e. its fully 
qualified name. Subject is a container for a configuration variables responsible for 
configuration of some aspect of an applied system or cluster. The same subject may 
appear at the different contexts. To get value of some particular variable you must 
specify all three parameters: context, subject and variable name. When you do this, 
all upper contexts considered to compute the resulting value of the variable. 
I.e. variable value in some subject in some context have a priority over the value 
of the same variable defined in the same subject but in an upper context (i.e. in a 
context whose path is a prefix of the first context).

#### Example ####
```
 * conf_root/
 *  test/
 *    external_services.json
 *      "metrics_server" : { "host : "test.some.host.com", "port" : 123 }
 *      "monitor_server" : { "host : "test.some.host2.com", "port" : 1234 }
 *    app_1/
 *      some.particular.host/
 *        external_services.json
 *          "metrics_server" : null
 *    app_2/
 *    
 *  prod/
 *    external_services.json
 *      "metrics_server" : { "host" : "prod.some.host.com", "port" : 123 }
 *      "monitor_server" : { "host" : "prod.some.host2.com", "port" : 1234 }
 *    app_1/
 *      ...
 *    app_2/
 *      ...
```

If we have situation above and query the variable as:

  ``http://.../test/app_1/some.particular.host?subject=external_services&variables=metrics_server``
  
then we get something like:
```json
  {
    "metrics_server" : null
  }
```
  
I.e. we disabled metrics reporting for app_1 on some.particular.host . But if we do query:

  ``http://.../test/app_1/some.OTHER.particular.host?subject=external_services&variables=metrics_server``

then we get something like:
```json
  {
    "metrics_server" : { "host" : "test.some.host.com", "port" : 123 }
  }
```
The same result will be if you issue query like:

``http://.../test/app_1?subject=external_services&variables=metrics_server``

And finally if you query:

``http://.../prod/app_1/some.PROD.particular.host?subject=external_services&variables=metrics_server``
Or
``http://.../prod/app_1?subject=external_services&variables=metrics_server``

you will get:

```json
  {
    "metrics_server" : { "host" : "prod.some.host.com", "port" : 123 }
  }
```

It is supposed that each application in a cluster configured with its context and address of 
confetti service, e.g. using URL like:

``http://confetti.host.com:8080/conf/test/app_1`` (and optionaly at runtime application may add ``/host.address.nam``)

and when this application wants to get some variable value it adds appropriate parameters
and issues a query.

## Usage ##

TBD
