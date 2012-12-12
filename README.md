Confetti is a simple, RESTful configuration publishing service.
----------------------------------------------------------------------------------

## Intro ##

This service mission is to provide a cluster of collaborating applications with 
a simple, convenient central point of configuration. This service is intended 
to be used by an applications built with different platforms & languages, so 
configuration data accessible via platform agnostic stack HTTP/RESTful/JSON.

## Background ##

It is quite annoying to configure every application on every computer in a cluster 
with common settings especialy if some times admin guys deleting or missconfigure 
some ones. Also I believe that in some cases it is better to store configuration 
files under some version control. And also IMHO better to have config settings at 
a central computer (probably under replication) then dispersed ones over cluster.

## Design & implementation notes ##

Conceptually this service supports the notion of a storage backend, but currently 
only default one is implemented. Default backend uses file system to store 
configuration data. The directory hierarchy forms contexts hierarchy and "*.json" 
files are subjects.

TBD

### Data model (end user view) ###

TBD

## Usage ##

TBD
