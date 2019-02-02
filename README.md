# [JrUtil](https://gitlab.com/dvdkon/jrutil)

JrUtil is a library and a set of utilities for working with Czech (and Slovak)
public transport data, such as:

- scheduled timetables (JDF to GTFS, CZPTTCIS to GTFS, more to come)
- real-time timetable changes (TODO)
- vehicle positions (TODO)

# Project parts

JrUtil's main part is the central library, also called *JrUtil*. It allows the
user to read a variety of public transport data formats (currently JDF and
CZPTTCIS), convert them to GTFS and then perform various operations on the
result (e.g. merging the resultant feeds into one).

*jrutil-multitool* is a small tool whose purpose is to provide a command line
interface to commonly used functions of *JrUtil*. It can, for example, convert
a single JDF or CZPTT batch to a GTFS feed or load a GTFS feed into a
PostgreSQL database.

*jrunify* is a tool that is used for merging data from various sources into a
single GTFS feed. It's designed to be ran periodically by
[*jrunify_cloud*](https://gitlab.com/dvdkon/jrunify_cloud). It currently uses
data from the [CIS JŘ FTP server](ftp://ftp.cisjr.cz).

*htmltt* is a web app/static website generator that can be used for displaying
GTFS feeds, primarily to aid debugging.

# Installation:

This project uses [.NET Core](https://www.microsoft.com/net).

First, download and install the
[.NET Core SDK](https://www.microsoft.com/net/download).
Using the latest version is highly recommended.

Clone the repo and run `dotnet build`. Now you can use the various utilities by
going to their directory and running `dotnet run -- ARGS...`.

Some parts of JrUtil use a PostgreSQL database to store data and speed up
bulk operations. To use those, set up a PostgreSQL server and pass an
[Npgsql connection string](https://www.npgsql.org/doc/connection-string-parameters.html)
as a parameter.

# Community

To contribute to the project or to report issues go to the
[GitLab repository](https://gitlab.com/dvdkon/jrutil).

You may also visit a forum dedicated to Czech transport data (in Czech
language): https://dadof.konarici.cz
