# JrUtil

jrUtil is a library and command-line utility for working with Czech (and
Slovak) public transport data, such as:

- scheduled timetables (JDF to GTFS, CZPTTCIS to GTFS, more to come)
- real-time timetable changes (TODO)
- vehicle positions (TODO)

# Usage:

- Conversion of JDF timetables (found on [ftp.cisjr.cz](ftp://ftp.cisjr.cz/)) to
  GTFS. JrUtil takes a directory of JDF files and creates a directory representing a
  GTFS feed.

  Usage within the repo:

	dotnet run -- jdf_to_gtfs <JDF dir> <GTFS out dir>

- Conversion of CZPTTCISMessage XML files (found on
  [ftp.cisjr.cz](ftp://ftp.cisjr.cz/draha/celostatni/szdc)) to GTFS.

  Usage within the repo:

	dotnet run -- czptt_to_gtfs <XML input file> <GTFS out dir>

# Installation:

This project uses [.NET Core](https://www.microsoft.com/net).

First, download and install the
[.NET Core SDK](https://www.microsoft.com/net/download).
Using the latest version is highly recommended.

Clone the repo and run `dotnet build`. Now you can use the command-line
interface by running `dotnet run -- <args>...`.

# Community

To contribute to the project or to report issues go to the
[GitLab repository](https://gitlab.com/dvdkon/jrutil).

You may also visit a forum dedicated to Czech transport data (in Czech
language): https://dadof.konarici.cz
