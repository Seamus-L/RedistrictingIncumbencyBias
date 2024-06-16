This is a work in progress


This project seeks to estimate the incumbency advantage in Canadaian federal elections by taking advantage of 10 year redistricting measures.
In short, the voting behaviour of polling stations that are newly added to a riding will be compared to those which are "old" to determine the pure advantage the incumbent enjoys

Presently there are shape files provided which provide geo-coded Lat/long coords for poll stations. poll stations that could not be located, or were placed outside of a given province have been weeded from the data set





Geoprocessing done in QGIS 3.32


Instructions for NewPollArea.py
Script used to computer regions that are "new" to a riding after redistricting. To operate properly, the layers selected must be:
  1. new riding boundaries filtered to incude only those which are considered for incumbency (as in, novel ridings cannot be used as they are all "new" area)
  2. old riding boundaries - can include entire set
  3. intersection of old riding boundaries and filterend new riding boundaries. This could be computed within the script, but is done externally to limit computation when re-running/trouble shooting

This script will only run properly with the files provided. If using different files, the sorting function in the script must be adjusted. Presently it pairs ridings from the 2003 redistricting to the 2013 redistricting matching by poll number.
