Abstract:

"This paper explores the incumbency advantage in Canada using two distinct methods: the
traditional regression discontinuity design (RDD) and an approach novel to Canada which
leverages redistricting. The RDD reveals a rising incumbency advantage, breaking trend
from the US. I estimate a personal advantage ranging from 17.8 to 21.2 percentage points
in close elections. In contrast, the redistricting method, which considers both contested and
safe seats, shows null results for personal incumbency advantage. This finding suggests
that the personal incumbency advantage measured by RDD is driven by the personal effort
rather than their status as an incumbent."

The final results are presented in "Incumbency Advantage in Canada.pdf".

For any inqueries, please contact seamuslong1@gmail.com



Processing details:

42RidingAggregator.R is responsible for aggregating results from the 41st and 42nd general election and arranging them in a form that is useful for statistial analysis.


RegressionDataSetBuilder.R assembles a regression data set from the output of 42RidingAggregator.R, and performs the main analysis of the project, which is a stnadard OLS. This document also generates the tables and figures that are used in the main report.

RegressionDiscontinuity.R performs an alternate regression using the standard RD format. Less aggregation was necessary, so all work is performed within this one document.


Geoprocessing done in QGIS 3.32.
Geocoding of polling stations performed through MMQGIS plugin using OpenStreetMap API


Instructions for NewPollArea.py
Script used to computer regions that are "new" to a riding after redistricting. To operate properly, the layers selected must be:
  1. new riding boundaries filtered to incude only those which are considered for incumbency (as in, novel ridings cannot be used as they are all "new" area)
  2. old riding boundaries - can include entire set
  3. intersection of old riding boundaries and filterend new riding boundaries. This could be computed within the script, but is done externally to limit computation when re-running/trouble shooting

This script will only run properly with the files provided. If using different files, the sorting function in the script must be adjusted. Presently it pairs ridings from the 2003 redistricting to the 2013 redistricting matching by poll number.
