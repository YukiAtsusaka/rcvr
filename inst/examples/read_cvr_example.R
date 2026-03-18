# inst/examples/read_cvr_example.R
# Demonstrates read_cvr() using the 2022 Alaska Governor CVR file.
#
# The CVR file can be downloaded from:
#   Otis (2025) - Harvard Dataverse doi:10.7910/DVN/AMK8PJ
#
# Expected file: Alaska_20221108_GovernorLieutenantGovernor.csv

library(rcvr)

dt <- read_cvr("Alaska_20221108_GovernorLieutenantGovernor.csv")



