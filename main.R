# Finance App
#
# This app reads transactions from multiple sources and summarizes them.
#
# Author: Viktor Szabo
# Year: 2019
# Licence: MIT

source("read.R")
source("check.R")

dt <- read.all(fn = "input.xlsx", folder = "reports/", year = 2019, verbose = F)
dt <- summarize.all(dt)

check.all(dt, showPairs = F, strictMode = T, verbose = T)