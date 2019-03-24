# Finance App
#
# This app reads transactions from multiple sources and summarizes them.
#
# Author: Viktor Szabo
# Year: 2019
# Licence: MIT

Sys.setlocale("LC_TIME", "English")

source("read.R")
source("check.R")
source("summary.R")
source("export.R")

Rdata <- "finance.Rdata"
newRun <- F

if (newRun) {
  dt <- read.all(fn = "input.xlsx", folder = "reports/", year = 2019, verbose = F)
  dt <- summary.all(dt)
  check.all(dt, showPairs = F, strictMode = T, verbose = T)
  save(dt, file = Rdata)
} else {
  load(Rdata)
}

export.all(dt, file = "output.xlsx", currency = "HUF", verbose = T)