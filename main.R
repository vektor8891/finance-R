# Finance App
#
# This app reads transactions from multiple sources and summarizes them.
#
# Author: Viktor Szabo
# Year: 2019
# Licence: MIT

# Run all: Ctrl + Alt + R
# TODO: check monthly balance
# find error: traceback()



Sys.setlocale("LC_TIME", if (.Platform$OS.type == "unix") "C" else "English")

source("read.R")
source("check.R")
source("summary.R")
source("export.R")

year <- 2019
currency <- "USD"
verbose <- F

newRun <- T
strictMode <- T
config <- "config.xlsx"
reports <- "reports/"
output <- "output/"
Rdata <- "finance1.RData"

tic("total")
if (newRun) {
  dt <- read.all(fn = config, folder = reports, year = year, verbose = verbose)
  dt <- summary.all(dt)
  check.all(dt, showPairs = F, strictMode = strictMode, verbose = T)
  save(dt, file = Rdata)
} else {
  load(Rdata)
}

# dt$all <- dt$all[1:100] # trunk data for testing
export.all(dt, folder = output, currency = currency, addTimeStamp = F,
           verbose = verbose)
