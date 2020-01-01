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

source("functions/read.R")
source("functions/check.R")
source("functions/summary.R")
source("functions/export.R")

year <- 2019
currency <- "USD"
verbose <- F

newRun <- T
strictMode <- T
config <- paste0("config/config_", year, ".xlsx")
reports <- paste0("reports/", year, "/")
output <- paste0("output/", year, "/")
Rdata <- "finance1.RData"

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
