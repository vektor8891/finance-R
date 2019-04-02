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

year <- 2019
currency <- "USD"
verbose <- F

newRun <- T
strictMode <- T
input <- "input.xlsx"
reports <- "reports/"
output <- "output/"
Rdata <- "finance.RData"

if (newRun) {
  dt <- read.all(fn = input, folder = reports, year = year, verbose = verbose)
  dt <- summary.all(dt)
  check.all(dt, showPairs = F, strictMode = strictMode, verbose = T)
  save(dt, file = Rdata)
} else {
  load(Rdata)
}

load("finance.RData")
# dt$all <- dt$all[1:100]
export.all(dt, folder = output, currency = currency, addTimeStamp = T,
           verbose = verbose)