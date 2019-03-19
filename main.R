# Finance App
#
# This app reads transactions from multiple sources and summarizes them.
#
# Author: Viktor Szabo
# Year: 2019
# Licence: MIT

source("read.R")
source("check.R")

dt <- read(fn = "input.xlsx", folder = "reports/", year = 2019, verbose = F)
dt <- check(dt, showAll = F, strictMode = T, verbose = T)

browser()

# check.duplicates(d, threshold = report$Duplicates, verbose = verbose)

# folderInput <- "input/"
# folderReports <- "reports/"
# folderOutput <- "output/"
# 
# fileIncomeCat <- "income_categories.csv"
# fileInitialBalance <- "initial_balance.csv"
# fileYearBalance <- paste0("balance_", year, ".csv")
# filePatterns <- "patterns.csv"
# fileRenameRules <- "rename_rules.csv"
# fileFXRates <- "fx_rates.csv"
# fileTargets <- "target_categories.csv"
# fileNotes <- "cash_inventory.csv"
# fileReportTypes <- "report_types.csv"
# 
# fileMissing <- "transactions_missing.csv"
# fileFinal <- "FINAL_REPORT"
# 
# # read input data
# dt <- list("year" = year)
# dt$folderInput <- folderInput
# dt$folderReports <- folderReports
# dt$rules <- read.data(fileRenameRules, folder = folderInput)
# dt$income <- read.data(fileIncomeCat, folder = folderInput)
# dt$patterns <- read.data(filePatterns, folder = folderInput)
# dt$reportTypes <- read.data(fileReportTypes, folder = folderInput)
# 
# dt <- get.fx(dt, fileFXRates, verbose = F)
# dt <- get.targets(dt, fileTargets, verbose = F)
# dt <- get.balance(dt, fileInitialBalance, fileYearBalance, verbose = F)
# dt <- get.notes(dt, fileNotes, verbose = T)
# 
# # read reports
# dt$all <- data.table()
# for (fn in list.files(path = folderReports)) {
#   type <- get.report.type(dt, fn, verbose = verbose)
#   if (!is.null(type)) {
#     print(fn)
#     dt <- get.data(dt, fn, type, verbose = verbose)
#   }
# }
# 
# # check data
# check.data(dt, showAll = F, strictMode = F, verbose = T)
# 
# # export results
# export.missing(dt$all, folderOutput, fileMissing)
# export.results(dt, folderOutput, fileFinal, currency = "HUF")
# export.results(dt, folderOutput, fileFinal, currency = "USD")