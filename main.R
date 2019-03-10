# Finance App
#
# This app reads transactions from multiple sources and summarizes them.
#
# Author: Viktor Szabo
# Year: 2019
# Licence: MIT

# TODO: check whether sum of transfer is 0
# TODO: create mapping between account name and account number for Unicredit
# TODO: automatically read all Unicredit exports

source("read.R")

# Inputs
fileIncomeCat <- "input/income_categories.csv"
fileInitialBalance <- "input/initial_balance.csv"
fileYearBalance <- paste0("input/balance_", year, ".csv")
filePatterns <- "input/patterns.csv"
fileRenameRules <- "input/rename_rules.csv"
fileFXRates <- "input/fx_rates.csv"
fileTargets <- "input/target_categories.csv"
fileNotes <- "input/cash_inventory.csv"
fileManual <- "input/manual.csv"

folderReports <- "reports/"
fileCash <- paste0("reports/cash_", year, ".csv")
fileTransactionMissing <- paste0("output/transactions_missing.csv")

# Read data
dt <- list("year" = 2019)
dt$rules <- read.data(fileRenameRules)
dt$income <- read.data(fileIncomeCat)
dt$patterns <- read.data(filePatterns)

dt$all <- data.table()
dt <- get.data.fx(dt, fileFXRates, verbose = F)
dt <- get.data.targets(dt, fileTargets, verbose = F)
dt <- get.data.balance(dt, fileInitialBalance, fileYearBalance, verbose = T)
dt <- get.data.manual(dt, fileManual, verbose = T)
dt <- get.data.cash(dt, fileCash, verbose = T)
dt <- get.data.unicredit(dt, folderReports, verbose = T)
dt <- get.data.notes(dt, fileNotes, verbose = T)

export.data(setorder(dt$all, Date), fileTransactionAll, verbose = T)
dtMissing <- setorder(dt$all, Category)[is.na(Category)]
export.data(dtMissing, fileTransactionMissing, deleteIfEmpty = T, verbose = T)

check.data(dt, showAll = F, verbose = T)

# browser()

# # # Summarize data
# dt$fileCatPivotHUF <- "output/pivot_category_huf.csv"
# dt$fileCatPivotUSD <- "output/pivot_category_usd.csv"
# dt$fileAccPivotHUF <- "output/pivot_account_huf.csv"
# dt$fileAccPivotUSD <- "output/pivot_account_usd.csv"
# pivCatHUF <- dcast(dataBC, Category ~ Month, value.var = "AmountHUF", fun = sum)
# pivCatUSD <- dcast(dataBC, Category ~ Month, value.var = "AmountUSD", fun = sum)
# pivAccHUF <- dcast(dataBC, Account ~ Month, value.var = "AmountHUF", fun = sum)
# pivAccUSD <- dcast(dataBC, Account ~ Month, value.var = "AmountUSD", fun = sum)
# 
# # Export results
# write.csv(pivCatHUF, fileCatPivotHUF, fileEncoding = "UTF-8", row.names = FALSE)
# write.csv(pivCatUSD, fileCatPivotUSD, fileEncoding = "UTF-8", row.names = FALSE)
# write.csv(pivAccHUF, fileAccPivotHUF, fileEncoding = "UTF-8", row.names = FALSE)
# write.csv(pivAccUSD, fileAccPivotUSD, fileEncoding = "UTF-8", row.names = FALSE)

