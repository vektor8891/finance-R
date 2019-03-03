# Finance App
#
# This app reads transactions from multiple source and summarizes them.
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
fileCash <- "input/cash_inventory.csv"

fileBluecoins <- "reports/transactions_list_table.csv"
fileUnicredit <- "reports/export_07_02_2019.xls"
fileTransactionAll <- paste0("output/transactions_", year, ".csv")
fileTransactionManual <- paste0("output/transactions_manual.csv")
fileTransactionMissing <- paste0("output/transactions_missing.csv")

# Read data
dt <- list("year" = 2019)
dt$rules <- read.data(fileRenameRules)
dt$income <- read.data(fileIncomeCat)
dt$patterns <- read.data(filePatterns)

dt <- get.data.all(dt, fileTransactionAll, empty = T, verbose = T)
dt <- get.data.fx(dt, fileFXRates, verbose = F)
dt <- get.data.balance(dt, fileInitialBalance, fileYearBalance, verbose = T)
dt <- get.data.manual(dt, fileTransactionManual, verbose = T)
dt <- get.data.bc(dt, fileBluecoins, verbose = T)
dt <- get.data.uni(dt, fileUnicredit, verbose = T)
dt <- get.data.notes(dt, fileCash, verbose = T)

check.data(dt, verbose = T)

export.data(setorder(dt$all, Date), fileTransactionAll, verbose = T)
dtManual <- setorder(dt$all, Category)[Source == "Manual"]
dtMissing <- setorder(dt$all, Category)[is.na(Category)]
if (nrow(dtManual) > 0) export.data(dtManual, fileTransactionManual, TRUE)
if (nrow(dtMissing) > 0) export.data(dtMissing, fileTransactionMissing, TRUE)
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

