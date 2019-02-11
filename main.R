# Finance App
#
# This app reads transactions from multiple source and summarizes them.
#
# Author: Viktor Szabo
# Year: 2019
# Licence: MIT

source("read.R")

# Inputs
year <- 2019
fxRates <- c("USD" = 280, "EUR" = 260)
verbose <- TRUE

fileIncomeCat <- "input/income_categories.csv"
fileBalanceCat <- "input/balance_categories.csv"
filePatterns <- "input/patterns.csv"
fileRenameRules <- "input/rename_rules.csv"

fileBluecoins <- "reports/transactions_list_table.csv"
fileUnicredit <- "reports/export_07_02_2019.xls"

fileTransAll <- "output/transactions_all.csv"
fileCatPivotHUF <- "output/pivot_category_huf.csv"
fileCatPivotUSD <- "output/pivot_category_usd.csv"
fileAccPivotHUF <- "output/pivot_account_huf.csv"
fileAccPivotUSD <- "output/pivot_account_usd.csv"

# Read data
renameRules <- read.data(fileRenameRules)
dataInc <- read.data(fileIncomeCat)
dataBal <- read.data(fileBalanceCat)
patterns <- read.data(filePatterns)

dataAll <- read.data(fileTransAll, dec = ".", verbose = verbose)
dataBC <- read.bluecoins(fileBluecoins, year, fxRates, renameRules)
check.column(dataInc, dataBC, fileBluecoins, "Category")
check.column(dataBal, dataBC, fileBluecoins, "Account")
# write.csv(dataBC[Account == "Cash"], fileTransAll, fileEncoding = "UTF-8", row.names = FALSE)
dataAll <- add.data(dataAll, dataBC[Account == "Cash"], verbose = verbose)

dataUni <- read.unicredit(fileUnicredit, year, fxRates, renameRules)
dataUni <- add.category(dataUni, patterns[Type == "Unicredit"], dataBC)

# # Summarize data
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

