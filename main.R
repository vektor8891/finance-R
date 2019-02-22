# Finance App
#
# This app reads transactions from multiple source and summarizes them.
#
# Author: Viktor Szabo
# Year: 2019
# Licence: MIT

# TODO: merge get.data.all

source("read.R")

# Inputs
fileIncomeCat <- "input/income_categories.csv"
fileBalanceCat <- "input/balance_categories.csv"
filePatterns <- "input/patterns.csv"
fileRenameRules <- "input/rename_rules.csv"

fileBluecoins <- "reports/transactions_list_table.csv"
fileUnicredit <- "reports/export_07_02_2019.xls"
fileTransAll <- "output/transactions_all.csv"
fileTransManual <- "output/transactions_missing.csv"

# Read data
dt <- list("year" = 2019)
dt$fx <- c("USD" = 280, "EUR" = 260)
dt$rules <- read.data(fileRenameRules)
dt$income <- read.data(fileIncomeCat)
dt$balance <- read.data(fileBalanceCat, dec = ",")
dt$patterns <- read.data(filePatterns)

dt <- get.data.all(dt, fileTransAll, empty = T, verbose = T)
dt <- get.data.manual(dt, fileTransManual, empty = T, verbose = T)
dt <- get.data.bc(dt, fileBluecoins, verbose = T)
dt <- get.data.uni(dt, fileUnicredit, verbose = T)
export.data(setorder(dt$all, Date), fileTransAll, verbose = T)
export.data(setorder(dt$all, Category)[is.na(Category) | Source == "Manual"], fileTransManual, verbose = T)
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

