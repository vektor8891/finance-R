# Finance App
#
# This app reads transactions from multiple source and summarizes them.
#
# Author: Viktor Szabo
# Year: 2019
# Licence: MIT

source("read.R")
# sink("finance.log", append = FALSE, split = TRUE)

# Inputs
year <- 2019
fxRates <- c("USD" = 280, "EUR" = 260)

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
dtInc <- read.data(fileIncomeCat)
dtBal <- read.data(fileBalanceCat)
patterns <- read.data(filePatterns)

dtAll <- read.data(fileTransAll, dec = ".", verbose = T)
cat(paste0(dim(dtAll)[[1]], " rows imported from ", fileTransAll))
dtBC <- read.bluecoins(fileBluecoins, year, fxRates, renameRules, verbose = T)
check.column(dtInc, dtBC, fileBluecoins, "Category")
check.column(dtBal, dtBC, fileBluecoins, "Account")
write.csv(dtBC[Account == "Cash"], fileTransAll, fileEncoding = "UTF-8", row.names = FALSE)
dtAll <- add.data(dtAll, dtBC[Account == "Cash"], verbose = T)

dtUni <- read.unicredit(fileUnicredit, year, fxRates, renameRules, verbose = T)
dtUni <- add.category(dtUni, patterns[Type == "Unicredit"], dtBC, verbose = F)

# browser()
dtAll <- add.data(dtAll, dtUni, verbose = T)

write.csv(dtAll, fileTransAll, fileEncoding = "UTF-8", row.names = FALSE)
cat(paste0(dim(dtAll)[[1]], " rows exported to ", fileTransAll))

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

