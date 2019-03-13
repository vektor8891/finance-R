# Finance App
#
# This app reads transactions from multiple sources and summarizes them.
#
# Author: Viktor Szabo
# Year: 2019
# Licence: MIT

source("read.R")

# Inputs
year <- 2019
verbose <- T

folderInput <- "input/"
folderReports <- "reports/"
folderOutput <- "output/"

fileIncomeCat <- "income_categories.csv"
fileInitialBalance <- "initial_balance.csv"
fileYearBalance <- paste0("balance_", year, ".csv")
filePatterns <- "patterns.csv"
fileRenameRules <- "rename_rules.csv"
fileFXRates <- "fx_rates.csv"
fileTargets <- "target_categories.csv"
fileNotes <- "cash_inventory.csv"
fileReportTypes <- "report_types.csv"

fileTransactionMissing <- "transactions_missing.csv"
fileTransactionAll <- paste0("transactions_", year, ".xlsx")

# read input data
dt <- list("year" = year)
dt$folderInput <- folderInput
dt$folderReports <- folderReports
dt$rules <- read.data(fileRenameRules, folder = folderInput)
dt$income <- read.data(fileIncomeCat, folder = folderInput)
dt$patterns <- read.data(filePatterns, folder = folderInput)
dt$reportTypes <- read.data(fileReportTypes, folder = folderInput)

dt <- get.fx(dt, fileFXRates, verbose = F)
dt <- get.targets(dt, fileTargets, verbose = F)
dt <- get.balance(dt, fileInitialBalance, fileYearBalance, verbose = F)
dt <- get.notes(dt, fileNotes, verbose = T)

# read reports
dt$all <- data.table()
for (fn in list.files(path = folderReports)) {
  print(fn)
  type <- get.report.type(dt, fn, verbose = verbose)
  if (!is.null(type)) {
    dt <- get.data(dt, fn, type, verbose = verbose)
  }
}

# export results
setorder(dt$all, Date)
setcolorder(dt$all, c(colnames(dt$all)[-2], colnames(dt$all)[2]))
write_xlsx(dt$all, path = paste0(folderOutput, fileTransactionAll))
dtMissing <- setorder(dt$all, Category)[is.na(Category)]
export.data(dtMissing, fileTransactionMissing, deleteIfEmpty = T,
            folder = folderOutput, verbose = T)

# check data
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

