library(data.table)
library(readxl)
library(qdap)

read.bluecoins <- function(fileName, year, fxRates) {
  # Read data from BlueCoins output file for given year
  #
  # Args:
  #   fileName: path to file
  #   year: year
  #   fxRates: named list of fxRates
  #
  # Returns:
  #   d: Dataframe
  d <- fread(fileName, encoding = "UTF-8")
  d <- d[, .(Date, Title, Amount, Currency, Category, Account)]
  d <- d[substr(Date, 1, 4) == year, ]
  d <- d[Category == "(Átvezetés)", Category := "Transfer"]
  d <- d[Account == "Unicredit", Account := "V.Uni"]
  d <- d[, Date := sapply(Date, function(x) sub("-", ".", substr(x, 1, 10)))]
  d <- d[, Month := sapply(Date, function(x) strtoi(substr(x, 6, 7)))]
  d <- d[, AmountHUF := Amount]
  d <- d[Currency == "USD", AmountHUF := Amount * fxRates["USD"]]
  d <- d[Currency == "EUR", AmountHUF := Amount * fxRates["EUR"]]
  d <- d[, AmountUSD := AmountHUF / fxRates["USD"]]
  return(d)
}

read.unicredit <- function(fileName, year, fxRates) {
  # Read data from Unicredit output file for given year
  #
  # Args:
  #   fileName: path to file
  #   year: year
  #   fxRates: named list of fxRates
  #
  # Returns:
  #   d: Dataframe
  d <- as.data.table(read_excel(fileName, skip = 3))
  old <- c("Tranzakció részletek", "Érték Dátum", "Összeg")
  new <- c("Details", "Date", "Amount")
  setnames(d, old, new)
  d <- d[, .(Details, Date, Amount)]
  d <- d[substr(Date, 1, 4) == year, ]
  d <- d[, Account := "V.Uni"]
  d <- d[, Month := sapply(Date, function(x) strtoi(substr(x, 6, 7)))]
  d <- d[, AmountHUF := sapply(Amount, function(x) 
    as.numeric(mgsub(c(",", "\u00A0", " HUF"), c(".", "", ""), x))
  )]
  d <- d[, AmountUSD := AmountHUF / fxRates["USD"]]
  return(d)
}

check.column <- function(dataAll, dataCurrent, fileName, colName) {
  # Check for unrecognizeable value in column
  #
  # Args:
  #   dataAll: dataframe containing all correct values
  #   dataCurrent: dataframe containing current values
  #   fileName: file name
  #   colName: column name to check
  #
  # Returns:
  #   Dataframe
  catAll <- unique(dataAll[[colName]])
  catAct <- unique(dataCurrent[[colName]])
  # browser()
  catWrong <- catAct[sapply(catAct, function(x) is.na(match(x, catAll)))]
  if (length(catWrong) > 0) {
    cat("Unknown ", colName, " found in file:", fileName, "\n")
    print(catWrong)
    stop()
  }
}

year <- 2019
fxRates <- c("USD" = 280, "EUR" = 260)

fileIncomeCat <- "input/income_categories.csv"
fileBalanceCat <- "input/balance_categories.csv"
filePatterns <- "input/balance_categories.csv"

fileBluecoins <- "reports/transactions_list_table.csv"
fileUnicredit <- "reports/export_07_02_2019.xls"

fileCatPivotHUF <- "output/pivot_category_huf.csv"
fileCatPivotUSD <- "output/pivot_category_usd.csv"
fileAccPivotHUF <- "output/pivot_account_huf.csv"
fileAccPivotUSD <- "output/pivot_account_usd.csv"

dataInc <- fread(fileIncomeCat)
dataBal <- fread(fileBalanceCat, dec = ",")
dataBC <- read.bluecoins(fileBluecoins, year, fxRates)
dataUC <- read.unicredit(fileUnicredit, year, fxRates)

check.column(dataInc, dataBC, fileBluecoins, "Category")
check.column(dataBal, dataBC, fileBluecoins, "Account")

pivCatHUF <- dcast(dataBC, Category ~ Month, value.var = "AmountHUF", fun = sum)
pivCatUSD <- dcast(dataBC, Category ~ Month, value.var = "AmountUSD", fun = sum)
pivAccHUF <- dcast(dataBC, Account ~ Month, value.var = "AmountHUF", fun = sum)
pivAccUSD <- dcast(dataBC, Account ~ Month, value.var = "AmountUSD", fun = sum)

write.csv(pivCatHUF, fileCatPivotHUF)
write.csv(pivCatUSD, fileCatPivotUSD)
write.csv(pivAccHUF, fileAccPivotHUF)
write.csv(pivAccUSD, fileAccPivotUSD)
