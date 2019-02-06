# TODO: add month to dataframe
# TODO: create pivot report for categories
# TODO: create pivot report for accounts
# TODO: save pivot reports to csv

library(data.table)

read.bluecoins <- function(fileName, year) {
  # Read data from BlueCoins output file for given year
  #
  # Args:
  #   fileName: path to file
  #   year: year
  #
  # Returns:
  #   Dataframe
  dataBC <- fread(fileName, encoding = "UTF-8")
  dataBC <- dataBC[, .(Date, Title, Amount, Currency, Category, Account)]
  dataBC <- dataBC[substr(Date, 1, 4) == year ,]
  dataBC <- dataBC[Category == "(Átvezetés)", Category := "Transfer" ]
  dataBC <- dataBC[Account == "Unicredit", Account := "V.Uni" ]
  dataBC <- dataBC[, Date := sapply(Date, function(x) (substr(x, 1, 10)))]
  return(dataBC)
}

check.column <- function(dataAll, dataCurrent, fileName, colName) {
  # Check if column values in dataframe are correct
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

fileBluecoins <- "input/transactions_list_table.csv"
fileIncomeCat <- "input/income_categories.csv"
fileBalanceCat <- "input/balance_categories.csv"

dataInc <- fread(fileIncomeCat)
dataBal <- fread(fileBalanceCat, dec = ",")
dataBC <- read.bluecoins(fileBluecoins, year)

check.column(dataInc, dataBC, fileBluecoins, "Category")
check.column(dataBal, dataBC, fileBluecoins, "Account")


