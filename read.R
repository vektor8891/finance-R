library(data.table)
library(readxl)
library(qdap)

rename.data <- function(d, rulesAll, type, column = FALSE, verbose = FALSE) {
  # Rename values in data.table
  #
  # Args:
  #   d: original data.table
  #   rulesAll: data.table containing all renaming rules
  #      colnames(rules) = c('Type','Value','From','To')
  #   type: filter for 'Type' column in rules data.table
  #   column: if TRUE, columns are renamed
  #   verbose: print additional information
  #
  # Returns:
  #   d: modified data.table
  
  # Rename columns
  if (column) {
    rules <- rulesAll[Type == type & Value == "Column"]
    setnames(d, rules$From, rules$To)
  # Rename values
  } else {
    rules <- rulesAll[Type == type & Value != "Column"]
    for (i in 1:nrow(rules)) {
      d <- d[get(rules[i, Value]) == rules[i, From],
             c(rules[i, Value]) := rules[i, To]]
    }
  }
  return(d)
}

read.data <- function(file, dec = ",", encoding = "UTF-8", verbose = FALSE) {
  # Read data from file
  #
  # Args:
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   d: data.table
  d <- fread(file, dec = dec, encoding = encoding)
  if (verbose) cat(dim(d)[[1]], " rows exported from: ", file)
  return(d)
}

add.data <- function(dataAll, dataNew, verbose = FALSE) {
  # Add new data to data.table
  #
  # Args:
  #   dataAll: data.table with all data
  #   dataNew: data.table with new data
  #   verbose: print additional information
  #
  # Returns:
  #   d: data.table
  # browser()
  d <- merge(dataAll, dataNew, all.y = TRUE)
  newRows <- dim(d)[[1]] - dim(dataAll)[[1]]
  if (verbose) cat(newRows, " new rows added to data table")
  return(d)
}

read.bluecoins <- function(fileName, year, fxRates, renameRules) {
  # Read data from BlueCoins output file for given year
  #
  # Args:
  #   fileName: path to file
  #   year: year
  #   fxRates: named list of fxRates
  #   renameRules: data.table containing all renaming rules
  #
  # Returns:
  #   d: data.table
  d <- fread(fileName, encoding = "UTF-8")
  d <- d[, .(Date, Title, Amount, Currency, Category, Account)]
  d <- rename.data(d, renameRules, "BlueCoins", column = TRUE)
  d <- rename.data(d, renameRules, "BlueCoins")
  d <- d[substr(Date, 1, 4) == year, ]
  d <- d[, Date := sapply(Date, function(x) gsub("-", ".", substr(x, 1, 10)))]
  d <- d[, Month := sapply(Date, function(x) strtoi(substr(x, 6, 7)))]
  d <- d[, AmountHUF := Amount]
  d <- d[Currency == "USD", AmountHUF := Amount * fxRates["USD"]]
  d <- d[Currency == "EUR", AmountHUF := Amount * fxRates["EUR"]]
  d <- d[, AmountUSD := round(AmountHUF / fxRates["USD"], 2)]
  setcolorder(d, c(colnames(d)[-2], colnames(d)[2]))
  return(d)
}

read.unicredit <- function(fileName, year, fxRates, renameRules) {
  # Read data from Unicredit output file for given year
  #
  # Args:
  #   fileName: path to file
  #   year: year
  #   fxRates: named list of fxRates
  #   renameRules: data.table containing all renaming rules
  #
  # Returns:
  #   d: data.table
  d <- as.data.table(read_excel(fileName, skip = 3))
  d <- rename.data(d, renameRules, "Unicredit", column = TRUE)
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
  #   dataAll: data.table containing all correct values
  #   dataCurrent: data.table containing current values
  #   fileName: file name
  #   colName: column name to check
  #
  # Returns:
  #   Nothing. Throws an error if unknown value found.
  catAll <- unique(dataAll[[colName]])
  catAct <- unique(dataCurrent[[colName]])
  catWrong <- catAct[sapply(catAct, function(x) is.na(match(x, catAll)))]
  if (length(catWrong) > 0) {
    cat("Unknown ", colName, " found in file:", fileName, "\n")
    print(catWrong)
    stop()
  }
}

add.category <- function(data, patternData, bcData = NULL) {
  # Add category for data based on patterns
  #
  # Args:
  #   data: data.table containing Detail fields
  #   patternData: data.table containing patterns
  #   bcData: data.table with BlueCoins data (optional)
  #
  # Returns:
  #   d: data.table
  patterns <- patternData[,Pattern]
  data[, Category := sapply(Details, function(x) {
      matchResults <- sapply(patterns, grepl, x)
      matchResults <- matchResults[matchResults == TRUE]
      if (sum(matchResults) > 1) {
        # browser()
        print("Multiple pattern match!")
        print(names(matchResults))
        print(x)
        stop()
      } else if (sum(matchResults) == 1) {
        patternData[Pattern == names(matchResults), Category]
      }
    }
    )]
}