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
  rulesCol <- rulesAll[Type == type & Value == "Column"]
  setnames(d, rulesCol$From, rulesCol$To)
  # Rename values
  rulesVal <- rulesAll[Type == type & Value != "Column"]
  if (nrow(rulesVal) > 0) {
    for (i in 1:nrow(rulesVal)) {
      d <- d[get(rulesVal[i, Value]) == rulesVal[i, From],
              c(rulesVal[i, Value]) := rulesVal[i, To]]
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

add.data <- function(dtAll, dtNew, verbose = FALSE) {
  # Add new data to data.table
  #
  # Args:
  #   dtAll: data.table with all data
  #   dtNew: data.table with new data
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table
  # browser()
  dt <- merge(dtAll, dtNew, all = TRUE)
  newRows <- dim(dt)[[1]] - dim(dtAll)[[1]]
  if (verbose) cat(newRows, " new rows added to data table")
  return(dt)
}

read.bluecoins <- function(fileName, year, fxRates, rules, verbose = FALSE) {
  # Read data from BlueCoins output file for given year
  #
  # Args:
  #   fileName: path to file
  #   year: year
  #   fxRates: named list of fxRates
  #   rules: data.table containing all renaming rules
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table
  dt <- fread(fileName, encoding = "UTF-8")
  if (verbose) cat(paste0(dim(dt)[[1]], " rows imported from ", fileName))
  dt <- dt[, .(Date, Title, Amount, Currency, Category, Account)]
  dt <- rename.data(dt, rules, "BlueCoins")
  dt <- dt[substr(Date, 1, 4) == year, ]
  dt <- dt[, Date := sapply(Date, function(x) gsub("-", ".", substr(x, 1, 10)))]
  dt <- dt[, Month := sapply(Date, function(x) strtoi(substr(x, 6, 7)))]
  dt <- dt[, AmountHUF := Amount]
  dt <- dt[Currency == "USD", AmountHUF := Amount * fxRates["USD"]]
  dt <- dt[Currency == "EUR", AmountHUF := Amount * fxRates["EUR"]]
  dt <- dt[, AmountUSD := round(AmountHUF / fxRates["USD"], 2)]
  setcolorder(dt, c(colnames(dt)[-2], colnames(dt)[2]))
  return(dt)
}

read.unicredit <- function(fileName, year, fxRates, renameRules, verbose = F) {
  # Read data from Unicredit output file for given year
  #
  # Args:
  #   fileName: path to file
  #   year: year
  #   fxRates: named list of fxRates
  #   renameRules: data.table containing all renaming rules
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table
  dt <- as.data.table(read_excel(fileName, skip = 3))
  if (verbose) cat(paste0(dim(dt)[[1]], " rows imported from ", fileName))
  dt <- rename.data(dt, renameRules, "Unicredit")
  dt <- dt[, .(Details, Date, Amount)]
  dt <- dt[substr(Date, 1, 4) == year, ]
  dt <- dt[, Account := "V.Uni"]
  dt <- dt[, Month := sapply(Date, function(x) strtoi(substr(x, 6, 7)))]
  dt <- dt[, AmountHUF := sapply(Amount, function(x) 
    as.numeric(mgsub(c(",", "\u00A0", " HUF"), c(".", "", ""), x))
  )]
  dt <- dt[, AmountUSD := round(AmountHUF / fxRates["USD"], 2)]
  dt <- dt[, c("Amount", "Currency") := list(AmountHUF, "HUF")]
  return(dt)
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

add.category <- function(dt, patternData, bcData = NULL, verbose = F) {
  # Add category for transactions
  #
  # Add category for transactions based on 1) patterns and 2) historical data.
  # Throw error if categories from 1) and 2) don't match.
  #
  # Args:
  #   dt: data.table containing Detail fields
  #   patternData: data.table containing patterns
  #   bcData: data.table with BlueCoins data (optional)
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table
  patterns <- patternData[,Pattern]
  dt <- dt[, Category := NA]
  dt[, Category := mapply(function(detail, huf, date, account) {
    if (verbose) cat(paste0(date, ": ", huf, " HUF (" ,detail, ")\n"))
    catBC <- get.category.data(bcData, huf, date, account, verbose = verbose)
    catPtn <- get.category.pattern(detail, patternData, verbose = verbose)
    # browser()
    if (is.na(catBC)) {
      return(catPtn)
    } else if (is.na(catPtn)) {
      return(catBC)
    } else if (catBC != catPtn) {
      if (verbose) cat(paste0("\tWARNING: '", catBC, "' overwrites '", catPtn, "'!!!\n"))
      return(catBC)
    } else {
      return(catBC)
    }}, Details, AmountHUF, Date, Account)]
  dt <- dt[,.(Category = unlist(Category)), by = setdiff(names(dt), 'Category')]
  return(dt)
}

get.category.data <- function(bcData, huf, date, account, days = 7, verbose = F) {
  # Get category for transation from BlueCoins data
  #
  # Args:
  #   bcData: data.table with BlueCoins data
  #   huf: amount in HUF
  #   date: transaction date
  #   account: account name
  #   days: max number or days to search before date
  #   verbose: print additional information
  #
  # Returns:
  #   catBC: category
  catBC <- NA
  matchRows <- bcData[Account == account & AmountHUF == huf]
  if (nrow(matchRows) > 0) {
    for (i in 1:nrow(matchRows)) {
      dateRow <- matchRows[i, Date]
      diff <- as.Date(dateRow, "%Y.%m.%d") - as.Date(date, "%Y.%m.%d")
      if (as.numeric(diff) <= days) {
        catBC <- matchRows[i, Category]
        if (verbose) cat(paste0("\t'", catBC, "' based on BC data\n"))
      }
    }
  }
  return(catBC)
}

get.category.pattern <- function(detail, patternData, verbose = F) {
  # Get category for transation from patterns
  #
  # Args:
  #   detail: transaction detail
  #   patternData: data.table containing patterns
  #   verbose: print additional information
  #
  # Returns:
  #   catPtn: category
  catPtn <- NA
  patterns <- patternData[,Pattern]
  matchResults <- sapply(patterns, grepl, detail)
  matchResults <- matchResults[matchResults == TRUE]
  if (sum(matchResults) > 1) {
    cat(c("Multiple match for", names(matchResults), "in\n", detail))
    stop()
  } else if (sum(matchResults) == 1) {
    catPtn <- patternData[Pattern == names(matchResults), Category]
    if (verbose) cat(paste0("\t'", catPtn, "' based on '", names(matchResults), "'\n"))
  }
  return(catPtn)
}
