library(data.table)
library(readxl)
library(qdap)

rename.data <- function(d, rulesAll, type, column = F, verbose = F) {
  # Rename values in data.table
  #
  # Args:
  #   d: original data.table
  #   rulesAll: data.table containing all renaming rules
  #      colnames(rules) = c('Type','Value','From','To')
  #   type: filter for 'Type' column in rules data.table
  #   column: if T, columns are renamed
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

read.data <- function(file, dec = ".", encoding = "UTF-8", skip = 0,
  verbose = F) {
  # Read data from file
  #
  # Args:
  #   file: path to file
  #   dec: decimal places
  #   encoding: file encoding
  #   skip: number of lines to skip
  #   verbose: print additional information
  #
  # Returns:
  #   d: data.table
  fileType <- unlist(strsplit(file, "[.]"))[2]
  if (fileType == "csv") {
    d <- fread(file, dec = dec, encoding = encoding)
  } else if (fileType %in% c("xlsx", "xls")) {
    d <- as.data.table(read_excel(file, skip = skip))
  } else {
    cat("Unknown filetype:", fileType, "in", file, "\n")
    stop()
  }
  if (verbose) cat(dim(d)[[1]], "rows imported from:", file)
  return(d)
}

export.data <- function(dt, file, encoding = "UTF-8", verbose = F) {
  # Export data to file
  #
  # Args:
  #   dt: data.table
  #   file: path to file
  #   encoding: file encoding
  #   verbose: print additional information
  #
  # Returns:
  #   d: data.table
  fileType <- unlist(strsplit(file, "[.]"))[2]
  if (fileType == "csv") {
    write.csv(dt, file, fileEncoding = encoding, row.names = FALSE)
  } else {
    cat("Unknown filetype:", fileType, "in", file, "\n")
    stop()
  }
  if (verbose) cat(dim(dt)[[1]], "rows exported to:", file)
}

add.data <- function(dtAll, dtNew, name = "data.table", verbose = F) {
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
  # View(dtAll)
  # View(dtNew)
  dt <- merge(dtAll, dtNew, all = T)
  newRows <- dim(dt)[[1]] - dim(dtAll)[[1]]
  if (verbose) cat(paste0("\n", newRows, " new rows added to ", name))
  return(dt)
}

read.bluecoins <- function(file, year, fxRates, rules, verbose = F) {
  # Read data from BlueCoins output file for given year
  #
  # Args:
  #   file: path to file
  #   year: year
  #   fxRates: named list of fxRates
  #   rules: data.table containing all renaming rules
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table
  dt <- read.data(file, verbose = verbose)
  dt <- dt[, .(Date, Title, Amount, Currency, Category, Account)]
  dt <- rename.data(dt, rules, "BlueCoins")
  dt <- dt[substr(Date, 1, 4) == year, ]
  dt[, Date := sapply(Date, function(x) gsub("-", ".", substr(x, 1, 10)))]
  dt[, Month := sapply(Date, function(x) strtoi(substr(x, 6, 7)))]
  dt[, AmountHUF := Amount]
  dt[Currency == "USD", AmountHUF := Amount * fxRates["USD"]]
  dt[Currency == "EUR", AmountHUF := Amount * fxRates["EUR"]]
  dt[, AmountUSD := round(AmountHUF / fxRates["USD"], 2)]
  dt[, Source := "BlueCoins"]
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
  dt <- read.data(fileName, skip = 3, verbose = verbose)
  dt <- rename.data(dt, renameRules, "Unicredit")
  # browser()
  dt[, Details := paste0(Partner, " | ",  PartnerAccount, " | ", Transaction)]
  dt <- dt[, .(Details, Date, Amount)]
  dt[, c("Category", "Source") := character()]
  dt <- dt[substr(Date, 1, 4) == year, ]
  dt <- dt[, Account := "V.Uni"]
  dt[, Month := sapply(Date, function(x) strtoi(substr(x, 6, 7)))]
  dt[, AmountHUF := sapply(Amount, function(x) 
    as.numeric(mgsub(c(",", "\u00A0", " HUF"), c(".", "", ""), x))
  )]
  dt[, AmountUSD := round(AmountHUF / fxRates["USD"], 2)]
  dt[, c("Amount", "Currency") := list(AmountHUF, "HUF")]
  return(dt)
}

check.column <- function(dataAll, dataCurrent, colName) {
  # Check for unrecognizeable value in column
  #
  # Args:
  #   dataAll: data.table containing all correct values
  #   dataCurrent: data.table containing current values
  #   colName: column name to check
  #
  # Returns:
  #   Nothing. Throws an error if unknown value found.
  catAll <- unique(dataAll[[colName]])
  catAct <- unique(dataCurrent[[!is.na(colName), colName]])
  # browser()
  if (!is.na(catAct)) {
    catWrong <- catAct[sapply(catAct, function(x) is.na(match(x, catAll)))]
    if (length(catWrong) > 0) {
      browser()
      cat(paste0("Unknown ", colName, " found\n"))
      print(catWrong)
      stop()
    }
  }
}

add.category <- function(dt, bcData, patternData, manData, skip = T,
  verbose = F) {
  # Add category for transactions
  #
  # Add category for transactions based on the following hierarchy:
  #   1) historical data
  #   2) patterns
  #   3) manual transactions
  #
  # Args:
  #   dt: data.table to check (must contain Detail column)
  #   bcData: data.table with BlueCoins transactions
  #   patternData: data.table containing patterns 
  #   manData: data.table with manual transactions
  #   skip: if TRUE skip rows where Category is not NA
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table
  # dt <- dt[, Category := character()]
  # dt <- dt[, Source := character()]
  
  for (row in 1:nrow(dt)) {
    if (!is.na(dt[row, Category]) | !skip) {
      huf <- dt[row, AmountHUF]
      detail  <- dt[row, Details]
      date  <- dt[row, Date]
      account  <- dt[row, Account]

      if (verbose) cat(paste0(date, ": ", huf, " HUF (" ,detail, ")\n"))
      # browser()
      # 1) From BlueCoins transations
      catBC <- get.category.data(bcData, huf, date, account, verbose = verbose)
      if (!is.na(catBC)) {
        dt[row, c("Category", "Source") := list(catBC, "BlueCoins")]
        next
      }

      # 2) Based on patterns
      catPtn <- get.category.pattern(detail, patternData, verbose = verbose)
      if (!is.na(catPtn)) {
        dt[row, c("Category", "Source") := list(catPtn, "Patterns")]
        next
      }

      # 3) From manual transations
      catMan <- get.category.manual(manData, huf, date, account, verbose = T)
      if (!is.na(catMan)){
        dt[row, c("Category", "Source") := list(catMan, "Manual")]
        next
      }
    }
  }
  # browser()
  # View(dt)
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
  matchResults <- sapply(patterns, grepl, detail, fixed = T)
  matchResults <- matchResults[matchResults == T]
  catPtnAll <- patternData[Pattern %in% names(matchResults), Category]
  if (length(unique(catPtnAll)) > 1) {
    cat(paste0("\nMultiple match: '", names(matchResults), "' in\n", detail))
    browser()
    stop()
  } else if (length(unique(catPtnAll)) == 1) {
    catPtn <- unique(catPtnAll)
    if (verbose) cat(paste0("\t'", catPtn, "' based on '", names(matchResults), "'\n"))
  }
  return(catPtn)
}

get.category.manual <- function(manData, huf, date, account, verbose = F) {
  # Get category for transation from manual transactions
  #
  # Args:
  #   manData: data.table with manual transactions
  #   huf: amount in HUF
  #   date: transaction date
  #   account: account name
  #   verbose: print additional information
  #
  # Returns:
  #   catPtn: category
  dtFilt <- manData[!is.na(Category) & AmountHUF == huf & Date == date & Account == account]
  if (nrow(dtFilt) == 1) {
    catMan <- dtFilt[1, Category]
    if (verbose) cat(paste0("\n\t'", catMan, "' based on manual edit\n"))
  } else if (nrow(dtFilt) > 1) {
    cat("Multiple values found:\n")
    print(dtFilt)
    stop()
  } else {
    catMan <- NA
  }
  return(catMan)
}

get.data.all <- function(dt, file, empty = F, verbose = F) {
  # Get all transactions or empty data.table
  #
  # Args:
  #   dt: list of data.tables
  #   file: path to file
  #   empty: if T returns empty data.table
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  dt$all <- read.data(file, verbose = (!empty & verbose))
  if (empty) {
    if (verbose) cat("read empty data.table for dt$all")
    dt$all <- data.table(Account = character(),
                         Date = character(),
                         Month = numeric(),
                         Category = character(),
                         Source = character(),
                         Amount = numeric(),
                         Currency = character(),
                         AmountHUF = numeric(),
                         AmountUSD = numeric(),
                         Details = character())
    # browser()
  }
  return(dt)
}

get.data.fx <- function(dt, file, verbose = F) {
  # Get FX rates
  #
  # Args:
  #   dt: list of data.tables
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  fxRates <- read.data(file)
  dt$fx <- setNames(fxRates$FXRate, fxRates$Currency)
  if (verbose) {
    cat("FX rates:\n")
    print(fxRates)
  }
  return(dt)
}

get.data.balance <- function(dt, fileInit, fileYear, verbose = F) {
  # Get initial balance sheet data
  #
  # Args:
  #   dt: list of data.tables
  #   fileInit: path to initial balance file
  #   fileYear: file containing monthly balances
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  dt$initBalance <- read.data(fileInit)
  dt$initBalance[, InitialHUF := HUF + USD * dt$fx["USD"] + EUR * dt$fx["EUR"]]
  dt$initBalance[, InitialUSD := round(InitialHUF / dt$fx["USD"], 2)]
  dt$yearBalance <- read.data(fileYear)
  return(dt)
}

get.data.manual <- function(dt, file, verbose = F) {
  # Get manual transactions or empty data.table
  #
  # Args:
  #   dt: list of data.tables
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  dt$manual <- read.data(file, verbose = verbose)
  check.column(dt$income, dt$manual[!is.na(Category)], "Category")
  check.column(dt$initBalance, dt$manual, "Account")
  dtDup <- dt$manual[duplicated(dt$manual), ]
  if (nrow(dtDup) > 0) {
    dt$manual <- dt$manual[!duplicated(dt$manual), ]
    if (verbose) cat(c("\n", dim(dtDup)[[1]], "duplicate(s) removed"))
  }
  return(dt)
}

get.data.bc <- function(dt, file, verbose = F) {
  # Get BlueCoins transactions
  #
  # Args:
  #   dt: list of data.tables
  #     $rules: renaming rules
  #     $income: income categories
  #     $balance: balance categories
  #     $year: year to check
  #     $fx: FX rates
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  dt$bc <- read.bluecoins(file, dt$year, dt$fx, dt$rules, verbose = T)
  check.column(dt$income, dt$bc, "Category")
  check.column(dt$initBalance, dt$bc, "Account")
  cash <- dt$bc[Account == "Cash"]
  dt$all <- add.data(dt$all, cash, name = "dt$all", verbose = T)
  return(dt)
}

get.data.uni <- function(dt, file, verbose = F) {
  # Get Unicredit transactions
  #
  # Args:
  #   dt: list of data.tables
  #     $rules: renaming rules
  #     $income: income categories
  #     $balance: balance categories
  #     $year: year to check
  #     $fx: FX rates
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  dt$uni <- read.unicredit(file, dt$year, dt$fx, dt$rules, verbose = verbose)
  patterns <- dt$patterns[Type == "Unicredit"]
  dt$uni <- add.category(dt$uni, dt$bc, patterns, dt$manual, skip = F,
                         verbose = F)
  dt$all <- add.data(dt$all, dt$uni, name = "dt$all", verbose = verbose)
  return(dt)
}

get.data.notes <- function(dt, file, verbose = F) {
  # Get data from cash inventory
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  cash <- read.data(file)
  cash[, AmountHUF := Notional * Amount * dt$fx[Currency]]
  cash[, AmountUSD := round(AmountHUF / dt$fx["USD"], 2)]
  if (nrow(cash[is.na(AmountHUF), ]) > 0) {
    cat("Error: cash conversion failed:\n")
    print(cash[is.na(AmountHUF), ])
    stop()
  }
  dt$notesHUF <- sum(cash[, AmountHUF])
  dt$notesUSD <- sum(cash[, AmountUSD])
  if (verbose) {
    cat(paste0("Notes HUF: ", dt$notesHUF, "\nNotes USD: ", dt$notesUSD))
  }
  return(dt)
}

check.notes <- function(dt, verbose = F) {
  # Check if notes match with cash transactions
  #
  # Compare total value of coins and notes with initial cash balance and
  # total cash transations. Throws error when amounts don't match. Error
  # can be manually remidiated by adjusting initial cash balance.
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  initialHUF <- dt$initBalance[Account == "Cash", InitialHUF]
  amountHUF <- sum(dt$all[Account == "Cash", AmountHUF])
  currentHUF <- initialHUF + amountHUF
  notesHUF <- dt$notesHUF
  if (currentHUF != notesHUF) {
    cat("WARNING: Mismatch between notes and cash transactions!\n",
        "A. Initial:\t\tHUF ", initialHUF, "\n",
        "B. Transactions:\tHUF ", amountHUF, "\n",
        "C. Current (A+B):\tHUF ", currentHUF, "\n",
        "D. Notes:\t\tHUF ", notesHUF, "\n",
        "E. Difference (D-C):\tHUF ", currentHUF - notesHUF, "\n",
        "HINT: to dismiss error adjust initial cash balance with E\n")
    stop()
  } else if (verbose) cat("PASS: Notes and cash transations match.\n")
}

check.balance <- function(dt, verbose = F) {
  # Check if transactions match with balances
  #
  # Compare total value of coins and notes with initial cash balance and
  # total cash transations. Throws error when amounts don't match. Error
  # can be manually remidiated by adjusting initial cash balance.
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #   file: path to file
  #   verbose: print additional information
  balance <- dt$yearBalance
  if (nrow(balance) > 0) {
    for (row in 1:nrow(balance)) {
      # browser()
      account <- balance[row, Account]
      month <- balance[row, Month]
      adjustVal <- balance[row, Adjustment]
      balanceVal <- balance[row, Balance]
      balanceHUF <- (balanceVal + adjustVal) * dt$fx[balance[row, Currency]]
      initialHUF <- dt$initBalance[Account == account, InitialHUF]
      amountHUF <- sum(dt$all[Account == account & Month <= month, AmountHUF])
      currentHUF <- initialHUF + amountHUF
      if (currentHUF != balanceHUF) {
        cat(paste0("WARNING: ", account, " transactions don't match with ",
                   "balance for month ", month, "!\n",
                   "A. Initial:\t\tHUF ", initialHUF, "\n",
                   "B. Transactions:\tHUF ", amountHUF, "\n",
                   "C. Current (A+B):\tHUF ", currentHUF, "\n",
                   "D. Balance:\t\tHUF ", balanceHUF, "\n",
                   "E. Difference (D-C):\tHUF ", currentHUF - balanceHUF, "\n",
                   "HINT: to dismiss error adjust monthly balance with E\n"))
        stop()
      } else if (verbose) {
        cat(paste0("PASS: ", account, " transactions match with ",
                   "balance for month ", month, " (Adjustment: HUF ",
                   adjustVal, ")\n"))
      }
    }
  }
}

check.data <- function(dt, verbose = F) {
  # Get cash transactions
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #   file: path to file
  #   verbose: print additional information
  check.notes(dt, verbose = verbose)
  check.balance(dt, verbose = verbose)
}