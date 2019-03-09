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

read.data <- function(fn, dec = ".", encoding = "UTF-8", skip = 0,
  verbose = F) {
  # Read data from file
  #
  # Args:
  #   fn: path to file
  #   dec: decimal places
  #   encoding: file encoding
  #   skip: number of lines to skip
  #   verbose: print additional information
  #
  # Returns:
  #   d: data.table
  if (file.exists(fn)) {
    fileType <- unlist(strsplit(fn, "[.]"))[2]
    if (fileType == "csv") {
      d <- fread(fn, dec = dec, encoding = encoding)
    } else if (fileType %in% c("xlsx", "xls")) {
      d <- as.data.table(read_excel(fn, skip = skip))
    } else {
      cat("Unknown filetype:", fileType, "in", fn, "\n")
      stop()
    }
    if (verbose) cat(dim(d)[[1]], "rows imported from:", fn)
    return(d)
  } else {
    if (verbose) cat("File not found:", fn)
    return(data.table())
  }
}

export.data <- function(dt, fn, encoding = "UTF-8", deleteIfEmpty = F,
                        verbose = F) {
  # Export data to file
  #
  # Args:
  #   dt: data.table
  #   fn: path to file
  #   encoding: file encoding
  #   deleteIfEmpty: delete file if data table is empty
  #   verbose: print additional information
  #
  # Returns:
  #   d: data.table
  if (nrow(dt) > 0) {
    fileType <- unlist(strsplit(fn, "[.]"))[2]
    if (fileType == "csv") {
      write.csv(dt, fn, fileEncoding = encoding, row.names = FALSE)
    } else {
      cat("Unknown filetype:", fileType, "in", fn, "\n")
      stop()
    }
    if (verbose) cat(dim(dt)[[1]], "rows exported to:", fn, "\n")
  } else {
    if (verbose) cat("Data table empty - nothing exported to:", fn, "\n")
    if (deleteIfEmpty & file.exists(fn)) {
      file.remove(fn)
      if (verbose) cat("File deleted:", fn, "\n")
    }
  }
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

read.cash <- function(file, year, fxRates, rules, verbose = F) {
  # Read cash transactions from BlueCoins output file for given year
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
  dt <- rename.data(dt, rules, "Cash")
  dt <- dt[substr(Date, 1, 4) == year, ]
  dt[, Date := sapply(Date, function(x) gsub("-", ".", substr(x, 1, 10)))]
  dt[, Month := sapply(Date, function(x) strtoi(substr(x, 6, 7)))]
  dt[, Amount := as.numeric(sub(",", ".", Amount, fixed = TRUE))]
  dt[, AmountHUF := Amount * fxRates[Currency]]
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

check.duplicates <- function(dt, stop = F, verbose = F) {
  # Check for duplicate rows in data table
  #
  # Args:
  #   dt: data.table
  #   stop: if TRUE, stops if duplicates found
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table without duplicates
  dtDup <- dt[duplicated(dt), ]
  if (nrow(dtDup) > 0 & stop) {
    cat("Error: duplicated transaction(s) found in:", file, "\n")
    print(dt[duplicated(dt), ])
    stop()
  }
  dt <- dt[!duplicated(dt), ]
  if (verbose) cat(c("\n", dim(dtDup)[[1]], "duplicate(s) removed"))
  return(dt)
}

add.category <- function(dt, patternData, skip = T, verbose = F) {
  # Add category for transactions based on patterns
  #
  # Args:
  #   dt: data.table to check (must contain Detail column)
  #   patternData: data.table containing patterns 
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

      catPtn <- get.category.pattern(detail, patternData, verbose = verbose)
      if (!is.na(catPtn)) {
        dt[row, c("Category", "Source") := list(catPtn, "Patterns")]
        next
      }
    }
  }
  # browser()
  # View(dt)
  dt <- dt[,.(Category = unlist(Category)), by = setdiff(names(dt), 'Category')]
  return(dt)
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

get.data.targets <- function(dt, file, verbose = F) {
  # Get target amounts for categories
  #
  # Targets can be used to check whether the sum of transactions
  # for a given category are correct
  # 
  #
  # Args:
  #   dt: list of data.tables
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  targetData <- read.data(file)
  targetData[, TargetHUF := Target * dt$fx[Currency]]
  dt$targetHUF <- setNames(targetData$TargetHUF, targetData$Category)
  if (verbose) {
    cat("Targets:\n")
    print(targetData)
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

get.data.cash <- function(dt, file, verbose = F) {
  # Get cash transactions
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
  cash <- read.cash(file, dt$year, dt$fx, dt$rules, verbose = T)
  check.column(dt$income, cash, "Category")
  if (nrow(cash[Account != "Cash"]) > 0) {
    cat("Error: non-cash transaction(s) found in:", file, "\n")
    print(cash[Account != "Cash"])
    stop()
  }
  check.duplicates(cash, stop = T, verbose = verbose)
  dt$all <- add.data(dt$all, cash, name = "dt$all", verbose = T)
  return(dt)
}

get.data.unicredit <- function(dt, file, verbose = F) {
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
  dt$uni <- add.category(dt$uni, patterns, skip = F, verbose = F)
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
  adjustmentColName <- "We don't know!!!"
  sapply(unique(cash[, Currency]), function (ccy) {
    adjustmentCcy <- cash[Note == adjustmentColName & Currency == ccy]
    if (nrow(adjustmentCcy) != 1) {
      cat(paste0(nrow(adjustmentCcy), " adjustment row(s) with note '",
                 adjustmentColName, "'found for ", ccy, " (1 expected)\n"))
      stop()
    }
  })
  dt$adjustmentHUF <- sum(cash[Note == adjustmentColName, AmountHUF])
  dt$adjustmentUSD <- sum(cash[Note == adjustmentColName, AmountUSD])
  dt$notesHUF <- sum(cash[Note != adjustmentColName, AmountHUF])
  dt$notesUSD <- sum(cash[Note != adjustmentColName, AmountUSD])
  if (verbose) {
    cat(paste0("Notes: ", dt$notesHUF, " HUF\t",
               "(", dt$notesUSD, " USD)\n",
               "Adjustment: ", dt$adjustmentHUF, " HUF\t",
               "(", dt$adjustmentUSD, " USD)\n"))
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
  totalHUF <- dt$notesHUF + dt$adjustmentHUF
  diffHUF <- currentHUF - totalHUF
  if (abs(diffHUF) > 0.01) {
    cat("WARNING: Mismatch between notes and cash transactions!\n",
        "A. Initial:\t\tHUF ", initialHUF, "\n",
        "B. Transactions:\tHUF ", amountHUF, "\n",
        "C. Current (A+B):\tHUF ", currentHUF, "\n",
        "D. Notes:\t\tHUF ", dt$notesHUF, "\n",
        "E. Adjustment:\t\tHUF ", dt$adjustmentHUF, "\n",
        "F. Total (D+E):\tHUF ", totalHUF, "\n",
        "------------------------------------\n",
        "G. DIFFERENCE (F-C):\tHUF ", diffHUF, "\n",
        "HINT: to dismiss error change initial cash adjustment to HUF",
        dt$adjustmentHUF + diffHUF,"(E+G)\n")
    stop()
  } else if (verbose) cat("PASS: Notes and cash transations match (adjustment:",
                          dt$adjustmentHUF, "HUF)\n")
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
                   "balance for month ", month, " (adjustment: HUF ",
                   adjustVal, ")\n"))
      }
    }
  }
}

check.category <- function(dt, catName, verbose = F) {
  # Check if transactions for given category match target
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $targets: target amounts for each categories
  #   catName: category name
  #   verbose: print additional information
  dtCat <- dt$all[Category == catName, ]
  catSumHUF <- sum(dtCat[, AmountHUF])
  if (hasName(dt$targetHUF, catName)) {
    targetHUF <- dt$targetHUF[catName]
  } else {
    targetHUF <- 0
  }
  
  diffHUF <- catSumHUF - targetHUF
  if (abs(diffHUF) > 0.01) {
    print(setorder(dtCat, Date))
    cat(paste0("WARNING: '", catName, "' transactions don't match ",
               "with target ", targetHUF, " HUF!\n",
               "HINT: to dismiss error set target to ", diffHUF, " HUF"))
    stop()
  } else if (verbose) {
    cat(paste0("PASS: '", catName, "' transactions match ",
               "with target ", targetHUF, " HUF\n"))
  }
}


check.data <- function(dt, verbose = F) {
  # Get cash transactions
  #
  # Args:
  #   dt: list of data.tables
  #   file: path to file
  #   verbose: print additional information
  check.notes(dt, verbose = verbose)
  check.balance(dt, verbose = verbose)
  check.category(dt, catName = "Withdrawal", verbose = verbose)
  check.category(dt, catName = "Transfer", verbose = verbose)
}