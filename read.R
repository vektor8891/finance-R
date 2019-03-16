library(data.table)
library(readxl)
library(writexl)
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
  if (nrow(rulesCol) > 0) {
    if (verbose) cat(nrow(rulesCol), "column(s) renamed")
    setnames(d, rulesCol$From, rulesCol$To)
  }
  # Rename values
  rulesVal <- rulesAll[Type == type & Value != "Column"]
  if (nrow(rulesVal) > 0) {
    if (verbose) cat(nrow(rulesVal), "value(s) renamed")
    for (i in 1:nrow(rulesVal)) {
      d <- d[get(rulesVal[i, Value]) == rulesVal[i, From],
              c(rulesVal[i, Value]) := rulesVal[i, To]]
    }
  }
  return(d)
}

read.data <- function(fn, folder = "", dec = ".", encoding = "UTF-8",
                      sep = "auto", skip = 0, na = NULL, verbose = F) {
  # Read data from file
  #
  # Args:
  #   fn: path to file
  #   dec: decimal places
  #   encoding: file encoding
  #   sep: separator between columns
  #   skip: number of lines to skip
  #   na: replacement for NA strings
  #   verbose: print additional information
  #
  # Returns:
  #   d: data.table
  if (is.na(skip)) skip <- 0
  if (sep == "") sep <- "auto"
  if (folder != "") fn <- paste0(folder, fn)
  if (file.exists(fn)) {
    fileType <- unlist(strsplit(fn, "[.]"))[2]
    if (fileType == "csv") {
      d <- fread(fn, dec = dec, encoding = encoding, sep = sep)
    } else if (fileType %in% c("xlsx", "xls")) {
      d <- as.data.table(read_excel(fn, skip = skip))
    } else {
      cat("Unknown filetype:", fileType, "in", fn, "\n")
      stop()
    }
    if (!is.null(na)) d[is.na(d)] <- na
    if (verbose) cat(dim(dt)[[1]], "rows imported from:", fn, "\n")
    return(d)
  } else {
    if (verbose) cat("File not found:", fn, "\n")
    return(data.table())
  }
}

export.data <- function(dt, fn, encoding = "UTF-8", deleteIfEmpty = F,
                        folder = "", verbose = F) {
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
  if (folder != "") fn <- paste0(folder, fn)
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
  if (nrow(dtAll) == 0) dtAll <- dtNew[FALSE]
  dt <- merge(dtAll, dtNew, all = T)
  newRows <- dim(dt)[[1]] - dim(dtAll)[[1]]
  if (verbose) cat(paste0("\n", newRows, " new rows added to ", name, "\n"))
  return(dt)
}

get.report.type <- function(dtAll, file, verbose = F) {
  # Get report type
  #
  # Args:
  #   dtAll: list of data.tables
  #     $reportTypes: list of report types
  #   file: file name
  #   types: report types
  #   verbose: print additional information
  #
  # Returns:
  #   type: type of report
  types <- dtAll$reportTypes$Type
  for (i in 1:length(types)) {
    if (grepl(tolower(types[i]), tolower(file), fixed = TRUE)) {
      type <- types[i]
      if (verbose) cat(paste0("Report type found for ", file, ": ", type, "\n"))
      return(type)
    }
  }
  if (verbose) cat(paste0("No report type found for ", file, "\n"))
}

finalize.data <- function(dt, d) {
  # Finalize data
  #
  # - Remove extra columns
  # - Filter data for given year
  # - Add Month/AmountHUF/AmountUSD columns
  # - Add missing categories
  # - Check Account/Category values
  #
  # Args:
  #   dt: list of data.tables
  #     $year: year for filtering
  #     $fx: FX rates
  #     $income: data.table for categories
  #     $initBalance: data.table for accounts
  #   d: data.table to check
  #   checkCol: check columns
  #
  # Returns:
  #   dt: data.table
  # browser()
  if (!hasName(d, "Category")) d[, Category := character()]
  d <- d[, .(Account, Details, Date, Amount, Category, Currency)]
  d <- d[substr(Date, 1, 4) == dt$year, ]
  d[, Date := sapply(Date, function(x) gsub("-", ".", substr(x, 1, 10)))]
  d[, Month := sapply(Date, function(x) as.integer(substr(x, 6, 7)))]
  d[, Day := sapply(Date, function(x) as.integer(substr(x, 9, 10)))]
  d[, AmountHUF := Amount * dt$fx[Currency]]
  d[, AmountUSD := round(AmountHUF / dt$fx["USD"], 2)]
  d <- add.category(d, dt$patterns, verbose = F)
  check.column(dt$income, d, "Category")
  check.column(dt$initBalance, d, "Account")
  return(d)
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
  if (!is.na(catAct)) {
    catWrong <- catAct[sapply(catAct, function(x) is.na(match(x, catAll)))]
    if (length(catWrong) > 0) {
      cat(paste0("Unknown ", colName, " found\n"))
      print(catWrong)
      stop()
    }
  }
}

check.duplicates <- function(dt, threshold = 0, verbose = F) {
  # Check for duplicate rows in data table
  #
  # Args:
  #   dt: data.table
  #   threshold: throws error if more duplicates found
  #   remove: remove duplicates if TRUE
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table without duplicates
  dtDup <- dt[duplicated(dt), ]
  if (is.na(threshold)) threshold <- 0
  if (verbose) cat(paste0("Checking for duplicates (", threshold, " allowed)"))
  if (nrow(dtDup) > threshold) {
    cat("Error: duplicated transaction(s) found in data.table\n")
    print(dtDup)
    cat("HINT: to remove error set threshold to", nrow(dtDup), "\n")
    stop()
  }
}

add.category <- function(dt, patternData, verbose = F) {
  # Add category for transactions based on patterns
  #
  # Args:
  #   dt: data.table to check (must contain Detail column)
  #   patternData: data.table containing patterns 
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table
  for (row in 1:nrow(dt)) {
    if (is.na(dt[row, Category])) {
      huf <- dt[row, AmountHUF]
      detail  <- dt[row, Details]
      date  <- dt[row, Date]
      account  <- dt[row, Account]
      if (verbose) cat(paste0(date, ": ", huf, " HUF (" ,detail, ")\n"))
      catPtn <- get.category.pattern(detail, patternData, verbose = verbose)
      if (!is.na(catPtn)) {
        dt[row, Category := catPtn]
        next
      }
    }
  }
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
  patterns <- patternData[, Pattern]
  matchResults <- sapply(patterns, grepl, gsub("\u00A0", " ", detail),
                         fixed = T)
  matchResults <- matchResults[matchResults == T]
  catPatternAll <- patternData[Pattern %in% names(matchResults), Category]
  if (length(unique(catPatternAll)) > 1) {
    cat(paste0("\nMultiple match: '", names(matchResults), "' in\n", detail))
    stop()
  } else if (length(unique(catPatternAll)) == 1) {
    catPtn <- unique(catPatternAll)
    if (verbose) cat(paste0("\t'", catPtn, "' based on '", names(matchResults),
                            "'\n"))
  }
  return(catPtn)
}

get.targets <- function(dt, file, verbose = F) {
  # Get target amounts for categories
  #
  # Targets can be used to check whether the sum of transactions
  # for a given category are correct
  # 
  #
  # Args:
  #   dt: list of data.tables
  #     $folderInput: input folder
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  targetData <- read.data(file, folder = dt$folderInput)
  targetData[, TargetHUF := Target * dt$fx[Currency]]
  dt$targetHUF <- setNames(targetData$TargetHUF, targetData$Category)
  if (verbose) {
    cat("Targets:\n")
    print(targetData)
  }
  return(dt)
}

get.fx <- function(dt, file, verbose = F) {
  # Get FX rates
  #
  # Args:
  #   dt: list of data.tables
  #     $folderInput: input folder
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  fxRates <- read.data(file, folder = dt$folderInput)
  dt$fx <- setNames(fxRates$FXRate, fxRates$Currency)
  if (verbose) {
    cat("FX rates:\n")
    print(fxRates)
  }
  return(dt)
}

get.balance <- function(dt, fileInit, fileYear, verbose = F) {
  # Get initial balance sheet data
  #
  # Args:
  #   dt: list of data.tables
  #     $folderInput: input folder
  #     $fx: FX rates
  #   fileInit: path to initial balance file
  #   fileYear: file containing monthly balances
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  dt$yearBalance <- read.data(fileYear, folder = dt$folderInput, na = 0)
  dt$initBalance <- read.data(fileInit, folder = dt$folderInput)
  dt$initBalance[, InitialHUF := HUF + USD * dt$fx["USD"] + EUR * dt$fx["EUR"]]
  dt$initBalance[, InitialUSD := round(InitialHUF / dt$fx["USD"], 2)]
  return(dt)
}

get.manual <- function(dt, file, verbose = F) {
  # Get manually added transactions
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
  manual <- read.data(file, verbose = verbose)
  manual <- manual[substr(Date, 1, 4) == dt$year, ]
  manual <- finalize.data(dt, manual)
  check.duplicates(manual, threshold = 1, verbose = verbose)
  dt$all <- add.data(dt$all, manual, name = "dt$all", verbose = T)
  return(dt)
}

get.columns <- function(dt, type = "normal", negate = FALSE) {
  # Get columns for different report types
  #
  # Args:
  #   dt: data.table
  #   type: file type
  #   negate: negate values
  #
  # Returns:
  #   dt: list of data.tables
  if (type == "Cash") {
    dt[, Amount := as.numeric(sub(",", ".", Amount, fixed = TRUE))]
  } else if (type == "Unicredit") {
    dt[, Details := paste0(Partner, " | ", PartnerAccount, " | ",  Transaction)]
    dt[, Amount := sapply(Amount, function(x) 
      as.numeric(mgsub(c(",", "\u00A0", " HUF"), c(".", "", ""), x))
    )]
  } else if (type == "Egeszsegpenztar") {
    dt[, Details := paste0(Partner, " | ", Type)]
    dtTransfer = dt[Credit != 0]
    dtTransfer[, c("Details", "Credit") := list('Bank fee', CreditFinal-Credit)]
    dt <- add.data(dt, dtTransfer)
    dt[, Amount := Credit + Debit]
  } else if (type == "53_checking") {
    names(dt) <- "Details"
    dt[, Date := paste0(2019, ".", gsub("/", ".", substr(Details, 1, 5)))]
    dt[, Amount := sapply(Details, function(x) {
      as.numeric(strsplit(x, " ")[[1]][2])
      })]
    if (negate) dt$Amount <- dt$Amount * -1
  }
  return(dt)
}

get.data <- function(dt, file, type = "normal", verbose = F) {
  # Get data from file
  #
  # Args:
  #   dt: list of data.tables
  #     $: FX rates
  #   file: path to file
  #   type: file type
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  reportType <- dt$reportTypes[Type == type]
  negate <- grepl("debit", tolower(file))
  if (verbose) cat(paste0("Get data from: ", dt$folderReports, file,
                          " (type: ", type, ")\n"))
  d <- read.data(file, folder = dt$folderReports, skip = reportType$Skip,
                 sep = reportType$Separator, verbose = verbose)
  d <- rename.data(d, dt$rules, type)
  d <- get.columns(d, type, negate)
  if ("" != reportType$Currency) d$Currency <- reportType$Currency
  if ("" != reportType$Account) d$Account <- reportType$Account
  d <- finalize.data(dt, d)
  check.duplicates(d, threshold = reportType$Duplicates, verbose = verbose)
  dt$all <- add.data(dt$all, d, name = "dt$all", verbose = T)
  return(dt)
}

get.notes <- function(dt, file, verbose = F) {
  # Get data from cash inventory
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #     $folderInput: input folder
  #   file: path to file
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  cash <- read.data(file, folder = dt$folderInput)
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

find.pairs <- function(dt, days = 7, verbose = F) {
  # Find transation with same amount within date range
  #
  # Args:
  #   dt: data.table of transactions
  #   days: length of date range
  #   verbose: print additional information
  #
  # Return:
  #   dt: data.table with HasPair column
  dt$HasPair <- FALSE
  dt$RowID <- 1:nrow(dt)
  setorder(dt, Date)
  for (row in 1:nrow(dt)) {
    if (!dt[row, HasPair]) {
      huf <- dt[row, AmountHUF]
      date  <- dt[row, Date]
      account  <- dt[row, Account]
      pairRows <- dt[Account != account & AmountHUF == -huf & !HasPair]
      if (nrow(pairRows) > 0) {
        dateRow <- pairRows[1, Date]
        diff <- as.Date(dateRow, "%Y.%m.%d") - as.Date(date, "%Y.%m.%d")
        if (abs(as.numeric(diff)) <= days) {
          dt[row, HasPair := TRUE]
          dt[RowID == pairRows[1, RowID], HasPair := TRUE]
          if (verbose) cat(paste0("Pair found for ", huf, "HUF (", date, ")\n"))
        }
      }
    }
  }
  return(dt[, RowID := NULL])
}

check.category <- function(dt, catName, showAll = F, strictMode = T,
                           verbose = F) {
  # Check if transactions for given category match target
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $targets: target amounts for each categories
  #   catName: category name
  #   showAll: if TRUE, shows all transactions if no match. if FALSE, shows only
  #     those transactions where that have no opposite transaction
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  dtCat <- dt$all[Category == catName, ]
  dtCat <- find.pairs(dtCat, verbose = F)
  catSumHUF <- sum(dtCat[, AmountHUF])
  targetHUF <- 0
  if (hasName(dt$targetHUF, catName)) targetHUF <- dt$targetHUF[catName]
  diffHUF <- catSumHUF - targetHUF
  if (abs(diffHUF) > 0.01) {
    setorder(dtCat[HasPair == FALSE], Date)
    if (showAll) print(dtCat) else print(dtCat[HasPair == FALSE])
    cat(paste0("WARNING: '", catName, "' transactions don't match ",
               "with target ", targetHUF, " HUF!\n",
               "HINT: to dismiss error set target to ",
               as.numeric(catSumHUF), " HUF\n"))
    if (strictMode) stop()
  } else if (verbose) {
    cat(paste0("PASS: '", catName, "' transactions match ",
               "with target ", targetHUF, " HUF\n"))
  }
}

check.notes <- function(dt, strictMode = T, verbose = F) {
  # Check if notes match with cash transactions
  #
  # Compare total value of coins and notes with initial cash balance and
  # total cash transations. Throws error when amounts don't match. Error
  # can be manually remidiated by adjusting initial cash balance.
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
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
    if (strictMode) stop()
  } else if (verbose) cat("PASS: Notes and cash transations match (adjustment:",
                          dt$adjustmentHUF, "HUF)\n")
}

check.balance <- function(dt, strictMode = T, verbose = F) {
  # Check if transactions match with balances
  #
  # Compare total value of coins and notes with initial cash balance and
  # total cash transations. Throws error when amounts don't match. Error
  # can be manually remidiated by adjusting initial cash balance.
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  dtBal <- dt$yearBalance
  if (nrow(dtBal) > 0) {
    for (row in 1:nrow(dtBal)) {
      account <- dtBal[row, Account]
      month <- dtBal[row, Month]
      ccy <- dtBal[row, Currency]
      day <- ifelse(dtBal[row, Day] != 0, dtBal[row, Day], 31)
      finalDate <- ifelse(day == 31, paste0("month ", month),
                          paste0(month, "/", day))
      fx <- dt$fx[ccy]
      adjustOld <- sum(dtBal[Account == account & Month < month, Adjustment])
      adjustNew <- sum(dtBal[row, Adjustment])
      adjustVal <- adjustOld + adjustNew
      balance <- dtBal[row, Balance] + adjustVal
      initial <- round(dt$initBalance[Account == account, InitialHUF] / fx, 2)
      trans <- round(sum(dt$all[Account == account & (Month < month
                                | (Month == month & Day <= day)), AmountHUF])
                     / fx, 2)
      current <- initial + trans
      diff <- current - balance
      adjustValNew <- adjustNew + diff
      if (abs(diff) > 0.01) {
        browser()
        View(dt$all[Account == account])
        cat(paste0("WARNING: ", account, " transactions don't match with ",
                   "balance for ", finalDate, "!\n",
                   "A. Initial:\t\t", ccy, " ", initial, "\n",
                   "B. Transactions:\t", ccy, " ", trans, "\n",
                   "C. Current (A+B):\t", ccy, " ", current, "\n",
                   "D. Balance:\t\t", ccy, " ", balance, "\n",
                   "E. Difference (D-C):\t", ccy, " ", current - balance, "\n",
                   "HINT: to dismiss error set adjustment for month ", month,
                   " to ", ccy, " ", adjustValNew, "\n"))
        if (strictMode) stop()
      } else if (verbose) {
        cat(paste0("PASS: ", account, " transactions match with ",
                   "balance for month ", month, " (adjustment: ", ccy, " ",
                   adjustVal, ")\n"))
      }
    }
  }
}

check.data <- function(dt, showAll = F, strictMode = T, verbose = F) {
  # Get cash transactions
  #
  # Args:
  #   dt: list of data.tables
  #   file: path to file
  #   showAll: if TRUE, shows all transactions in check.category()
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  check.notes(dt, strictMode = strictMode, verbose = verbose)
  check.balance(dt, strictMode = strictMode, verbose = verbose)
  check.category(dt, catName = "Withdrawal", strictMode = strictMode,
                 verbose = verbose)
  check.category(dt, catName = "Transfer", showAll = showAll,
                 strictMode = strictMode, verbose = verbose)
}
