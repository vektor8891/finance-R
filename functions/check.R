library(data.table)

check.all <- function(dt, showPairs = F, strictMode = T, verbose = F) {
  # Check transactions
  #
  # Args:
  #   dt: list of data.tables
  #   showPairs: if TRUE, shows all transactions pairs in check.category()
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  check.missing.category(dt$all, strictMode = strictMode, verbose = verbose)
  check.patterns(dt$patterns, strictMode = strictMode, verbose = verbose)
  check.cash(dt, strictMode = strictMode, verbose = verbose)
  check.balance(dt, strictMode = strictMode, verbose = verbose)
  check.duplicates(dt, strictMode = strictMode, verbose = verbose)
  check.latest(dt, strictMode = strictMode, verbose = verbose)
  check.frequency(dt, strictMode = strictMode, verbose = verbose)
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
  #     $monthlyBalance: monthly balances
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  bal <- dt$monthlyBalance[Account != "Cash" & !is.na(Balance)]
  if (nrow(bal) > 0) {
    for (row in 1:nrow(bal)) {
      account <- bal[row, Account]
      month <- bal[row, Month]
      ccy <- bal[row, Currency]
      day <- ifelse(is.na(bal[row, Day]), 31, bal[row, Day])
      finalDate <- ifelse(day == 31, paste0("month ", month),
                          paste0(month, "/", day))
      fx <- dt$fx[ccy]
      adjust <- ifelse(is.na(bal[row, Adjustment]), 0, bal[row, Adjustment])
      multiply <- dt$initBalance[Account == account, Multiply]
      balance <- ifelse(is.na(multiply), bal[row, Balance], bal[row, Balance] * multiply)
      initial <- round(dt$initBalance[Account == account, InitialHUF] / fx, 2)
      trans <- round(sum(dt$all[Account == account & (Month < month
                                                      | (Month == month & Day <= day)), AmountHUF])
                     / fx, 2)
      total <- initial + trans
      diff <- total - balance
      adjustNew <- round(adjust - diff, 2)
      adjComment <- ifelse(adjust == 0, "", paste0("\n\t(*adjustment: ", ccy, " ",
                                                   adjust, ")"))
      if (abs(diff) > 0.01) {
        cat(paste0("WARNING: ", account, " balance error for ", finalDate,
                   "!\n", "A. Initial:\t\t", ccy, " ", initial, "\n",
                   "B. Transactions:\t", ccy, " ", trans, "\n",
                   "C. Current (A+B):\t", ccy, " ", total, "\n",
                   "D. Balance:\t\t", ccy, " ", balance, "\n",
                   "E. Difference (D-C):\t", ccy, " ", total - balance, "\n",
                   "HINT: to dismiss error set adjustment for month ", month,
                   " to ", ccy, " ", adjustNew, "\n"))
        if (strictMode) stop()
      } else if (verbose) {
        cat(paste0("PASS: ", account, " balance correct for ", finalDate,
                   adjComment,"\n"))
      }
    }
  }
}

check.cash <- function(dt, strictMode = T, verbose = F) {
  # Check if notes match with cash transactions
  #
  # Compare total value of coins and notes with initial cash balance and
  # total cash transations. Throws error when amounts don't match. Error
  # can be manually remidiated by adjusting initial cash balance.
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #     $initBalance: initial balance
  #     $all: all transactions
  #     $notes: cash inventory
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  initialHUF <- dt$initBalance[Account == "Cash", InitialHUF]
  adjustHUF <- sum(dt$all[Account == "Cash" & Category == "Adjustment",
                          AmountHUF])
  notesHUF <- sum(dt$notes$AmountHUF)
  transHUF <- sum(dt$all[Account == "Cash", AmountHUF])
  currentHUF <- initialHUF + transHUF
  diffHUF <- currentHUF - notesHUF
  adjustNewHUF <- round(adjustHUF - diffHUF, 2)
  if (abs(diffHUF) > 0.01) {
    cat("WARNING: Mismatch between notes and cash transactions!\n",
        "A. Initial:\t\tHUF", initialHUF, "\n",
        "B. Transactions:\tHUF", transHUF, " - adjustment:", adjustHUF,"\n",
        "C. Current (A+B):\tHUF", currentHUF, "\n",
        "D. Notes:\t\tHUF", notesHUF, "\n",
        "------------------------------------\n",
        "G. DIFFERENCE (D-C):\tHUF", diffHUF, "\n",
        "HINT: to dismiss error set initial cash adjustment to HUF",
        adjustNewHUF,"(E+G)\n")
    browser()
    if (strictMode) stop()
  } else if (verbose) cat("PASS: Notes and cash transations match (adjustment:",
                          adjustHUF, "HUF)\n")
}

check.duplicates <- function(dt, strictMode = T, verbose = F) {
  # Check for duplicates in transactions
  #
  # Check if:
  #   1. duplicated transactions for given account & month match with threshold
  #   2. total duplicated rows match total threshold
  #
  # Args:
  #   dt: list of data.tables
  #     $monthlyBalance: monthly balances
  #     $all: all transactions
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table without duplicates
  dup <- dt$monthlyBalance[Duplicates > 0]
  if (nrow(dup) > 0) {
    for (row in 1:nrow(dup)) {
      threshold <- dup[row, Duplicates]
      account <- dup[row, Account]
      month <- dup[row, Month]
      dupTrans <- dt$all[Account == account & Month == month & 
                           duplicated(dt$all), ]
      if (nrow(dupTrans) != threshold) {
        cat("Error: duplicated rows in ", account, "for month ",
            month, " != ", threshold, "\n")
        print(dupTrans)
        cat("HINT: to remove error set threshold to", nrow(dupTrans), "\n")
        if (strictMode) stop()
      } else {
        cat(paste0("PASS: duplicated rows in ", account, " for month ",
                   month, " = ", threshold, "\n"))
      }
    }
  }
  thresholdTotal <- sum(dt$monthlyBalance[Duplicates > 0, Duplicates])
  dtDupTotal <- dt$all[duplicated(dt$all), ]
  if (nrow(dtDupTotal) != thresholdTotal) {
    cat("Error: total duplicated rows != ", thresholdTotal, "\n")
    print(dtDupTotal)
    cat("HINT: to remove error set threshold to ", nrow(dtDupTotal), "\n")
    if (strictMode) stop()
  } else {
    cat(paste0("PASS: total duplicated rows = ", thresholdTotal, "\n"))
  }
}

check.missing.category <- function(dt, strictMode = T, verbose = F) {
  # Check for missing categories
  #
  # Args:
  #   dt: data.tables for transactions
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  dtMissing <- setorder(dt, Category)[is.na(Category)]
  if (nrow(dtMissing) == 0) {
    if (verbose) cat("PASS: no missing categories found\n")
  } else {
    if (verbose) cat("FAIL: missing categories found:\n")
    print(dtMissing)
    View(dtMissing)
    if (strictMode) stop()
  }
}

check.patterns <- function(dt, strictMode = T, verbose = F) {
  # Check if all patterns are used to assign category
  #
  # Categories are assigned by patterns. If pattern is not used, it should be
  # deleted.
  #
  # Args:
  #   dt: data.table for patterns
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  unused <- dt[Category != "Adjustment" & Match == 0 & Used != "N"]
  if (nrow(unused) > 0) {
    if (verbose) cat("FAIL: unused patterns found:\n")
    print(unused)
    View(unused)
    if (strictMode) stop()
  } else {
    if (verbose) cat("PASS: no unused patterns found\n")
  }
}

check.latest <- function(dt, strictMode = T, verbose = F) {
  # Check if latest transaction for account is before threshold
  #
  # Args:
  #   dt: list of data.tables
  #     $all: transactions
  #     $initBalance: initial balance values
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  dtThreshold <- dt$initBalance[!is.na(NoTransactionAfter)]
  if (nrow(dtThreshold) > 0) {
    for (row in 1:nrow(dtThreshold)) {
      account <- dtThreshold[row, Account]
      dtAcc <- dt$all[Account == account]
      latest <- ifelse(nrow(dtAcc) > 0, max(dtAcc$Date), paste0(dt$year, ".01.01"))
      threshold <- dtThreshold[row, NoTransactionAfter]
      # cat(account, latest, threshold)
      if (as.Date(latest, format = "%Y.%m.%d") >
          as.Date(threshold, format = "%Y.%m.%d")) {
        if (verbose) cat("FAIL:", account, ":", latest, "(latest) >",
                         threshold, "\n")
        if (strictMode) stop()
      } else {
        if (verbose) cat("PASS:", account, ":", latest, "(latest) <=",
                         threshold, "\n")    }
    }
  }
}

check.frequency <- function(dt, strictMode = T, verbose = F) {
  # Check if transactions have been updated for account
  #
  # Args:
  #   dt: list of data.tables
  #     $all: transactions
  #   strictMode: if check fails throws error if TRUE and warning if FALSE
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table without duplicates
  dtFreq <- dt$initBalance[!is.na(UpdateFrequencyDays)]
  for (row in 1:nrow(dtFreq)) {
    account <- dtFreq[row, Account]
    dtAcc <- dt$all[Account == account]
    latest <- ifelse(nrow(dtAcc) > 0, max(dtAcc$Date), paste0(dt$year, ".01.01"))
    freq <- dtFreq[row, UpdateFrequencyDays]
    diff <- as.numeric(difftime(Sys.Date(),
                                as.Date(latest, format = "%Y.%m.%d"),
                                units = "days"))
    freqDays <- dtFreq[1, UpdateFrequencyDays]
    if (diff > freq) {
      if (verbose) cat("FAIL:", account, "latest update", diff, "days ago >",
                       freq, "\n")
      if (strictMode) stop()
    } else {
      if (verbose) {
        cat("PASS:", account, "latest update", diff, "days ago <=", freq, "\n")
      }
    }
  }
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
  setorder(dt, Date)
  dt$HasPair <- FALSE
  dt$RowID <- 1:nrow(dt)
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

