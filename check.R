balance.summary <- function(dt) {
  # Summarize initial balance
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #
  # Returns:
  #   dt: list of data.tables
  dt$initBalance[, InitialHUF := HUF + USD * dt$fx["USD"] + EUR * dt$fx["EUR"]]
  dt$initBalance[, InitialUSD := round(InitialHUF / dt$fx["USD"], 2)]
  return(dt)
}

cash.summary <- function(dt, adjustment = "We don't know!!!", verbose = F) {
  # Summarize cash inventory
  #
  # Args:
  #   dt: list of data.tables
  #     $fx: FX rates
  #     $folderInput: input folder
  #   file: path to file
  #   adjustmentColName: name for manual adjustments
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  dt$notes[, AmountHUF := Notional * Amount * dt$fx[Currency]]
  dt$notes[, AmountUSD := round(AmountHUF / dt$fx["USD"], 2)]
  if (nrow(dt$notes[is.na(AmountHUF), ]) > 0) {
    cat("Error: cash conversion failed:\n")
    print(dt$notes[is.na(AmountHUF), ])
    stop()
  }
  sapply(unique(dt$notes[, Currency]), function (ccy) {
    adjustmentCcy <- dt$notes[Note == adjustment & Currency == ccy]
    if (nrow(adjustmentCcy) != 1) {
      cat(paste0(nrow(adjustmentCcy), " adjustment row(s) with note '",
                 adjustment, "'found for ", ccy, " (1 expected)\n"))
      stop()
    }
  })
  dt$adjustmentHUF <- sum(dt$notes[Note == adjustment, AmountHUF])
  dt$adjustmentUSD <- sum(dt$notes[Note == adjustment, AmountUSD])
  dt$notesHUF <- sum(dt$notes[is.na(Note) | Note != adjustment, AmountHUF])
  dt$notesUSD <- sum(dt$notes[is.na(Note) | Note != adjustment, AmountUSD])
  if (verbose) {
    cat(paste0("Notes: ", dt$notesHUF, " HUF\t",
               "(", dt$notesUSD, " USD)\n",
               "Adjustment: ", dt$adjustmentHUF, " HUF\t",
               "(", dt$adjustmentUSD, " USD)\n"))
  }
  return(dt)
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
               round(catSumHUF, 2), " HUF\n"))
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
  dt <- cash.summary(dt, verbose = verbose)
  dt <- balance.summary(dt)
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
  dtBal[is.na(dtBal)] <- 0
  dt$all <- add.adjustment(dt, dtBal[Adjustment != 0])
  if (nrow(dtBal) > 0) {
    for (row in 1:nrow(dtBal)) {
      account <- dtBal[row, Account]
      month <- dtBal[row, Month]
      ccy <- dtBal[row, Currency]
      day <- ifelse(dtBal[row, Day] != 0, dtBal[row, Day], 31)
      finalDate <- ifelse(day == 31, paste0("month ", month),
                          paste0(month, "/", day))
      fx <- dt$fx[ccy]
      adjust <- dtBal[row, Adjustment]
      balance <- dtBal[row, Balance]
      initial <- round(dt$initBalance[Account == account, InitialHUF] / fx, 2)
      trans <- round(sum(dt$all[Account == account & (Month < month
                                                      | (Month == month & Day <= day)), AmountHUF])
                     / fx, 2)
      total <- initial + trans
      diff <- total - balance
      adjustNew <- round(adjust - diff, 2)
      if (abs(diff) > 0.01) {
        cat(paste0("WARNING: ", account, " transactions don't match with ",
                   "balance for ", finalDate, "!\n",
                   "A. Initial:\t\t", ccy, " ", initial, "\n",
                   "B. Transactions:\t", ccy, " ", trans, "\n",
                   "C. Current (A+B):\t", ccy, " ", total, "\n",
                   "D. Balance:\t\t", ccy, " ", balance, "\n",
                   "E. Difference (D-C):\t", ccy, " ", total - balance, "\n",
                   "HINT: to dismiss error set adjustment for month ", month,
                   " to ", ccy, " ", adjustNew, "\n"))
        if (strictMode) stop()
      } else if (verbose) {
        cat(paste0("PASS: ", account, " transactions match with ",
                   "balance for month ", month, " (adjustment: ", ccy, " ",
                   adjust, ")\n"))
      }
    }
  }
}

check <- function(dt, showAll = F, strictMode = T, verbose = F) {
  # Check transactions
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


