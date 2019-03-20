library(data.table)
library(readxl)
library(qdap)
library(xlsx)

add.adjustment <- function(dt) {
  # Add adjustment to transactions
  #
  # Args:
  #   dt: list of data.tables
  #   adjustmentData: data.table containing adjustment 
  #
  # Returns:
  #   dt: list of data.table with updated transactions
  dtBal <- dt$yearBalance[Adjustment != 0]
  dtBal$Year <- dt$year
  dt$report <- add.columns(dtBal, type = "adjust")
  dt <- finalize(dt)
  dt$all <- merge.dt(dt$all, dt$report, "adjust")
  return(dt)
}

add.category <- function(dt, verbose = F) {
  # Add category for transactions based on patterns
  #
  # Args:
  #   dt: data.table to check (must contain Detail column)
  #   patternData: data.table containing patterns 
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table
  for (row in 1:nrow(dt$report)) {
    if (is.na(dt$report[row, Category])) {
      huf <- dt$report[row, AmountHUF]
      detail  <- dt$report[row, Details]
      date  <- dt$report[row, Date]
      account  <- dt$report[row, Account]
      if (verbose) cat(paste0(date, ": ", huf, " HUF (" ,detail, ")\n"))
      dt <- get.category.pattern(dt, detail, verbose = verbose)
      if (!is.na(dt$catPtn)) {
        dt$report[row, Category := dt$catPtn]
        next
      }
    }
  }
  dt$report <- dt$report[,.(Category = unlist(Category)),
                         by = setdiff(names(dt$report), 'Category')]
  return(dt)
}

add.columns <- function(d, type = "normal", negate = FALSE) {
  # Add columns to data.table
  #
  # Args:
  #   d: data.table
  #   type: file type
  #   negate: negate values
  #
  # Returns:
  #   d: updated data.table
  if (type == "cash") {
    d[, Amount := as.numeric(sub(",", ".", Amount, fixed = TRUE))]
  } else if (type == "unicredit") {
    d[, Details := paste0(Partner, " | ", PartnerAccount, " | ",  Transaction)]
    d[, Amount := sapply(Amount, function(x) 
      as.numeric(mgsub(c(",", "\u00A0", " HUF"), c(".", "", ""), x))
    )]
  } else if (type == "egeszsegpenztar") {
    d[, Details := paste0(Partner, " | ", Type)]
    dtTransfer = d[Credit != 0]
    dtTransfer[, Details := 'Bank fee']
    dtTransfer[, Credit := CreditFinal - Credit]
    d <- merge.dt(d, dtTransfer)
    d[, Amount := Credit + Debit]
  } else if (type == "53_checking") {
    names(d) <- "Details"
    d[, Date := paste0(2019, ".", gsub("/", ".", substr(Details, 1, 5)))]
    d[, Amount := sapply(Details, function(x) {
      as.numeric(strsplit(x, " ")[[1]][2])
    })]
    if (negate) d$Amount <- d$Amount * -1
  } else if (type == "capital_one") {
    d[is.na(d)] <- 0
    d[, Amount := Credit - Debit]
    d[, Date := sapply(Date, function(x) {
      format(as.POSIXct(x, format = "%m/%d/%Y"), format = "%Y.%m.%d")
    })]
    d[, Category := NULL]
  } else if (type == "adjust") {
    d[, Amount := Adjustment]
    d[, Details := "Adjustment"]
    d[, Day := ifelse(is.na(Day), 1, Day)]
    d$Date <- sprintf("%4d.%02d.%02d", d$Year, d$Month, d$Day)
  }
  return(d)
}

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
  dt$notesHUF <- sum(dt$notes$AmountHUF)
  dt$notesUSD <- sum(dt$notes$AmountUSD)
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
  if (!is.na(catAct)) {
    catWrong <- catAct[sapply(catAct, function(x) is.na(match(x, catAll)))]
    if (length(catWrong) > 0) {
      cat(paste0("Unknown ", colName, " found\n"))
      print(catWrong)
      stop()
    }
  }
}

finalize <- function(dt) {
  # Finalize data.table
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
  if (!hasName(dt$report, "Category")) dt$report[, Category := character()]
  dt$report <- dt$report[, .(Account, Details, Date, Amount, Category,
                             Currency)]
  dt$report <- dt$report[substr(Date, 1, 4) == dt$year, ]
  dt$report[, Date := sapply(Date, function(x) {
    gsub("-", ".", substr(x, 1, 10))
    })]
  dt$report[, Month := sapply(Date, function(x) as.integer(substr(x, 6, 7)))]
  dt$report[, Day := sapply(Date, function(x) as.integer(substr(x, 9, 10)))]
  dt$report[, AmountHUF := Amount * dt$fx[Currency]]
  dt$report[, AmountUSD := round(AmountHUF / dt$fx["USD"], 2)]
  dt <- add.category(dt, verbose = F)
  check.column(dt$income, dt$report, "Category")
  check.column(dt$initBalance, dt$report, "Account")
  return(dt)
}

get.category.pattern <- function(dt, detail, verbose = F) {
  # Get category for transation from patterns
  #
  # Args:
  #   detail: transaction detail
  #   patternData: data.table containing patterns
  #   verbose: print additional information
  #
  # Returns:
  #   catPtn: category
  dt$catPtn <- NA
  patterns <- dt$patterns[, Pattern]
  matchResults <- sapply(patterns, grepl, gsub("\u00A0", " ", detail),
                         fixed = T)
  matchResults <- matchResults[matchResults == T]
  catPatternAll <- dt$patterns[Pattern %in% names(matchResults), Category]
  if (length(unique(catPatternAll)) > 1) {
    cat(paste0("\nMultiple match: '", names(matchResults), "' in\n", detail))
    stop()
  } else if (length(unique(catPatternAll)) == 1) {
    dt$catPtn <- unique(catPatternAll)
    dt$patterns[Pattern %in% names(matchResults), Match := Match + 1]
    if (verbose) cat(paste0("\t'", dt$catPtn, "' based on '",
                            names(matchResults), "'\n"))
  }
  return(dt)
}

get.report.type <- function(dt, file) {
  # Get report type
  #
  # Args:
  #   dt: list of data.tables
  #     $reportTypes: list of report types
  #   file: file name
  #   types: report types
  #
  # Returns:
  #   type: type of report
  types <- dt$reportTypes$Type
  for (i in 1:length(types)) {
    if (grepl(tolower(types[i]), tolower(file), fixed = TRUE)) {
      type <- types[i]
      return(type)
    }
  }
}

merge.dt <- function(dtAll, dtNew, name = "data.table", verbose = F) {
  # Merge data.tables
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

read.dt <- function(fn, sheet) {
  # Read data table from excel
  #
  # Args:
  #   fn: file name
  #   sheet: sheet name
  # Returns:
  #   dt: data.table
  dt <- as.data.table(read_excel(path = fn, sheet = sheet))
  return(dt)
}

read.file <- function(fn, folder = "", dec = ".", encoding = "UTF-8",
                      sep = "auto", skip = 1, na = NULL, verbose = F) {
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
  if (is.na(sep)) sep <- "auto"
  if (folder != "") fn <- paste0(folder, fn)
  if (file.exists(fn)) {
    fileType <- unlist(strsplit(fn, "[.]"))[2]
    if (fileType == "csv") {
      d <- fread(fn, dec = dec, encoding = encoding, sep = sep)
    } else if (fileType %in% c("xlsx", "xls")) {
      d <- as.data.table(read.xlsx(fn, 1, encoding = encoding,
                                   startRow = skip + 1))
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

read.input <- function(fn, verbose = F) {
  # Read input data
  #
  # Args:
  #   fn: file name
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  if (verbose) cat("Read input:", fn, "\n")
  dt <- list()
  dt$rules <- read.dt(fn, sheet = "rename_rules")
  dt$income <- read.dt(fn, sheet = "income_categories")
  dt$patterns <- read.dt(fn, sheet = "patterns")
  dt$patterns$Match <- 0
  dt$reportTypes <- read.dt(fn, sheet = "report_types")
  fxRates <- read.dt(fn, sheet = "fx_rates")
  dt$fx <- setNames(fxRates$FXRate, fxRates$Currency)
  dt$yearBalance <- read.dt(fn, sheet = "balance_monthly")
  dt$initBalance <- read.dt(fn, sheet = "balance_initial")
  dt$target <- read.dt(fn, sheet = "target_categories")
  dt$target[, TargetHUF := Target * dt$fx[Currency]]
  dt$targetHUF <- setNames(dt$target$TargetHUF, dt$target$Category)
  dt$notes <- read.dt(fn, sheet = "cash_inventory")
  return(dt)
}

read.report <- function(dt, file, type = "normal", verbose = F) {
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
  rep <- dt$reportTypes[Type == type]
  dt$report <- read.file(file, folder = dt$folderReports, skip = rep$Skip,
                 sep = rep$Separator, verbose = verbose)
  dt <- rename(dt, type)
  negate <- grepl("debit", tolower(file))
  dt$report <- add.columns(dt$report, type, negate)
  if (!is.na(rep$Currency)) dt$report$Currency <- rep$Currency
  if (!is.na(rep$Account)) dt$report$Account <- rep$Account
  dt <- finalize(dt)
  dt$all <- merge.dt(dt$all, dt$report, name = "dt$all", verbose = verbose)
  return(dt)
}

read.all <- function(fn, folder, year, verbose = F) {
  # Read reports from folder
  #
  # Args:
  #   fn: input file name
  #   folder: folder name for reports
  #   year: year name
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  dt <- read.input(fn = fn, verbose = verbose)
  dt$all <- data.table()
  dt$folderReports <- folder
  dt$year <- year
  for (fn in list.files(path = folder)) {
    type <- get.report.type(dt, fn)
    if (!is.null(type)) {
      if (verbose) cat("Read report:", fn, "\n")
      dt <- read.report(dt, fn, type, verbose = verbose)
    }
  }
  return(dt)
}

rename <- function(dt, type, verbose = F) {
  # Rename values in data.table
  #
  # Args:
  #   dt: list of data.tables
  #     $report: data to rename
  #     $rules: renaming rules
  #   type: filter for 'Type' column in rules data.table
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of updated data.tables
  # Rename columns
  rulesCol <- dt$rules[Type == type & Value == "Column"]
  if (nrow(rulesCol) > 0) {
    if (verbose) cat(nrow(rulesCol), "column(s) renamed")
    setnames(dt$report, rulesCol$From, rulesCol$To)
  }
  # Rename values
  rulesVal <- dt$rules[Type == type & Value != "Column"]
  if (nrow(rulesVal) > 0) {
    if (verbose) cat(nrow(rulesVal), "value(s) renamed")
    for (i in 1:nrow(rulesVal)) {
      dt$report <- dt$report[get(rulesVal[i, Value]) == rulesVal[i, From],
                             c(rulesVal[i, Value]) := rulesVal[i, To]]
    }
  }
  return(dt)
}

summarize.all <- function(dt) {
  # Summarize values in data.tables
  #
  # Args:
  #   dt: list of data.tables
  #
  # Returns:
  #   dt: list of updated data.tables
  dt <- add.adjustment(dt)
  dt <- balance.summary(dt)
  dt <- cash.summary(dt)
  return(dt)
}