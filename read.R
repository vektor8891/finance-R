library(data.table)
library(readxl)
library(qdap)
library(xlsx)
library(gsubfn)

add.category <- function(patterns, d, verbose = F) {
  # Add category for transactions based on patterns
  #
  # Args:
  #   patterns: data.table for patterns
  #   d: data.table to check
  #   verbose: print additional information
  #
  # Returns:
  #   d: data.table with category
  #   patterns: patterns data with updated match results
  for (row in 1:nrow(d)) {
    if (is.na(d[row, Category])) {
      huf <- d[row, AmountHUF]
      detail  <- d[row, Details]
      date  <- d[row, Date]
      account  <- d[row, Account]
      if (verbose) cat(paste0(date, ": ", huf, " HUF (" ,detail, ")\n"))
      list[categ, pattern] <- get.pattern(patterns, detail, verbose = verbose)
      if (!is.null(categ)) {
        d[row, Category := categ]
        patterns[Pattern %in% pattern, Match := Match + 1]
        next
      }
    }
  }
  d <- d[, .(Category = unlist(Category)), by = setdiff(names(d), 'Category')]
  return(list(d, patterns))
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
  } else if (grepl("53", type)) {
    names(d) <- "Details"
    d[, Date := paste0(2019, ".", gsub("/", ".", substr(Details, 1, 5)))]
    d[, Amount := sapply(Details, function(x) {
      as.numeric(gsub(",", "", strsplit(x, " ")[[1]][2]))
    })]
    if (negate) d$Amount <- d$Amount * -1
  } else if (type == "capital_one") {
    d[is.na(d)] <- 0
    d[, Amount := Credit - Debit]
    d[, Date := sapply(Date, function(x) {
      format(as.POSIXct(x, format = "%m/%d/%Y"), format = "%Y.%m.%d")
    })]
    d[, Category := NULL]
  } else if (grepl("HSBC", type)) {
    # browser()
    d[, Date := sapply(DateRaw, function(x) {
      format(as.POSIXct(x, format = "%m/%d/%Y"), format = "%Y.%m.%d")
    })]
    d[, Amount := as.numeric(sub(",", "", AmountRaw))]
    if (type == "HSBC_mastercard") {
      d$Amount <- -d$Amount
    }
  } else if (type == "adjust") {
    d[, Amount := Adjustment]
    d[, Details := "Adjustment"]
    d[, Day := ifelse(is.na(Day), 1, Day)]
    d$Date <- sprintf("%4d.%02d.%02d", d$Year, d$Month, d$Day)
  }
  return(d)
}

check.column <- function(dataAll, dataCurrent, colName) {
  # Check for unrecognizeable value in column
  #
  # Args:
  #   dataAll: data.table containing all correct values
  #   dataCurrent: data.table containing current values
  #   colName: column name to check
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

finalize <- function(dt, d) {
  # Finalize data.table
  #
  # - Remove extra columns
  # - Filter data for given year
  # - Add Month/Day/AmountHUF/AmountUSD columns
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
  #
  # Returns:
  #   d: updated data.table
  #   patterns: updated data.table for patterns
  if (!hasName(d, "Category")) d[, Category := character()]
  d <- d[, .(Account, Details, Date, Amount, Category, Currency)]
  d <- d[substr(Date, 1, 4) == dt$year, ]
  d[, Date := sapply(Date, function(x) {
    gsub("-", ".", substr(x, 1, 10))
    })]
  d[, Month := sapply(Date, function(x) as.integer(substr(x, 6, 7)))]
  d[, Day := sapply(Date, function(x) as.integer(substr(x, 9, 10)))]
  d[, AmountHUF := Amount * dt$fx[Currency]]
  d[, AmountUSD := round(AmountHUF / dt$fx["USD"], 2)]
  list[d, patterns] <- add.category(dt$patterns, d, verbose = F)
  check.column(dt$income, d, "Category")
  check.column(dt$initBalance, d, "Account")
  return(list(d, patterns))
}

get.pattern <- function(patternData, detail, verbose = F) {
  # Get category for transation from patterns
  #
  # Args:
  #   patternData: data.table containing patterns
  #   detail: transaction detail
  #   verbose: print additional information
  #
  # Returns:
  #   category: category name
  #   pattern: pattern name (can be multiple)
  pattern <- NULL
  category <- NULL
  patterns <- patternData[, Pattern]
  matchResults <- sapply(patterns, grepl, gsub("\u00A0", " ", detail),
                         fixed = T)
  matchResults <- matchResults[matchResults == T]
  pattern <- names(matchResults)
  catPatternAll <- patternData[Pattern %in% pattern, Category]
  category <- unique(catPatternAll)
  if (length(category) > 1) {
    cat(paste0("\nMultiple match: '", names(matchResults), "' in\n", detail))
    stop()
  } else if (length(category) == 1) {
    if (verbose) cat(category, "based on", pattern, "\n")
  }
  return(c(category, pattern))
}

get.report.type <- function(reports, file) {
  # Get report type
  #
  # Args:
  #   reports: data.table of report types
  #   file: file name
  #   types: report types
  #
  # Returns:
  #   type: type of report
  types <- reports$Type
  for (i in 1:length(types)) {
    if (grepl(tolower(types[i]), tolower(file), fixed = TRUE)) {
      type <- types[i]
      return(type)
    }
  }
}

merge.dt <- function(dtAll, dtNew, name = "data.table", by = NULL,
                     verbose = F) {
  # Merge data.tables
  #
  # Args:
  #   dtAll: data.table with all data
  #   dtNew: data.table with new data
  #   name: name of data.table
  #   by: vector of shared column names to merge on
  #   verbose: print additional information
  #
  # Returns:
  #   dt: data.table
  if (nrow(dtAll) == 0) dtAll <- dtNew[FALSE]
  dt <- merge(dtAll, dtNew, by = by, all = T)
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
                      sep = "auto", header = T, verbose = F) {
  # Read data from file
  #
  # Args:
  #   fn: path to file
  #   dec: decimal places
  #   encoding: file encoding
  #   sep: separator between columns
  #   header: if FALSE then no header
  #   verbose: print additional information
  #
  # Returns:
  #   d: data.table
  if (is.na(sep)) sep <- "auto"
  if (folder != "") fn <- paste0(folder, fn)
  if (file.exists(fn)) {
    fileType <- unlist(strsplit(fn, "[.]"))[2]
    if (fileType == "csv") {
      hdr <- ifelse(is.null(header), "auto", as.logical(header))
      d <- fread(fn, dec = dec, encoding = encoding, sep = sep, header = hdr)
    } else if (fileType %in% c("xlsx", "xls")) {
      d <- as.data.table(xlsx::read.xlsx(fn, 1, encoding = encoding))
    } else {
      cat("Unknown filetype:", fileType, "in", fn, "\n")
      stop()
    }
    if (verbose) cat(dim(dt)[[1]], "rows imported from:", fn, "\n")
    return(d)
  } else {
    cat("Error: file not found:", fn, "\n")
    stop
  }
}

read.input <- function(dt, fn, verbose = F) {
  # Read input data
  #
  # Args:
  #   dt: (empty) list of data.tables
  #   fn: file name
  #   verbose: print additional information
  #
  # Returns:
  #   dt: list of data.tables
  if (verbose) cat("Read input:", fn, "\n")
  dt$rules <- read.dt(fn, sheet = "rename_rules")
  dt$income <- read.dt(fn, sheet = "income_categories")
  dt$patterns <- read.dt(fn, sheet = "patterns")
  dt$patterns$Match <- 0
  dt$reportTypes <- read.dt(fn, sheet = "report_types")
  fxRates <- read.dt(fn, sheet = "fx_rates")
  dt$fx <- setNames(fxRates$FXRate, fxRates$Currency)
  dt$monthlyBalance <- read.dt(fn, sheet = "balance_monthly")
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
  #     $all: all transactions
  #     $patterns: patterns with updated math results
  print(file)
  rep <- dt$reportTypes[Type == type]
  d <- read.file(file, folder = dt$folderReports, sep = rep$Separator,
                 header = rep$Header, verbose = verbose)
  d <- rename.dt(d, type, dt$rules, verbose = verbose)
  negate <- grepl("debit", tolower(file))
  d <- add.columns(d, type, negate)
  if (!is.na(rep$Currency)) d$Currency <- rep$Currency
  if (!is.na(rep$Account)) d$Account <- rep$Account
  list[d, dt$patterns] <- finalize(dt, d)
  dt$all <- merge.dt(dt$all, d, name = "dt$all", verbose = verbose)
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
  dt <- list()
  dt <- read.input(dt, fn = fn, verbose = verbose)
  dt$all <- data.table()
  dt$folderReports <- folder
  dt$year <- year
  for (fn in list.files(path = folder)) {
    type <- get.report.type(dt$reportTypes, fn)
    if (!is.null(type)) {
      if (verbose) cat("Read report:", fn, "\n")
      dt <- read.report(dt, fn, type, verbose = verbose)
    }
  }
  return(dt)
}

rename.dt <- function(d, type, rules, verbose = F) {
  # Rename values in data.table
  #
  # Args:
  #   d: data.table
  #   rules: data.table containing renaming rules
  #   type: filter for 'Type' column in rules data.table
  #   verbose: print additional information
  #
  # Returns:
  #   d: renamed data.table
  rulesCol <- rules[Type == type & Value == "Column"]
  if (nrow(rulesCol) > 0) {
    if (verbose) cat(nrow(rulesCol), "column(s) renamed")
    setnames(d, rulesCol$From, rulesCol$To)
  }
  rulesVal <- rules[Type == type & Value != "Column"]
  if (nrow(rulesVal) > 0) {
    if (verbose) cat(nrow(rulesVal), "value(s) renamed")
    for (i in 1:nrow(rulesVal)) {
      d <- d[get(rulesVal[i, Value]) == rulesVal[i, From],
             c(rulesVal[i, Value]) := rulesVal[i, To]]
    }
  }
  return(d)
}