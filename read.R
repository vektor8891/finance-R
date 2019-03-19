library(xlsx)
library(readxl)

add.columns <- function(dt, type = "normal", negate = FALSE) {
  # Add columns to data.table
  #
  # Args:
  #   dt: data.table
  #   type: file type
  #   negate: negate values
  #
  # Returns:
  #   dt: list of data.tables
  if (type == "cash") {
    dt[, Amount := as.numeric(sub(",", ".", Amount, fixed = TRUE))]
  } else if (type == "unicredit") {
    dt[, Details := paste0(Partner, " | ", PartnerAccount, " | ",  Transaction)]
    dt[, Amount := sapply(Amount, function(x) 
      as.numeric(mgsub(c(",", "\u00A0", " HUF"), c(".", "", ""), x))
    )]
  } else if (type == "egeszsegpenztar") {
    dt[, Details := paste0(Partner, " | ", Type)]
    dtTransfer = dt[Credit != 0]
    dtTransfer[, Details := 'Bank fee']
    dtTransfer[, Credit := CreditFinal - Credit]
    dt <- add.data(dt, dtTransfer)
    dt[, Amount := Credit + Debit]
  } else if (type == "53_checking") {
    names(dt) <- "Details"
    dt[, Date := paste0(2019, ".", gsub("/", ".", substr(Details, 1, 5)))]
    dt[, Amount := sapply(Details, function(x) {
      as.numeric(strsplit(x, " ")[[1]][2])
    })]
    if (negate) dt$Amount <- dt$Amount * -1
  } else if (type == "capital_one") {
    dt[is.na(dt)] <- 0
    dt[, Amount := Credit - Debit]
    dt[, Date := sapply(Date, function(x) {
      format(as.POSIXct(x, format = "%m/%d/%Y"), format = "%Y.%m.%d")
    })]
    dt[, Category := NULL]
  } else if (type == "adjust") {
    dt[, Amount := Adjustment]
    dt[, Details := "Adjustment"]
    dt[, Day := ifelse(Day == 0, 1, Day)]
    dt$Date <- sprintf("%4d.%02d.%02d", dt$Year, dt$Month, dt$Day)
  }
  return(dt)
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

finalize.dt <- function(dt, d) {
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
  # browser()
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
  if (verbose) cat("Read input:", fn)
  dt <- list()
  dt$rules <- read.dt(fn, sheet = "rename_rules")
  dt$income <- read.dt(fn, sheet = "income_categories")
  dt$patterns <- read.dt(fn, sheet = "patterns")
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
  report <- dt$reportTypes[Type == type]
  d <- read.file(file, folder = dt$folderReports, skip = report$Skip,
                 sep = report$Separator, verbose = verbose)
  d <- rename.dt(d, dt$rules, type)
  negate <- grepl("debit", tolower(file))
  d <- add.columns(d, type, negate)
  if (!is.na(report$Currency)) d$Currency <- report$Currency
  if (!is.na(report$Account)) d$Account <- report$Account
  d <- finalize.dt(dt, d)
  dt$all <- merge.dt(dt$all, d, name = "dt$all", verbose = verbose)
  return(dt)
}

read <- function(fn, folder, year, verbose = F) {
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
      if (verbose) cat("Read report:", fn)
      dt <- read.report(dt, fn, type, verbose = verbose)
    }
  }
  return(dt)
}

rename.dt <- function(d, rulesAll, type, column = F, verbose = F) {
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
