library(pivottabler)
library(data.table)
library(lubridate)
library(openxlsx)
library(qdap)

theme <- list(
  fontName = "Verdana, Arial",
  headerBackgroundColor = "rgb(68, 114, 196)",
  headerColor = "rgb(255, 255, 255)",
  cellBackgroundColor = "rgb(255, 255, 255)",
  cellColor = "rgb(0, 0, 0)",
  totalBackgroundColor = "rgb(186, 202, 233)",
  totalColor = "rgb(0, 0, 0)",
  borderColor = "rgb(48, 84, 150)",
  "xl-value-format" = "#,##0" # TODO: fix format for Excel output
)

cumulativeFilter <- function(pt, filters, cell) {
  # get the date filter
  filter <- filters$getFilter("Date")
  # get the date value and modify the filter
  if (is.null(filter) || (filter$type == "ALL") || (length(filter$values) > 1)) {
    # do nothing
  } else {
    date <- filter$values
    newDates <- seq(as.Date(paste0(dt$year, "-01-01")), date, by = "days")
    filter$values <- newDates
  }
}

export.all <- function(dt, folder, currency, addTimeStamp = F, verbose = F) {
  # Export results
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $income: categories
  #     $year: year name
  #   folder: output folder name
  #   ccy: currency (HUF or USD)
  #   addTimeStamp: add time stamp to output file name
  #   verbose: print additional information
  dt <- modify.dt(dt, currency, folder)
  fn <- get.fileName(dt, addTimeStamp = addTimeStamp)
  wb <- openxlsx::createWorkbook(creator = Sys.getenv("USERNAME"))
  
  wb <- pivot.income(dt, wb, showRatio = T, verbose = verbose)
  wb <- pivot.income(dt, wb, showCategory = T, showPnL = T, verbose = verbose)
  wb <- pivot.balance(dt, wb, verbose = verbose)
  wb <- export.dt(dt$transactions, wb, "Transactions", verbose = verbose)
  
  openxlsx::saveWorkbook(wb, file = fn, overwrite = T)
}

export.dt <- function(dt, wb, sheetName, verbose = F) {
  # Export results
  #
  # Args:
  #   dt: data.table to export
  #   wb: Excel workbook object
  #   sheetName: sheet name
  #   verbose: print additional information
  #
  # Returns:
  #   wb: Excel workbook object
  if (verbose) cat(paste0("Export '", sheetName, "' to Excel\n"))
  openxlsx::addWorksheet(wb, sheetName)
  openxlsx::writeData(wb, sheet = sheetName, dt)
  openxlsx::setColWidths(wb, sheet = sheetName, widths = "auto",
                         cols = 1:length(colnames(dt)))
  return(wb)
}

pivot.balance <- function(dt, wb, cumulative = T ,verbose = F) {
  # Create pivot table for balance sheet
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $year: year name
  #     $ccy: currency (HUF or USD)
  #   wb: Excel workbook object
  #   cumulative: use cumulative sum
  #   verbose: print additional information
  #
  # Returns:
  #   wb: Excel workbook object
  pt <- PivotTable$new()
  pt$theme <- theme
  dt$all$Date <- as.Date(dt$all$Date)
  pt$addData(dt$all)
  pt$addColumnDataGroups("Date", dataFormat = list(format = "%b"),
                         totalCaption = as.character(dt$year))
  pt$addRowDataGroups("AccountType", totalCaption = "TOTAL",
                      styleDeclarations = list("xl-min-column-width" = "13"))
  pt$addRowDataGroups("AccountGroup",
                      styleDeclarations = list("xl-min-column-width" = "14"))
  pt$addRowDataGroups("Account", addTotal = FALSE,
                      styleDeclarations = list("xl-min-column-width" = "18"))
  filterOverrides <- PivotFilterOverrides$new(pt, overrideFunction =
                                                cumulativeFilter)
  pt$defineCalculation(calculationName = "MonthlyCumulativeSum", format = "%.0f",
                       caption = dt$ccy, filters = filterOverrides, 
                       summariseExpression = paste0("sum(Amount", dt$ccy, ")"))
  pt$evaluatePivot()
  cells <- pt$findCells(calculationNames = c("MonthlyCumulativeSum", "MonthlySum"))
  pt$setStyling(cells = cells, declarations = list("xl-value-format" = "#,##0"))
  sheetName <- paste0("Balance", dt$ccy)
  wb <- export.pt(wb, pt = pt, sheet = sheetName, verbose = verbose)
  return(wb)
}


pivot.income <- function(dt, wb, showCategory = F, showPnL = F, showRatio = F,
                         verbose = F) {
  # Create pivot table for income statement
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $year: year name
  #     $ccy: currency (HUF or USD)
  #   wb: Excel workbook object
  #   showCategory: show category groups in pivot
  #   showPnL: show total PnL
  #   showRatio: show absolute ratio compared to total income
  #   verbose: print additional information
  #
  # Returns:
  #   wb: Excel workbook object
  pt <- PivotTable$new()
  pt$theme <- theme
  pt$addData(dt$all[CategoryType != "OTHER"])
  pt$addColumnDataGroups("Date", dataFormat = list(format = "%b"),
                    totalCaption = as.character(dt$year))
  if (showPnL) {
    pt$addRowDataGroups("CategoryType", totalCaption = "TOTAL")
  } else {
    pt$addRowDataGroups("CategoryType", addTotal = F)
  }
  pt$addRowDataGroups("CategoryGroup",
                      styleDeclarations = list("xl-min-column-width" = "15"))
  if (showCategory) {
    pt$addRowDataGroups("Category", addTotal = FALSE,
                        styleDeclarations = list("xl-min-column-width" = "17"))
  }
  pt$defineCalculation(calculationName = "MonthlySum", caption = dt$ccy, format = "%.0f",
                       summariseExpression = paste0("sum(Amount", dt$ccy, ")"))
  if (showRatio) {
    incomeFilter <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor = "Date")
    incomeFilter$add(variableName = "CategoryType", values = "INCOME", action = "replace")
    pt$defineCalculation(calculationName = "TotalIncome",
                         summariseExpression = paste0("sum(Amount", dt$ccy, ")"),
                         filters = incomeFilter, visible = F)
    pt$defineCalculation(calculationName = "Ratio", caption = "%", type = "calculation",
                         basedOn = c("TotalIncome", "MonthlySum"), format = "%.0f %%",
                         calculationExpression = "abs(values$MonthlySum/values$TotalIncome * 100)")
  }
  pt$evaluatePivot()
  if (showRatio) {
    cells <- pt$findCells(calculationNames = "Ratio")
    pt$setStyling(cells = cells, declarations = list("font-style" = "italic"))
  }
  cells <- pt$findCells(calculationNames = "MonthlySum")
  pt$setStyling(cells = cells, declarations = list("xl-value-format" = "#,##0"))
  sheetName <- ifelse(showCategory, paste0("Category", dt$ccy),
                      paste0("CategoryGroup", dt$ccy))
  wb <- export.pt(wb, pt = pt, sheet = sheetName, verbose = verbose)
  return(wb)
}

export.pt <- function(wb, pt, sheet, verbose = F) {
  # Export pivot table
  #
  # Args:
  #   wb: Excel workbook object
  #   pt: pivot report object
  #   sheet: sheet name
  #   verbose: print additional information
  #
  # Returns:
  #   wb: Excel workbook
  if (verbose) cat(paste0("Export '", sheet, "' to Excel\n"))
  addWorksheet(wb, sheet)
  pt$writeToExcelWorksheet(wb = wb, wsName = sheet, 
                           topRowNumber = 1, leftMostColumnNumber = 1,
                           outputValuesAs = "formattedValueAsNumber",
                           applyStyles = T, mapStylesFromCSS = T)
  return(wb)
}

get.fileName <- function(dt, addTimeStamp = F) {
  # Get file name
  #
  # Args:
  #   dt: list of data tables
  #     $folder: folder name
  #     $year: year
  #     $ccy: currency
  #   addTimeStamp: add time stamp to output file name
  #
  # Returns:
  #   fn: file name
  if (addTimeStamp) {
    time <- mgsub(c(" ",":"), c("_","."), as.POSIXlt(Sys.time()))
    fn <- paste0(dt$folder, "output_", dt$year,"_", dt$ccy, "_", time, ".xlsx")
  } else {
    fn <- paste0(dt$folder, "output.xlsx")
  }
  return(fn)
}

modify.dt <- function(dt, ccy, folder) {
  # Modify transactions
  #
  # Args:
  #   dt: list of data.tables
  #     $all: transactions
  #     $income: income categories
  #     $year: year
  #   ccy: currency
  #   folder: output folder name
  #
  # Returns:
  #   dt: list of data.tables
  #     $all: all transactions with adjustment
  #     $calc: calculation names
  # save transactions
  setcolorder(dt$all, c(colnames(dt$all)[-2], colnames(dt$all)[2]))
  dt$transactions <- dt$all
  # add extra rows
  dt <- add.initial.balance(dt)
  dt <- add.duplicated(dt)
  # merge transactions with category & account types
  dt$all <- merge(dt$all, dt$income, by = "Category", all.x = T)
  dt$all <- merge(dt$all, dt$initBalance, by = "Account", all.x = T)
  # rename columns
  dt$all$DateOld <- dt$all$Date
  dt$all$CategoryGroupOld <- dt$all$CategoryGroup
  dt$all$CategoryOld <- dt$all$Category
  dt$all <- dt$all[, Date := make_date(year = dt$year, month = Month, day = 1L)]
  dt$all <- dt$all[, CategoryGroup := sapply(CategoryGroup, function(x) {
    strsplit(x, " - ")[[1]][1]
  })]
  dt$all <- dt$all[, Category := sapply(Category, function(x) {
    sub("^[A-Z]*-", "", x)
  })]
  # add new parameters
  dt$ccy <- ccy
  dt$folder <- folder
  return(dt)
}

add.initial.balance <- function(dt) {
  # Add initial balance to transactions
  #
  # Args:
  #   dt: list of data.tables
  #     $all: transactions
  #     $initBalance: initial balance
  #     $year: year
  #
  # Returns:
  #   dt: list of data.tables
  #     $all: all transactions with initial balance
  d <- dt$initBalance
  d[, c("Amount", "AmountHUF", "AmountUSD", "Date", "Currency", "Category",
        "Details") := list(InitialHUF, InitialHUF, InitialUSD,
                           paste0(dt$year, ".01.01"), "HUF", "Initial balance",
                           "Initial balance")]
  list[d, ] <- finalize(dt, d)
  dt$all <- merge.dt(dt$all, d, name = "Initial balance")
  return(dt)
}

add.duplicated <- function(dt) {
  # Duplicate transactions where category is separate account
  #
  # Args:
  #   dt: list of data.tables
  #     $all: transactions
  #     $year: year
  #
  # Returns:
  #   dt: list of data.tables
  #     $all: all transactions with initial balance
  d <- dt$all[Category %in% dt$initBalance$Account]
  d[, c("Account", "Amount", "AmountHUF", "AmountUSD", "Category", "Details")
    := list(Category, -Amount, -AmountHUF, -AmountUSD, "Duplicated",
            "Duplicated (category is separate account)")]
  list[d, ] <- finalize(dt, d)
  dt$all <- merge.dt(dt$all, d, name = "Duplicated")
  return(dt)
}