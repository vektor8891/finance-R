library(pivottabler)
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
  borderColor = "rgb(48, 84, 150)"
)

export.all <- function(dt, folder, ccy, addTimeStamp = F, verbose = F) {
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
  #   verbose: print additional information7
  dt <- modify.dt(dt, ccy, folder)
  fn <- get.fileName(dt, addTimeStamp = addTimeStamp)
  wb <- openxlsx::createWorkbook(creator = Sys.getenv("USERNAME"))
  pt <- pivot.income(dt, showRatio = T)
  wb <- export.pt(wb, pt = pt, sheet = paste0("Grp_", ccy), verbose = verbose)
  pt <- pivot.income(dt, showCategory = T, showPnL = T)
  wb <- export.pt(wb, pt = pt, sheet = paste0("Cat_", ccy), verbose = verbose)
  openxlsx::addWorksheet(wb, "Transactions")
  openxlsx::writeData(wb, sheet = "Transactions", dt$transactions)  
  openxlsx::saveWorkbook(wb, file = fn, overwrite = T)
}

pivot.income <- function(dt, showCategory = F, showPnL = F, showRatio = F) {
  # Create pivot table for income statement
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $year: year name
  #     $ccy: currency (HUF or USD)
  #   showCategory: show category groups in pivot
  #   showPnL: show total PnL
  #   showRatio: show absolute ratio compared to total income
  #
  # Returns:
  #   pt: pivot report object
  pt <- PivotTable$new()
  pt$addData(dt$all)
  pt$addColumnDataGroups("Date", dataFormat = list(format = "%b"),
                    totalCaption = as.character(dt$year))
  if (showPnL) {
    pt$addRowDataGroups("Type", totalCaption = "TOTAL")
  } else {
    pt$addRowDataGroups("Type", addTotal = F)
  }
  pt$addRowDataGroups("Group",
                      styleDeclarations = list("xl-min-column-width" = "15"))
  if (showCategory) {
    pt$addRowDataGroups("Category", addTotal = FALSE,
                        styleDeclarations = list("xl-min-column-width" = "17"))
  }
  pt$defineCalculation(calculationName = "MonthlySum", caption = dt$ccy, format = "%.0f",
                       summariseExpression = paste0("abs(sum(Amount", dt$ccy, "))"))
  if (showRatio) {
    incomeFilter <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor = "Date")
    incomeFilter$add(variableName = "Type", values = "INCOME", action = "replace")
    pt$defineCalculation(calculationName = "TotalIncome",
                         summariseExpression = paste0("sum(Amount", dt$ccy, ")"),
                         filters = incomeFilter, visible = F)
    pt$defineCalculation(calculationName = "Ratio", caption = "%", type = "calculation",
                         basedOn = c("TotalIncome", "MonthlySum"), format = "%.0f",
                         calculationExpression = "abs(values$MonthlySum/values$TotalIncome * 100)")
  }
  pt$evaluatePivot()
  return(pt)
}

export.pt <- function(wb, pt, sheet, verbose = F) {
  # Export pivot table
  #
  # Args:
  #   wb: Excel workbook object
  #   pt: pivot report object
  #   type: pivot report type:
  #   ccy: currency
  #   verbose: print additional information
  #
  # Returns:
  #   wb: Excel workbook
  if (verbose) cat(paste0("Export '", sheet, "' to Excel\n"))
  addWorksheet(wb, sheet)
  # if (style) browser()
  pt$theme <- theme
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
  #     $all: year name
  #     $income: income categories
  #     $year: year
  #   ccy: currency
  #   folder: output folder name
  #
  # Returns:
  #   dt: list of data.tables
  #     $all: all transactions with adjustment
  #     $calc: calculation names
  dt$transactions <- dt$all
  dt$all <- merge(dt$all, dt$income, by = "Category", all.x = T)
  dt$all$DateOld <- dt$all$Date
  dt$all$GroupOld <- dt$all$Group
  dt$all$CategoryOld <- dt$all$Category
  dt$all <- dt$all[, Date := make_date(year = dt$year, month = Month, day = 1L)]
  dt$all <- dt$all[, Group := sapply(Group, function(x) {
    strsplit(x, " - ")[[1]][1]
  })]
  dt$all <- dt$all[, Category := sapply(Category, function(x) {
    sub("^[A-Z]*-", "", x)
  })]
  dt$all$Average <- "Monthly"
  dt$calc <- c("Average" = "AvgCalc", "Sum" = "SumCalc")
  dt$ccy <- ccy
  dt$folder <- folder
  return(dt)
}


load("finance.RData")
dt$all <- dt$all
export.all(dt, folder = "output/", ccy = "USD", addTimeStamp = T, verbose = T)