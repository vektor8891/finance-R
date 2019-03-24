library(pivottabler)
library(lubridate)
library(openxlsx)

export.all <- function(dt, file, currency, verbose = F) {
  # Export results
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $income: categories
  #     $year: year name
  #   folder: output folder name
  #   currency: currency (HUF or USD)
  #   verbose: print additional information
  dt$all <- merge(dt$all, dt$income, by = "Category", all.x = T)
  dt$all <- dt$all[, Date2 := make_date(year = dt$year, month = Month, day = 1L)]
  dt$all <- dt$all[, Group2 := sapply(Group, function(x) strsplit(x, " - ")[[1]][1])]
  dt$all <- dt$all[, Category2 := sapply(Category, function(x) sub("^[A-Z]*-", "", x))]
  dt$all <- dt$all[, Average := "Monthly"]
  type <- "Category"
  pt <- create.pivot(dt, type = type, currency = currency, verbose = verbose)
  pt <- format.pivot(pt, type = type, verbose = verbose)
  export.pivot(pt, file, sheet = type, verbose = verbose)
}

create.pivot <- function(dt, type, currency, avgCalc = "AvgCalc",
                         sumCalc = "SumCalc", verbose = F) {
  # Create pivot table
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $year: year name
  #   type: pivot report type:
  #     - "category": pivot report for categories
  #   currency: currency (HUF or USD)
  #   avgCalc: average calculation name
  #   sumCalc: sum calculation name
  #   verbose: print additional information
  #
  # Returns:
  #   pt: pivot report object
  if (verbose) cat(paste0("Create '", type, "' pivot table\n"))
  pt <- PivotTable$new()
  pt$addData(dt$all)
  if (type == "Category") {
    cg1 <- pt$columnGroup$addChildGroup(caption = "Average")
    cg1$addDataGroups("Average", addTotal = FALSE)
    cg2 <- pt$columnGroup$addChildGroup(caption = paste0("Total ", currency))
    cg2$addDataGroups("Date2", dataFormat = list(format = "%b"),
                      totalCaption = as.character(dt$year))
    pt$addRowDataGroups("Type", totalCaption = "TOTAL")
    pt$addRowDataGroups("Group2")
    pt$addRowDataGroups("Category2", totalCaption = "Subtotal")
    pt$defineCalculation(calculationGroupName = "calcGrp1",
                         calculationName = avgCalc, format = "%.0f",
                         summariseExpression = paste0("sum(Amount", currency, ")/max(Month)"))
    pt$defineCalculation(calculationName = sumCalc, format = "%.0f",
                         summariseExpression = paste0("sum(Amount", currency, ")"))
    cg1$addCalculationGroups("calcGrp1")
    cg2$addCalculationGroups("default")
  } else {
    cat("Unknown type:", type)
    stop()
  }
  pt$evaluatePivot()
  return(pt)
}

format.pivot <- function(pt, type, clrAvg = "#feffba", clrGrp = "#baffbc",
                         clrTyp = "#ffbbba", clrDat = "#ffbafe",
                         xlFormat = "#,##0", avgCalc = "AvgCalc",
                         sumCalc = "SumCalc", verbose = F) {
  # Format pivot table
  #
  # Args:
  #   pt: pivot report object
  #   type: pivot report type:
  #     - "category": pivot report for categories
  #   clrAvg: background color for average column
  #   clrGrp: background color for category group total
  #   clrTyp: background color for category type total
  #   clrYr: background color for year summary
  #   avgCalc: average calculation name
  #   sumCalc: sum calculation name
  #   xlFormat: Excel format
  #   verbose: print additional information
  #
  # Returns:
  #   pt: pivot report object
  if (verbose) cat(paste0("Format '", type, "' pivot table\n"))
  # average values
  cells <- pt$findCells(calculationName = avgCalc, variableValues = list("Category2" = "!*"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrAvg, "xl-value-format" = xlFormat))
  cells <- pt$findCells(calculationName = avgCalc, variableValues = list("Category2" = "**", "Group2" = "!*"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrAvg, "font-weight" = "bold", "font-style" = "italic", "xl-value-format" = xlFormat))
  cells <- pt$findCells(calculationName = avgCalc, variableValues = list("Group2" = "**", "Type" = "!*"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrGrp, "font-weight" = "bold", "xl-value-format" = xlFormat))
  cells <- pt$findCells(calculationName = avgCalc, variableValues = list("Group2" = "**", "Type" = "**"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrTyp, "font-weight" = "bold", "xl-value-format" = xlFormat))
  # sums
  cells <- pt$findCells(calculationName = sumCalc, variableValues = list("Category2" = "!*", "Date2" = "!*"))
  pt$setStyling(cells = cells, declarations = list("xl-value-format" = xlFormat))
  cells <- pt$findCells(calculationName = sumCalc, variableValues = list("Category2" = "**", "Group2" = "!*", "Date2" = "!*"))
  pt$setStyling(cells = cells, declarations = list("font-weight" = "bold", "font-style" = "italic", "xl-value-format" = xlFormat))
  cells <- pt$findCells(calculationName = sumCalc, variableValues = list("Group2" = "**", "Type" = "!*", "Date2" = "!*"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrGrp, "font-weight" = "bold", "xl-value-format" = xlFormat))
  cells <- pt$findCells(calculationName = sumCalc, variableValues = list("Group2" = "**", "Type" = "**", "Date2" = "!*"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrTyp, "font-weight" = "bold", "xl-value-format" = xlFormat))
  # year sums
  cells <- pt$findCells(calculationName = sumCalc, variableValues = list("Category2" = "!*", "Date2" = "**"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrDat, "xl-value-format" = xlFormat))
  cells <- pt$findCells(calculationName = sumCalc, variableValues = list("Category2" = "**", "Group2" = "!*", "Date2" = "**"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrDat, "font-weight" = "bold", "font-style" = "italic", "xl-value-format" = xlFormat))
  cells <- pt$findCells(calculationName = sumCalc, variableValues = list("Group2" = "**", "Type" = "!*", "Date2" = "**"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrGrp, "font-weight" = "bold", "xl-value-format" = xlFormat))
  cells <- pt$findCells(calculationName = sumCalc, variableValues = list("Group2" = "**", "Type" = "**", "Date2" = "**"))
  pt$setStyling(cells = cells, declarations = list("background-color" = clrTyp, "font-weight" = "bold", "xl-value-format" = xlFormat))
  return(pt)
}

export.pivot <- function(pt, file, sheet, verbose = F) {
  # Export pivot table
  #
  # Args:
  #   pt: pivot report object
  #   file: output file name
  #   sheet: sheet name
  #   verbose: print additional information
  if (verbose) cat(paste0("Export pivot table\n"))
  wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
  addWorksheet(wb, sheet)
  pt$writeToExcelWorksheet(wb = wb, wsName = sheet, 
                           topRowNumber = 1, leftMostColumnNumber=1,
                           applyStyles = TRUE,
                           mapStylesFromCSS = TRUE)
  saveWorkbook(wb, file = file, overwrite = TRUE)
}