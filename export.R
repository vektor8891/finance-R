library(pivottabler)
library(lubridate)
library(openxlsx)

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
  #   verbose: print additional information
  dt <- modify.dt(dt, ccy, folder)
  fn <- get.fileName(dt, addTimeStamp = addTimeStamp)
  dt$wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
  type <- "Group"
  pt <- create.pivot(dt, type, verbose = verbose)
  pt <- format.pivot(dt, pt, type, verbose = verbose)
  dt$wb <- export.pivot(dt, pt = pt, type = type, ccy = ccy, verbose = verbose)
  type <- "Category"
  pt <- create.pivot(dt, type, verbose = verbose)
  pt <- format.pivot(dt, pt, type, verbose = verbose)
  dt$wb <- export.pivot(dt, pt = pt, type = type, ccy = ccy, verbose = verbose)
  saveWorkbook(dt$wb, file = fn, overwrite = T)
}

create.pivot <- function(dt, type, verbose = F) {
  # Create pivot table
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $year: year name
  #     $calc: calculation names
  #     $ccy: currency (HUF or USD)
  #   type: pivot report type
  #   verbose: print additional information
  #
  # Returns:
  #   pt: pivot report object
  if (verbose) cat(paste0("Create '", type, "' pivot table\n"))
  pt <- PivotTable$new()
  pt$addData(dt$all)
  if (type == "Category" || type == "Group") {
    cg1 <- pt$columnGroup$addChildGroup(caption = "Average")
    cg1$addDataGroups("Average", addTotal = FALSE)
    cg2 <- pt$columnGroup$addChildGroup(caption = paste0("Total ", dt$ccy))
    cg2$addDataGroups("Date", dataFormat = list(format = "%b"),
                      totalCaption = as.character(dt$year))
    pt$addRowDataGroups("Type", totalCaption = paste0("TOTAL ", dt$ccy))
    pt$addRowDataGroups("Group")
    if (type == "Category") {
      pt$addRowDataGroups("Category", totalCaption = "Subtotal")
    }
    pt$defineCalculation(calculationGroupName = "calcGrp1",
                         calculationName = unname(dt$calc["Average"]),
                         format = "%.0f",
                         summariseExpression = paste0("sum(Amount", dt$ccy,
                                                      ")/max(Month)"))
    pt$defineCalculation(calculationName = unname(dt$calc["Sum"]),
                         format = "%.0f",
                         summariseExpression = paste0("sum(Amount", dt$ccy, ")"))
    cg1$addCalculationGroups("calcGrp1")
    cg2$addCalculationGroups("default")
  } else {
    cat("Unknown type:", type)
    stop()
  }
  pt$evaluatePivot()
  return(pt)
}

format.pivot <- function(dt, pt, type, verbose = F) {
  # Format pivot table
  #
  # Args:
  #   dt: list of data.tables
  #     $format: formatting parameters
  #     $calc: calculation names
  #   pt: pivot report object
  #   type: pivot report type
  #   verbose: print additional information
  #
  # Returns:
  #   pt: pivot report object
  if (verbose) cat(paste0("Format '", type, "' pivot table\n"))
  dt$format <- dt$format[, `background-color` :=
                           sapply(`background-color`, function(x) {
                             dt$colors[x]})]
  df <- dt$format[PivotType == type]
  if (nrow(df) == 0) {
    cat("Unknown type:", type)
    stop()
  }
  for (i in 1:nrow(df)) {
    calcName <- dt$calc[df[i, Calculation]]
    
    row <- df[i, colSums(is.na(df[i])) == 0, with = FALSE]
    values <- row[, colnames(row) %in% c("Group",
                                     "Category",
                                     "Type",
                                     "Date"), with = FALSE]
    valList <- apply(values, 1, as.list)[[1]]
    format <- row[, colnames(row) %in% c("background-color",
                                     "font-weight",
                                     "font-style",
                                     "xl-value-format"), with = FALSE]
    formatList <- apply(format, 1, as.list)[[1]]
    cells <- pt$findCells(calculationNames = calcName, variableValues = valList)
    pt$setStyling(cells = cells, declarations = formatList)
  }
  return(pt)
}

export.pivot <- function(dt, pt, type, ccy, verbose = F) {
  # Export pivot table
  #
  # Args:
  #   dt: list of data tables
  #     $wb: Excel workbook object
  #   pt: pivot report object
  #   type: pivot report type:
  #   ccy: currency
  #   verbose: print additional information
  #
  # Returns:
  #   wb: Excel workbook
  if (verbose) cat(paste0("Export '", type, "' pivot table\n"))
  sheet <- paste0(type, ccy)
  addWorksheet(dt$wb, sheet)
  pt$writeToExcelWorksheet(wb = dt$wb, wsName = sheet, 
                           topRowNumber = 1, leftMostColumnNumber = 1,
                           applyStyles = TRUE,
                           mapStylesFromCSS = TRUE)
  return(dt$wb)
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


load("finance.Rdata")
dt$all <- dt$all[1:100]
dt$format <- read.dt("input.xlsx", sheet = "format")
col <- read.dt("input.xlsx", sheet = "colors")
dt$colors <- setNames(col$Hex, col$Name)
export.all(dt, folder = "output/", ccy = "USD", addTimeStamp = T, verbose = T)