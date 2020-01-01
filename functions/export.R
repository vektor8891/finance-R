library(pivottabler)
library(data.table)
library(lubridate)
library(openxlsx)
library(qdap) # mgsub()
library(ggplot2)
library(RColorBrewer)
library(colorRamps)

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
  
  fig.costs(dt, verbose = verbose)
  fig.balance(dt, verbose = verbose)
  fig.cumulative.balance(dt, account = "HSBC", verbose = verbose)
  fig.cumulative.balance(dt, account = "HSBC", month = 12, verbose = verbose)
  
  ptGrp <- pivot.income(dt, showRatio = T, showPnL = T, verbose = verbose)
  ptCat <- pivot.income(dt, showCategory = T, showPnL = T, verbose = verbose)
  ptBal <- pivot.balance(dt, verbose = verbose)

  wb <- export.pt(wb, pt = ptGrp, sheet = "CategoryGroup", verbose = verbose)
  wb <- export.pt(wb, pt = ptCat, sheet = "Category", verbose = verbose)
  wb <- export.pt(wb, pt = ptBal, sheet = "Balance", verbose = verbose)
  wb <- export.dt(dt$transactions[order(Category, Date), ], wb, "Transactions",
                  verbose = verbose)

  openxlsx::saveWorkbook(wb, file = fn, overwrite = T)
  cat("Export DONE.")
}

fig.costs <- function(dt, threshold = 100, verbose = F) {
  # Export costs to png file
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $ccy: currency (HUF or USD)
  #   threshold: minimum absolute limit for considering costs in a category
  #   verbose: print additional information
  
  # generate pivot table
  pt <- PivotTable$new()
  pt$addData(dt$all[CategoryType == "COSTS"])
  pt$addRowDataGroups("Month", addTotal = FALSE)
  pt$addRowDataGroups("CategoryGroup", addTotal = FALSE)
  pt$defineCalculation(calculationName="Amount",
                       summariseExpression = paste0("sum(Amount", dt$ccy, ")"))
  pt$evaluatePivot()
  df <- pt$asTidyDataFrame()
  df <- df[abs(df$rawValue) >= threshold, ]
  
  # create plot
  colourCount <- length(unique(df$CategoryGroup))
  getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
  df$Month <- as.numeric(as.character(df$Month))
  pl <- ggplot(df, aes(fill=df$CategoryGroup, y=-df$rawValue, x=factor(df$Month))) + 
    geom_bar(stat="identity")
  pl + 
    scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(colourCount)) +
    labs(fill="Category Group", y=paste0("Costs (", dt$ccy, ")"), x="Month")
  
  # save results
  fileName <- paste0(dt$folder, "/costs.png")
  ggsave(fileName)
  if (verbose) cat(paste0("Export costs to '", fileName, "'\n"))
}


fig.balance <- function(dt, verbose = F) {
  # Export costs to png file
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $ccy: currency (HUF or USD)
  #   threshold: minimum absolute limit for considering costs in a category
  #   verbose: print additional information
  
  # generate pivot table
  pt <- PivotTable$new()
  pt$addData(dt$all)
  pt$addRowDataGroups("AccountGroup", addTotal = FALSE)
  pt$addRowDataGroups("Month", addTotal = FALSE)
  cumulativeSum <- function(pt, filters, cell) {
    # get the month filter
    filter <- filters$getFilter("Month")
    if(is.null(filter)||(filter$type=="ALL")||(length(filter$values)>1)) {
      # there is no filter on Month in this cell
      newFilter <- PivotFilter$new(pt, variableName="Month", type="NONE")
      filters$setFilter(newFilter, action="replace")
    }
    else {
      # get the month value and modify the filter
      month <- filter$values
      newMonths <- seq(1, month)
      filter$values <- newMonths
    }
  }
  filterOverrides <- PivotFilterOverrides$new(pt, overrideFunction = cumulativeSum)
  pt$defineCalculation(calculationName = "Amount", filters = filterOverrides, format = "%.0f",
                       summariseExpression = paste0("sum(Amount", dt$ccy, ")"))
  pt$evaluatePivot()
  df <- pt$asTidyDataFrame()
  
  # create plot
  df$Month <- as.numeric(as.character(df$Month))
  pl <- ggplot(df, aes(fill=df$AccountGroup, y=df$rawValue, x=factor(df$Month))) + 
    geom_bar(stat="identity")
  pl + labs(fill="Account Group", y=paste0("Balance (", dt$ccy, ")"), x="Month")
  
  # save results
  fileName <- paste0(dt$folder, "/balance.png")
  ggsave(fileName)
  if (verbose) cat(paste0("Export costs to '", fileName, "'\n"))
}


fig.cumulative.balance <- function(dt, account, month = 3, verbose = F) {
  # Export cumulative balance for Checking/Savings account to png file
  #
  # Args:
  #   dt: list of data.tables
  #     $transations: all transactions
  #     $ccy: currency
  #   account: name of account
  #   month: last number of months to consider
  #   verbose: print additional information
  
  col <- paste0("Amount", dt$ccy)
  curMonth <- as.integer(format(Sys.Date(),"%m"))
  
  getCumBalance <- function(dt) {
    dt <- dt[order(Date)][, CumBalance := cumsum(eval(parse(text = col)))]
    dt <- dt[Month >= curMonth - month]
    dt$Date <- as.Date(dt$Date, '%Y.%m.%d')
    dt[, Seconds := as.numeric(as.POSIXct(Date, origin = "1970-01-01"))]
    dt <- rbind(
      dt,
      transform(dt[order(dt$Seconds),],
                Seconds=Seconds-1,  # required to avoid crazy steps
                CumBalance=ave(CumBalance, FUN=function(z) c(z[[1]], head(z, -1L)))
                ))
    dt[, Date2 := .POSIXct(Seconds)]
    return(dt)
  }
  
  dtCum1 <- dt$transactions[Account == paste0(account, ".Savings")]
  dtCum2 <- dt$transactions[Account %in% c(paste0(account, ".Checking"),
                                           paste0(account, ".Savings"))]
  dtCum1 <- getCumBalance(dtCum1)
  dtCum2 <- getCumBalance(dtCum2)
  
  maxBal <- 10^ceiling(log10(max(dtCum2$CumBalance)))
  
  pl <- ggplot() + 
    geom_ribbon(aes(x = Date2, ymin = 0, ymax = CumBalance, fill="Checking & Savings"), data = dtCum2,
                alpha=0.5) +
    geom_ribbon(aes(x = Date2, ymin = 0, ymax = CumBalance, fill="Savings"), data = dtCum1,
                alpha=0.5)
  pl + labs(y = paste0(account, " balance (", dt$ccy, ")"), x = "Date") +
    theme(legend.position = "top", legend.spacing.x = unit(0.1, 'cm')) +
    theme(legend.title = element_blank()) +
    scale_y_continuous(breaks = seq(0, maxBal, 1000),
                       minor_breaks = seq(0, maxBal, 500))
  
  fileName <- paste0(dt$folder, "/", account, "_", month, "m.png")
  ggsave(fileName)
  if (verbose) cat(paste0("Export cumulative balance to '", fileName, "'\n"))
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

pivot.balance <- function(dt, cumulative = T ,verbose = F) {
  # Create pivot table for balance sheet
  #
  # Args:
  #   dt: list of data.tables
  #     $all: all transactions
  #     $year: year name
  #     $ccy: currency (HUF or USD)
  #   cumulative: use cumulative sum
  #   verbose: print additional information
  #
  # Returns:
  #   pt: pivot table
  pt <- PivotTable$new()
  pt$theme <- theme
  dt$all$Date <- as.Date(dt$all$Date)
  pt$addData(dt$all)
  pt$addColumnDataGroups("Date", dataFormat = list(format = "%b"), dataSortOrder="desc",
                         totalCaption = as.character(dt$year), totalPosition = "before")
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
  return(pt)
}


pivot.income <- function(dt, showCategory = F, showPnL = F, showRatio = F,
                         verbose = F) {
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
  #   verbose: print additional information
  #
  # Returns:
  #   pt: pivot table
  pt <- PivotTable$new()
  pt$theme <- theme
  pt$addData(dt$all[CategoryType != "OTHER"])
  pt$addColumnDataGroups("Date", dataFormat = list(format = "%b"), dataSortOrder="desc",
                    totalCaption = as.character(dt$year), totalPosition = "before")
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
  return(pt)
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
  time <- ifelse(addTimeStamp, mgsub(c(" ",":"), c("_","."), as.POSIXlt(Sys.time())), "")
  fn <- paste0(dt$folder, "financial_report_", dt$year,"_", dt$ccy, time, ".xlsx")
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
  d <- copy(dt$initBalance)
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
  if (nrow(d) > 0) {
    d[, c("Account", "Amount", "AmountHUF", "AmountUSD", "Category", "Details")
      := list(Category, -Amount, -AmountHUF, -AmountUSD, "Duplicated",
              "Duplicated (category is separate account)")]
    list[d, ] <- finalize(dt, d)
    dt$all <- merge.dt(dt$all, d, name = "Duplicated")
  }
  return(dt)
}
