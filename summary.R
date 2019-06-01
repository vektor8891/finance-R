source("read.R")

add.adjustment <- function(dt) {
  # Add adjustment to transactions
  #
  # Args:
  #   dt: list of data.tables
  #     $year: year name
  #     $monthlyBalance: monthly balance statements
  #
  # Returns:
  #   dt: list of data.tables
  #     $all: all transactions with adjustment
  d <- dt$monthlyBalance[Adjustment != 0]
  d$Year <- dt$year
  d <- add.columns(d, type = "adjust")
  d$Amount <- d$Adjustment
  list[d, ] <- finalize(dt, d)
  dt$all <- merge.dt(dt$all, d, "adjust")
  return(dt)
}

add.extra.rows <- function(dt) {
  # Add extra rows to transactions
  #
  # - Add extra rows for category "Deposit" in "Unicredit.Saving1"
  #
  # Args:
  #   dt: list of data.tables
  #
  # Returns:
  #   dt: list of data.tables
  #     $all: transactions with extra rows
  cat <- dt$income[IsAccount == "Y"]
  for (i in 1:nrow(cat)) {
    d <- dt$all[Category == cat[i, Category]]
    dt$all[Category == cat[i, Category], Category := cat[i, Replace]]
    setkeyv(dt$all, cols = colnames(dt$all))
    d$Account <- cat[i, Category]
    d$Category <- cat[i, Replace]
    d$Amount <- d$Amount * -1
    d$AmountHUF <- d$AmountHUF * -1
    d$AmountUSD <- d$AmountUSD * -1
    dt$all <- merge.dt(dt$all, d, "extra rows")
  }
  return(dt)
}

balance.summary <- function(dt) {
  # Summarize initial balance
  #
  # Args:
  #   dt: list of data.tables
  #     $initBalance: initial balance
  #     $fx: FX rates
  #
  # Returns:
  #   dt: list of data.tables
  dt$initBalance[, InitialHUF := HUF + USD * dt$fx["USD"] + EUR * dt$fx["EUR"]]
  dt$initBalance[, InitialUSD := round(InitialHUF / dt$fx["USD"], 2)]
  return(dt)
}

cash.summary <- function(dt) {
  # Summarize cash inventory
  #
  # Args:
  #   dt: list of data.tables
  #     $notes: notes inventory
  #     $fx: FX rates
  #
  # Returns:
  #   dt: list of data.tables
  #     $notes: notes with HUF/USD amount
  #     $notesHUF: total HUF value of notes
  #     $notesUSD: total USD value of notes
  dt$notes[, AmountHUF := Notional * Amount * dt$fx[Currency]]
  dt$notes[, AmountUSD := round(AmountHUF / dt$fx["USD"], 2)]
  dt$notesHUF <- sum(dt$notes$AmountHUF)
  dt$notesUSD <- sum(dt$notes$AmountUSD)
  return(dt)
}

summary.all <- function(dt) {
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
  dt <- add.extra.rows(dt)
  return(dt)
}