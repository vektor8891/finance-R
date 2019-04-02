# Overview

The purpose of this finance app is to summarize & categorize transactions for a given year. The program:

1. reads data from multiple sources & multiple currencies
1. categorizes them based on string patterns
1. performs several tests to avoid typos and errors
1. summarizes & exports results

# How to use

To run the app, simply execure **main.R**. Here you can change the following parameters:

- `year`: which year do we want to check
- `currency`: currency for reports
- `verbose`: if **TRUE** print additional information

There are two additional parameters which are useful for debugging:

- `strictMode`: if **TRUE** throws an error if any of the tests fail, if **FALSE**, it just gives warnings.
- `newRun`: if **TRUE** runs everything from scratch, if **FALSE** loads all the data from memory.

# Description

The program executes the following functions:

- `read.all(input, folder, year, verbose)`: this function:
	1. reads every sheet of the `input` file and converts it into a _data.table_,
	1. for every file in the `report` folder:
		1. checks the type of the report file
		1. adds & transforms columns based on report type to get standard column names & data format
		1. merges all the transations for a given `year` into a single _data.table_,
	1. returns a list of _data.tables_ (`dt`).
- `summary.all(dt)`: this function:
	1. adds manual adjustments to the transactions
	1. calculates the total USD and HUF value of the initial balance and the notes & coins
	1. adds extra rows for categories that are treated as a separate account.
- `check.all(dt)`: this function tests if:
	1. every transaction has a category,
	1. every pattern is used at least once for defining the category,
	1. current value of notes & coins match with initial cash value and cash transactions,
	1. for a given account and date balance matches with predefined value,
	1. the total value of a given category matches with predefined value,
	1. for given month and given the account the number of duplicates match a predefined value and there are no more duplicates,
	1. accounts that expired have no transactions after given date,
	1. accounts that are supposed to be updated regularly are not outdated.
- `export.all(dt, folder, currency)`: this function creates an **output_TIMESTAMP.xlsx** file in the output `folder` and exports the following data in a given `currency`:
	1. **Category**: income statement on a category level _(How much did I earn/spend in X category for Y month?)_.
	1. **CategoryGroup**: income statement on a category group level _(How much did I earn/spend in X category group for Y month?)_, ratio to total income _(How much percent was that of my monthly total income?)_.
	1. **Balance**: balance statement for given month _(How much money I had at the end of month X for account Y?)_.
	1. **Transactions**: list of all transactions ordered by 1) categories and 2) date.


# Inputs

All the input data is stored in **input.xlsx**. Each sheet is converted into a _data.table_. Here is a description for each sheet:

## report_types
Here you can define different report types which are used to decide how to process report file. For each report type you can rename columns/values based on **rename_rules**. You can also transform/add columns using the `add.column()` function in `read.R`.

- _Type_: Name of report type. Every file in the `reports` folder that contains this strings AND the `year` will be assigned to this type. If a file only contains the type but not the year, it won't be processed.
- _Currency_: Currency of report. This field is not mandatory but it's recommended to fill it. You may leave field empty if:
	- the currency is in the report under 'Currency' column,
	- the column name for currency is renamed to 'Currency' using **rename_rules**.
	- a column 'Currency' is calculated using the `add_column()` function.
- _Account_: Account name. Must match with _Account_ in **balance_initial**.
	- If report file has 'Account' column, you may leave this field empty.
	- If the account name is included in the report but the column name is not 'Account', you may leave this field empty and rename it using **rename_rules**.
	- If currency is included in the report but not as a separate column, you can compute it using the `add_column()` function.

# Frequently Asked Questions?

## How to add a new report type?
