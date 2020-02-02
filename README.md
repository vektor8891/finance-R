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

The program executes the following steps:

## Step 1) Read

This is done by the `read.all(config, folder, year, verbose)` function. In this step the program:

1. reads all the date from the `config` file,
1. reads every report files in the `report` folder,
1. modifies data to get standard column names & data format
1. merges all the transations for a given `year` into a single table.

### Notes:

- Currently, only _*.csv_ and _*.xlsx_ can be used for report file.
- A file in the `report` folder is a valid report file if
	- it contains a report type (see **report_types** in the config file), AND
	- it contain the `year`.
- Column names in the report files can be:
	 - renamed (see **rename_rules** sheet in the config file), or
	 - customly created (see examples at the `add_column()` fuction in `read.R`).
- Report data can be only merged if - after renaming and modification - it contains the following columns (the order does not matter):
	- `Date`: date of transaction in *"YYYY.MM.DD"* format (e.g. "2019.01.20")
	- `Account`: name of the account (e.g. "Cash", "Unicredit")
	- `Amount`: amount of transaction (e.g. 100, 1000.5)
	- `Currency`: currency of transaction (e.g. "HUF", "USD")
	- `Details`: this string is used to categorize the transaction.

## Step 2) Summarize

This is done by the `summary.all(dt)` function. In this step the program:

1. adds manual adjustments to the transactions
1. calculates the total USD and HUF value of the initial balance and the notes & coins

## Step 3) Check

This is done by the `check.all(dt)` function. In this step the program:

1. every transaction has a category,
1. every pattern is used at least once for defining the category,
1. current value of notes & coins match with initial cash value and cash transactions,
1. for a given account and date balance matches with predefined value,
1. the total value of a given category matches with predefined value,
1. for given month and given the account the number of duplicates match a predefined value and there are no more duplicates,
1. accounts that expired have no transactions after given date,
1. accounts that are supposed to be updated regularly are not outdated.

## Step 4) Export

This is done by the `export.all(dt, folder, currency)` function. In this step the program:

1. creates an **output_TIMESTAMP.xlsx** file in the output `folder` and exports the following data in a given `currency`:
1. **Category**: income statement on a category level _(How much did I earn/spend in X category for Y month?)_.
1. **CategoryGroup**: income statement on a category group level _(How much did I earn/spend in X category group for Y month?)_, ratio to total income _(How much percent was that of my monthly total income?)_.
1. **Balance**: balance statement for given month _(How much money I had at the end of month X for account Y?)_.
1. **Transactions**: list of all transactions ordered by 1) categories and 2) date.

# Config File

All the configuration for the program is stored in **config.xlsx**. Each sheet is converted into a _data.table_. Here is a description for each sheet:

## report_types
Here you can define different report types which are used to decide how to process a report file. Using report types, you can rename columns/values (see **rename_rules** sheet) and you can also transform/add columns using the `add.column()` function in `read.R`. The column names are the following:

- ***Type***: Name of report type. Every file in the `reports` folder that contains this strings AND the `year` will be assigned to this type. **NOTE**: If a file only contains the `Type` but not the `year`, it won't be processed.
- ***Currency***: Currency of report (e.g. "USD", "HUF"). This field is mandatory unless:
	- the report file has a 'Currency' column for the currency. In this case this column will be automatically processed.
	- the report file has a column for the currency, but the column name is not 'Currency'. In this case you need to rename the column (see **rename_rules** sheet).
	- the report file contains the currency but not as a separate column but as part of another column. In this case the 'Currency' column must be calculated using the `add_column()` function.
- ***Account***: Account name. Must be consistent with _Account_ field in **balance_initial** sheet. This field is mandatory unless:
	- the report file has an 'Account' column for the account name. In this case this column will be automatically processed.
	- the report file has a column for the account, but the column name is not 'Account'. In this case you need to rename the column (see **rename_rules**).
	- the report file contains the account name but not as a separate column but as part of another column. In this case the 'Account' column must be calculated using the `add_column()` function.
- ***Separator***: Optional parameter, only applicable for _*.csv_ files. It is used as a parameter for `fread()`. The default value is `"auto"`.
- ***Header***: Optional parameter, only applicable for _*.csv_ files. It is used as a parameter for `fread()`. The default value is `"auto"`. If there is no header in the file set `Header` to `0`.
- ***Multiply***: All values will be multiplied with the number in this column. E.g. if for a debit or mastercard report type the opposite value is reported, you can set `Multiply` to `-1`.

## rename_rules

In this sheet the column names and the column values so that the program can successfully process it. Each row defines a rule. The column names are the following:

- ***Type***: Report type. Must match with _Type_ in **report_types**. This defines which report types the rename rule is applied to.
- ***Value***: To rename a column, set _Value_ to `Column`. To rename a specific value in a given column, set _Value_ to the name of the column.
- ***From***: This is the name of the original column (if _Value_ is set to `Column`) or the original value (otherwise) that will be changed.
- ***To***: This is the name of new column  (if _Value_ is set to `Column`) or the new value (otherwise) that will be replaced.

## cash_inventory

In this sheet you can provide an inventory of your notes and coins for different currency. If the value of the initial cash balance and the cash transactions don't match with the value of the cash inventory, the program will give an error. The value of the cash balance can be manually adjusted in the **balance_monthly** sheet. The column names are the following:

- ***Currency***: Currency of the note or coin (e.g. `HUF` or `USD`).
- ***Notional***: Notional amount of one note or coin (e.g. for a \$100 bill this is `100`).
- ***Amount***: Amount of note or coin (e.g. if you have five \$100 bill, you put `5` here).
- ***Note***: Optional.

For vouchers or gift cards it is recommended to set the _Notional_ to `1`, the amount to the gift card amount, and write `Gift card` to the _Note_ field. If you have multiple gift cards, you can define a specific row for each of them.

## fx_rates

In this sheet you can define FX rates for different currencies. This will be used to calculate total value of transactions from different currencies. The column names are the following:

- ***Currency***: Name of the currency (e.g. `HUF` or `USD`).
- ***FXRate***: FX rate (e.g. `270` or `0.023`).

## income_categories

In this sheet you can define category types, category groups and categories for the income statement. **Note**: You can only use categories that are listed here, otherwise the program will give an error. The column names are the following:

- ***CategoryType***: Category type (recommended values: `COSTS` and `INCOME` but it can be customized).
- ***CategoryGroup***: Category group (e.g. `Transportation` or `Communication`).
- ***Category***: Category name (e.g. `Public transportation` or `Telephone`).

## patterns

In this sheet you can define which patterns belong to a given category. For those transactions that don't have a _Category_ value, the program automatically will try to assign to one based on patterns in the _Details_ field. **Note**: If multiple patterns can be found in the _Details_ field and the related categories are not the same, the program will give an error. To avoid such cases, you can define a longer pattern or you can change the _Details_ field. The column names are the following:

- ***Category***: Name of the category which will be assigned to the transaction.
- ***Pattern***: Pattern text. If found in the _Details_ field, the transaction will be assigned to _Category_.
- ***Note***: Optional parameter. Here you can add note to remember why you assigned a specific category for a given pattern. This can be useful if the reason is not trivial.
- ***Used***: Optional parameter. By default the program throws an error if it finds an unused pattern and requires the user to remove from the list. This can be overwritten by setting this parameter to `N` which means the pattern will be skipped for the check.

## balance_initial

In this sheet you can define different accounts and their features (account type, account group, initial balance etc.) which will be used for the balance sheet. **Note**: Accounts and report types are treated separately by the program. Several report types can belong to the same account, and one report type can contain several accounts. The column names are the following:

- ***AccountType***: Account type (recommended values: `ASSETS` and `LIABILITIES` but it can be customized).
- ***AccountGroup***: Account group (e.g. `Bank` or `Long term loans`).
- ***Account***: Account name (e.g. `Unicredit Checking` or `Student loan`).
- ***HUF***: Initial balance in HUF. If the account has different currency, set this to `0`.
- ***USD***: Initial balance in USD. If the account has different currency, set this to `0`.
- ***EUR***: Initial balance in EUD. If the account has different currency, set this to `0`.
- ***NoTransactionAfter***: Optional parameter. If the account has been decommissioned e.g. on January 4, 2019, you can write `2019.01.04` here and the program will check if this account has no transactions after this date. If there is some, it will give an error.
- ***UpdateFrequencyDays***: Optional parameter. This can be useful to remind yourself to update the reports for the different accounts regularly. E.g. if you want to update transactions for an account in every 30 days, you can put `30` here and the program will check if the latest transaction on this account is no older than 30 days. If it is older, it will give you an error.
- ***Multiply***: All monthly balances will be multiplied with the number in this column. **Note**: This does not affect initial balance, only monthly balances. E.g. if for a debit or mastercard report type the opposite values are reported in the monthly balance, you can copy the original summary from the monthly report and set `Multiply` to `-1`.

## balance_monthly (OPTIONAL)

In this sheet you can add monthly balances for accounts. This will be used to check if the initial balance and transactions for the given account until the given date match with the given value. If there is a difference, you can also add manual adjustments to avoid error, which will appear as a separate category in the final reports. The column names are the following:

- ***Account***: Account name. Must match with _Account_ in **balance_initial**.
- ***Month***: Integer value for the month (e.g `1` refers to January and `12` refers to December).
- ***Currency***: Name of the currency (e.g. `HUF` or `USD`).
- ***Balance***: Balance value for given _Account_ and given _Month_. The program will check if the initial balance and transactions for the _Account_ for the given _Month_ match with _Balance_ and give error if not.
- ***Adjustment***: Manual adjustment to avoid error (initial balance and transactions will be adjusted with this value and compared with _Balance_ afterwards). **Note**: Adjustmens will appear in a separate category in the final reports.
- ***Day***: Optional parameter. If the balance value is defined not for the whole month but e.g. until the 15th day, you can write here `15` and the program will only add up transactions before this date.
- ***Duplicates***: Number of duplicates for given _Account_ and given _Month_. By default, no duplicates are allowed among the transactions. If there are some, they must be noted here, otherwise the program will give an error. E.g. if there are 2 transactions with exactly the same values, you need to write `1` here (because only one of them is duplicate). **Note**: If there are no duplicates, this parameter is optional.

# Contact

If you have further questions about this program, feel free to write me on Github.