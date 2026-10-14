# Personal Expense Tracker Using R Shiny

## Overview
A local web dashboard to record income and expenses, review savings, view category/monthly charts, filter transactions, and download the data as CSV.

## Files
- `app.R` — complete Shiny application
- `data/expenses.csv` — sample transactions; the app updates this file when a transaction is added
- `docs/Personal_Expense_Tracker_Assignment.docx` — college-format project documentation
- `docs/Personal_Expense_Tracker_Assignment.pdf` — PDF version (if exported in this environment)

## Requirements
- R (recent version recommended)
- RStudio (recommended)
- R packages: `shiny`, `ggplot2`

Install packages once:
```r
install.packages(c("shiny", "ggplot2"))
```

## Run
1. Extract the project folder.
2. Open the folder in RStudio.
3. Set it as the working directory, or open the folder as an RStudio project.
4. Run:
```r
shiny::runApp()
```
The dashboard opens in a browser. Add a transaction from the sidebar. Records are stored in `data/expenses.csv`.

## Main features
- Add income or expense transactions with date, category, description, and amount.
- KPI cards for total income, total expenses, and net savings.
- Bar chart of expenses by category.
- Line chart of expenses by month.
- Recent transaction table and filterable full transaction table.
- CSV download.

## Notes
- Amounts must be greater than zero.
- This is an educational local application, not an accounting or banking system.
- Back up `data/expenses.csv` before replacing or deleting it.
