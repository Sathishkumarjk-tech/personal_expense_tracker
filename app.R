# Personal Expense Tracker Using R Shiny
# Run from this folder in RStudio with: shiny::runApp()
# Required packages: shiny, ggplot2

required_packages <- c("shiny", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install required packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(shiny)
library(ggplot2)

DATA_FILE <- file.path("data", "expenses.csv")
CATEGORIES <- c("Food", "Travel", "Shopping", "Bills", "Education",
                "Health", "Entertainment", "Other")
TYPES <- c("Expense", "Income")

# Create a starter CSV if the data file is missing.
if (!file.exists(DATA_FILE)) {
  dir.create(dirname(DATA_FILE), recursive = TRUE, showWarnings = FALSE)
  starter <- data.frame(
    Date = as.Date(c("2026-10-01", "2026-10-02", "2026-10-03",
                     "2026-10-04", "2026-10-05", "2026-10-06",
                     "2026-10-07", "2026-10-08")),
    Type = c("Income", "Expense", "Expense", "Expense",
             "Expense", "Income", "Expense", "Expense"),
    Category = c("Other", "Food", "Travel", "Bills",
                 "Education", "Other", "Shopping", "Food"),
    Description = c("Part-time income", "Lunch", "Bus pass", "Mobile recharge",
                    "Course materials", "Freelance work", "Notebook", "Groceries"),
    Amount = c(4500, 180, 90, 299, 650, 1200, 240, 420),
    stringsAsFactors = FALSE
  )
  write.csv(starter, DATA_FILE, row.names = FALSE)
}

read_transactions <- function() {
  if (!file.exists(DATA_FILE)) {
    return(data.frame(
      Date = as.Date(character()), Type = character(), Category = character(),
      Description = character(), Amount = numeric(), stringsAsFactors = FALSE
    ))
  }
  x <- tryCatch(read.csv(DATA_FILE, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !all(c("Date", "Type", "Category", "Description", "Amount") %in% names(x))) {
    return(data.frame(
      Date = as.Date(character()), Type = character(), Category = character(),
      Description = character(), Amount = numeric(), stringsAsFactors = FALSE
    ))
  }
  x$Date <- as.Date(x$Date)
  x$Amount <- suppressWarnings(as.numeric(x$Amount))
  x$Type <- as.character(x$Type)
  x$Category <- as.character(x$Category)
  x$Description <- as.character(x$Description)
  x <- x[!is.na(x$Date) & !is.na(x$Amount) & x$Amount >= 0, , drop = FALSE]
  x
}

ui <- fluidPage(
  titlePanel("Personal Expense Tracker"),
  tags$head(tags$style(HTML("
    body { background: #f5f7fb; }
    .well { background: #ffffff; border-radius: 10px; border: 1px solid #e0e6ef; }
    .value-box { background: white; padding: 16px; border-radius: 10px;
                 border: 1px solid #e0e6ef; margin-bottom: 14px; min-height: 95px; }
    .value-box h4 { margin-top: 0; color: #536273; font-size: 14px; }
    .value-box h2 { margin-bottom: 0; font-size: 25px; color: #172b4d; }
    .help-note { color: #68778a; font-size: 12px; }
  "))),
  sidebarLayout(
    sidebarPanel(
      h4("Add a transaction"),
      dateInput("date", "Date", value = Sys.Date()),
      selectInput("type", "Transaction type", choices = TYPES, selected = "Expense"),
      selectInput("category", "Category", choices = CATEGORIES),
      textInput("description", "Description", placeholder = "e.g., Lunch or salary"),
      numericInput("amount", "Amount (₹)", value = 100, min = 0.01, step = 1),
      actionButton("add", "Add transaction", class = "btn-primary"),
      br(), br(),
      actionButton("refresh", "Refresh data"),
      hr(),
      downloadButton("download", "Download CSV"),
      p(class = "help-note", "Transactions are saved to data/expenses.csv on this computer.")
    ),
    mainPanel(
      fluidRow(
        column(4, div(class = "value-box", h4("Total income"), h2(textOutput("income", inline = TRUE)))),
        column(4, div(class = "value-box", h4("Total expenses"), h2(textOutput("expenses", inline = TRUE)))),
        column(4, div(class = "value-box", h4("Net savings"), h2(textOutput("savings", inline = TRUE))))
      ),
      tabsetPanel(
        tabPanel("Dashboard",
          br(),
          fluidRow(
            column(6, plotOutput("categoryPlot", height = "300px")),
            column(6, plotOutput("monthlyPlot", height = "300px"))
          ),
          h4("Recent transactions"),
          tableOutput("recent")
        ),
        tabPanel("All transactions",
          br(),
          fluidRow(
            column(4, dateRangeInput("dateRange", "Filter date range",
                                    start = Sys.Date() - 365, end = Sys.Date() + 365)),
            column(4, selectInput("typeFilter", "Transaction type",
                                  choices = c("All", TYPES), selected = "All")),
            column(4, selectInput("categoryFilter", "Category",
                                  choices = c("All", CATEGORIES), selected = "All"))
          ),
          tableOutput("allTransactions")
        ),
        tabPanel("About",
          br(),
          h4("Project objectives"),
          tags$ul(
            tags$li("Record income and expenses with date, category, description, and amount."),
            tags$li("Calculate total income, total expenses, and net savings."),
            tags$li("Visualize category spending and monthly totals."),
            tags$li("Filter transactions and export the dataset as CSV.")
          ),
          p("This is a local learning project. Keep a backup of the CSV file.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  transactions <- reactiveVal(read_transactions())

  observeEvent(input$refresh, {
    transactions(read_transactions())
    showNotification("Transaction data refreshed.", type = "message")
  })

  observeEvent(input$add, {
    req(input$date, input$type, input$category, input$description, input$amount)
    description <- trimws(input$description)
    if (!nzchar(description)) {
      showNotification("Please enter a description.", type = "error")
      return()
    }
    if (is.na(input$amount) || input$amount <= 0) {
      showNotification("Amount must be greater than zero.", type = "error")
      return()
    }

    new_row <- data.frame(
      Date = as.character(as.Date(input$date)),
      Type = input$type,
      Category = input$category,
      Description = description,
      Amount = as.numeric(input$amount),
      stringsAsFactors = FALSE
    )
    current <- transactions()
    current_out <- current
    current_out$Date <- as.character(current_out$Date)
    updated <- rbind(current_out, new_row)
    write.csv(updated, DATA_FILE, row.names = FALSE)
    transactions(read_transactions())
    updateTextInput(session, "description", value = "")
    showNotification("Transaction added and saved.", type = "message")
  })

  filtered <- reactive({
    x <- transactions()
    if (nrow(x) == 0) return(x)
    if (!is.null(input$dateRange) && length(input$dateRange) == 2) {
      x <- x[x$Date >= as.Date(input$dateRange[1]) &
               x$Date <= as.Date(input$dateRange[2]), , drop = FALSE]
    }
    if (!is.null(input$typeFilter) && input$typeFilter != "All") {
      x <- x[x$Type == input$typeFilter, , drop = FALSE]
    }
    if (!is.null(input$categoryFilter) && input$categoryFilter != "All") {
      x <- x[x$Category == input$categoryFilter, , drop = FALSE]
    }
    x
  })

  money <- function(x) paste0("₹", format(round(x, 2), big.mark = ",", nsmall = 2))
  output$income <- renderText({
    x <- transactions()
    money(sum(x$Amount[x$Type == "Income"], na.rm = TRUE))
  })
  output$expenses <- renderText({
    x <- transactions()
    money(sum(x$Amount[x$Type == "Expense"], na.rm = TRUE))
  })
  output$savings <- renderText({
    x <- transactions()
    income <- sum(x$Amount[x$Type == "Income"], na.rm = TRUE)
    expenses <- sum(x$Amount[x$Type == "Expense"], na.rm = TRUE)
    money(income - expenses)
  })

  output$categoryPlot <- renderPlot({
    x <- transactions()
    x <- x[x$Type == "Expense", , drop = FALSE]
    validate(need(nrow(x) > 0, "Add expense transactions to see category spending."))
    totals <- aggregate(Amount ~ Category, data = x, sum)
    ggplot(totals, aes(x = reorder(Category, Amount), y = Amount)) +
      geom_col(fill = "#3977d5", width = 0.72) +
      coord_flip() +
      labs(title = "Expenses by category", x = NULL, y = "Amount (₹)") +
      theme_minimal(base_size = 12)
  })

  output$monthlyPlot <- renderPlot({
    x <- transactions()
    x <- x[x$Type == "Expense", , drop = FALSE]
    validate(need(nrow(x) > 0, "Add expense transactions to see monthly totals."))
    x$Month <- format(x$Date, "%Y-%m")
    totals <- aggregate(Amount ~ Month, data = x, sum)
    ggplot(totals, aes(x = Month, y = Amount, group = 1)) +
      geom_line(color = "#e07a35", linewidth = 1) +
      geom_point(color = "#e07a35", size = 2.5) +
      labs(title = "Monthly expense trend", x = "Month", y = "Amount (₹)") +
      theme_minimal(base_size = 12)
  })

  output$recent <- renderTable({
    x <- transactions()
    if (nrow(x) == 0) return(data.frame(Message = "No transactions yet."))
    x <- x[order(x$Date, decreasing = TRUE), , drop = FALSE]
    x$Date <- format(x$Date, "%Y-%m-%d")
    head(x, 6)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$allTransactions <- renderTable({
    x <- filtered()
    if (nrow(x) == 0) return(data.frame(Message = "No transactions match the selected filters."))
    x <- x[order(x$Date, decreasing = TRUE), , drop = FALSE]
    x$Date <- format(x$Date, "%Y-%m-%d")
    x
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download <- downloadHandler(
    filename = function() paste0("expense_transactions_", Sys.Date(), ".csv"),
    content = function(file) write.csv(transactions(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
