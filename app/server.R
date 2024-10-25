######## [START] Packages requirements ########

##### Add here the packages needed #############################################
packagesNeeded <- c("dplyr", "knitr", "tidyr", "shiny", "shinydashboard", 
                    "shinyjs", "fresh", "reactable", "plotly", "ggplot2",
                    "KrakenR")
################################################################################

installedPackages <- installed.packages()

for (packageName in packagesNeeded) {
  packageExists <- is.element(packageName, installedPackages)
  if (packageExists != TRUE) {
    install.packages(packageName)
    library(packageName, character.only = TRUE)
    print(paste(packageName, "has been installed and the library is loaded!"))
  } else {
    library(packageName, character.only = TRUE)
    print(paste(packageName, "is installed and the library is loaded!"))
  }
}

rm(installedPackages, packageName, packagesNeeded, packageExists)

######## [END] Packages requirements ########

######## [START] Set working directory ########

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######## [END] Set working directory ########





######## [START] Server Logic ########

server <- function(input, output, session) {
  
  ############ [START] Loading Screen #############
  
  delay(3000, hide("loading-screen", anim = TRUE, animType = "fade"))
  
  # # Reactive value to check if the app is ready
  # app_ready <- reactiveVal(FALSE)
  # 
  # # Perform initial data loading
  # observe({
  #   # Example API calls or data loading
  #   data_pairs <- rate_limited_call(getPairs)
  #   data_tickers <- rate_limited_call(getTickers)
  #   
  #   # Ensure data is loaded before continuing
  #   if (!is.null(data_pairs) && !is.null(data_tickers)) {
  #     # Set loaded data in reactive values
  #     getpairs_data(data_pairs)
  #     gettickers_data(data_tickers)
  #     
  #     # Indicate that loading is complete
  #     app_ready(TRUE)
  #   }
  # })
  # 
  # # Hide loading screen once the app is ready
  # observe({
  #   if (app_ready()) {
  #     hide("loading-screen", anim = TRUE, animType = "fade")
  #   }
  # })
  
  ############ [END] Loading Screen #############  
  
  ############ [START] API Rate Limit #############
  
  # Track the time of the last API call
  last_call_time <- Sys.time() - 2  # Initialize to more than 1 second ago
  
  rate_limited_call <- function(api_call, timeout = 1) {
    # Calculate time since the last call
    time_since_last_call <- Sys.time() - last_call_time
    
    # If the last call was less than 1 second ago, wait
    if (time_since_last_call < timeout) {
      Sys.sleep(timeout - as.numeric(time_since_last_call, units = "secs"))
      message("Timeout: waiting before next API call.")
    }
    
    # Proceed with the API call
    tryCatch({
      # Update the timestamp of the last call
      last_call_time <<- Sys.time()
      
      # Execute the API call
      data <- api_call()
      message("API call successful.")
      return(data)
      
    }, error = function(e) {
      message("API call error: ", e$message)
      return(NULL)
    })
  }
  
  ############ [END] API Rate Limit ############
  
  ############ [START] Generic API Calls with Throttling ############
  
  # getPairs() - Updates every 24h - Generic
  getpairs_data <- reactiveVal()
  observe({
    invalidateLater(86400000, session)  # Refresh every 24 hours
    data <- rate_limited_call(getPairs)
    if (!is.null(data)) {
      getpairs_data(data)  # Update the reactive value with the new data
      message("getPairs data updated successfully.")
    } else {
      message("getPairs call failed.")
    }
  })
  
  # getTickers() - Updates every 5s - Generic
  gettickers_data <- reactiveVal()
  observe({
    invalidateLater(3000, session)
    data <- rate_limited_call(getTickers)
    if (!is.null(data)) {
      gettickers_data(data)  # Update the reactive value with the new data
      message("getTickers data updated successfully.")
    } else {
      message("getTickers call failed.")
    }
  })
  
  ############ [END] Generic API Calls with Throttling ############
  
  ############ [START] Trading Pair Picker / Title ############
  
  ## Pairs names
  selected_pair <- reactiveVal()  # Default value
  
  observe({
    # Extract wsname
    pair_names <- getpairs_data()$wsname
    
    # Update dropdown choices
    updateSelectInput(session, "pair_select",
                      choices = pair_names,
                      selected = "XBT/CHF")
  })
  
  # Update titleBox based on last valid selection in selected_pair
  observeEvent(input$pair_select, {
    if (!is.null(input$pair_select) && input$pair_select != "") {
      selected_pair(input$pair_select)  # Update only on valid selection
    }
  })
  
  # Display selected trading pair in titleBox
  output$titleBox <- renderValueBox({
    valueBox(selected_pair(), "TRADING PAIR", icon = icon("coins"))
  })
  
  ############ [END] Trading Pair Picker / Title ############
  
  ############ [START] Specific API Calls with Throttling ############
  
  # # getOHLC() - Updates every 10m - Specific
  # getohlc_data <- reactiveVal()
  # observe({
  #   invalidateLater(600000, session)
  #   getohlc_data(rate_limited_call(function() getOHLC(last_valid_pair_id(), interval = "1h", since = as.numeric(Sys.time()) - (24 * 3600))))
  # })
  # 
  # # getTrades() - Updates every 5s - Specific
  # gettrades_data <- reactiveVal()
  # observe({
  #   invalidateLater(5000, session)
  #   gettrades_data(rate_limited_call(function() getTrades(last_valid_pair_id())))
  # })
  
  ############ [END] Specific API Calls with Throttling ############
  
  ######## [START] getTickers() / Last Price ########
  
  # Reactive values for selected currency and pair ID
  selected_currency <- reactiveVal("CHF")
  last_valid_pair_id <- reactiveVal("XBTCHF")
  
  # Update selected currency and pair ID based on the dropdown selection
  observeEvent(input$pair_select, {
    if (!is.null(input$pair_select) && input$pair_select != "") {
      # Extract currencies from the selected pair
      currencies <- strsplit(input$pair_select, "/")[[1]]
      selected_currency(if (length(currencies) > 1) currencies[2] else "")
      
      # Get the PairID based on selected pair without an additional API call
      new_pair_id <- as.character(getpairs_data()$PairID[getpairs_data()$wsname == input$pair_select])
      if (!is.null(new_pair_id) && new_pair_id != "") {
        last_valid_pair_id(new_pair_id)
      }
    }
  })
  
  # Retrieve the relevant ticker data for the selected pair from gettickers_data
  ticker_data <- reactive({
    req(last_valid_pair_id())  # Ensure the pair ID is available
    gettickers_data()[gettickers_data()$PairID == last_valid_pair_id(), ]
  })
  
  # Display the last price for the selected pair
  output$lastpriceBox <- renderValueBox({
    ticker <- ticker_data()
    valueBox(
      paste0(if (nrow(ticker) > 0) ticker$Ask_Price else "N/A", " ", selected_currency()),
      "LAST PRICE", icon = icon("coins")
    )
  })
  
  # # Track selected currency and pair ID based on dropdown choice
  # selected_currency <- reactiveVal("CHF")
  # last_valid_pair_id <- reactiveVal("XBTCHF")
  # 
  # observeEvent(input$pair_select, {
  #   if (!is.null(input$pair_select) && input$pair_select != "") {
  #     currencies <- strsplit(input$pair_select, "/")[[1]]
  #     selected_currency(if (length(currencies) > 1) currencies[2] else "")
  # 
  #     # Update PairID for selected pair
  #     new_pair_id <- as.character(getpairs_data()$PairID[getpairs_data()$wsname == input$pair_select])
  #     if (!is.null(new_pair_id) && new_pair_id != "") {
  #       last_valid_pair_id(new_pair_id)
  #     }
  #   }
  # })
  # 
  # # Update ticker data every 3 seconds
  # ticker_data <- reactive({
  #   gettickers_data()[gettickers_data()$PairID == last_valid_pair_id(), ]
  # })
  # 
  # output$lastpriceBox <- renderValueBox({
  #   ticker <- ticker_data()
  #   valueBox(
  #     paste0(if (nrow(ticker) > 0) ticker$Ask_Price else "N/A", " ", selected_currency()),
  #     "LAST PRICE", icon = icon("coins")
  #   )
  # })
  
  ######## [END] getTickers() / Last Price ########
  
  ######## [START] getOHLC() / 24H Change ########
  
  # selected_change_24h <- reactiveVal("N/A")
  # 
  # observe({
  #   invalidateLater(3000, session)
  #   pair_id <- last_valid_pair_id()
  #   if (!is.null(pair_id)) {
  #     ohlc_data <- getohlc_data()
  #     pair_ohlc <- ohlc_data[ohlc_data$PairID == pair_id, ]
  #     if (nrow(pair_ohlc) >= 2) {
  #       close_24h_ago <- pair_ohlc$Close[1]
  #       current_close <- tail(pair_ohlc$Close, 1)
  #       selected_change_24h(
  #         if (!is.na(current_close) && !is.na(close_24h_ago) && close_24h_ago != 0) 
  #           round(((current_close - close_24h_ago) / close_24h_ago) * 100, 2) 
  #         else "N/A"
  #       )
  #     }
  #   }
  # })
  # 
  # output$changeBox <- renderValueBox({
  #   valueBox(
  #     paste0(selected_change_24h(), " %"),
  #     "24H CHANGE", icon = icon("percent"),
  #     color = if (selected_change_24h() >= 0) "green" else "red"
  #   )
  # })
  
  ######## [END] getOHLC() / 24H Change ########
  
}

######## [END] Server Logic ########
