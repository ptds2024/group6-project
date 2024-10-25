######## [START] Custom Theme ########

kraken <- create_theme(
  adminlte_color(
    light_blue = "#023E7D"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#001233",
    dark_hover_bg = "#0466C8",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#001845",
    box_bg = "#002855", 
    info_box_bg = "#0466C8"
  )
)

######## [END] Custom Theme ########

######## [START] Shiny UI ########

dashboardPage(
  
  ## Header content
  dashboardHeader(
    title = "KrakenR",
    dropdownMenu(type = "notification",
                 notificationItem(
                   text = "5 new users today",
                   icon("users")
                 ),
                 notificationItem(
                   text = "12 items delivered",
                   icon("truck"),
                   status = "success"
                 ),
                 notificationItem(
                   text = "Server load at 86%",
                   icon = icon("exclamation-triangle"),
                   status = "warning"
                 )
    )
  ),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    #use_theme(kraken),
    #tags$head(tags$link(rel = "stylesheet", href = "style.css")),
    ### Loading screen
    useShinyjs(),
    # CSS for loading screen
    tags$head(
      tags$style(HTML("
        #loading-screen {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: #001845;
          color: #ffffff;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 24px;
          z-index: 1000;
        }
      "))
    ),
    # Loading screen
    div(id = "loading-screen", "Loading, please wait..."),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              
              fluidRow(
                
                # A static valueBox
                valueBoxOutput("titleBox", width = 3),
                
                # A static lastpriceBox
                valueBoxOutput("lastpriceBox", width = 3),
                
                # A static valueBox
                valueBoxOutput("changeBox", width = 2),
                
                # A static valueBox
                valueBox(10 * 2, "24h VOLUME", icon = icon("chart-line"), width = 2),
                
                box(title = NULL, width = 2, solidHeader = FALSE,
                    selectizeInput("pair_select", "Choose a Trading Pair",
                                   choices = NULL,
                                   options = list(
                                     placeholder = 'Type to search for a pair',
                                     allowEmptyOption = FALSE
                                   )
                    )
                )
              ),
              
              fluidRow(
                box(
                  title = "Order book", width = 4, height = 400, solidHeader = TRUE, status = "primary",
                  "Box content"
                ),
                box(
                  title = "Market chart", width = 8, height = 400, solidHeader = TRUE, 
                  "Box content"
                ),
              ),        
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
      )
    )
  )
)

######## [END] Shiny UI ########
