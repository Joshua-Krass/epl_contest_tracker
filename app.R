# Josh Krassenstein
# This application takes the predicted EPL 2022-2023 tables created
# by the TM Analytics team members and computes the standings 
# compared to the live table using rvest web scraping
#
#    
#

# packages needed
library(shiny)
library(rvest)
library(googlesheets4)
library(DT)
library(dplyr)
library(shinycssloaders)
library(shinydashboard)
library(stringr)
# bring in and clean live epl data 
epl <- rvest::read_html("https://www.premierleague.com/tables")

epl_table <- epl %>% html_nodes("table") %>% html_text()
epl_table

points <- epl %>% html_nodes(".points") %>% html_text()
points <- points[2:21]

team <- epl %>% html_nodes(".long") %>% html_text()
team <- team[1:20]

placement <- epl %>% html_nodes(".value") %>% html_text()
placement <- placement[1:20]
live_table <- data.frame(
  ranking = placement,
  team = team,
  points = points
)

emps <- c("Kevin", "Laura", "Jacob", "Josh", "Paul", "Colin")
# load in predictions
epl_predictions <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1TqFW99kr5ayaSrxpkTdPqyVAegB0IaGzVPnfT2KCDaE/edit#gid=0")

epl <- cbind(live_table$team, epl_predictions) %>%
  dplyr::rename(Standings = `live_table$team`)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("TM Premier League Standings"),
    # Sidebar with a slider input for number of bins 
        # Show the live table
    mainPanel(
      tabsetPanel(
        tabPanel("Scores",
                 actionButton("calc_standings",
                              "Calculate Standings"),
                 shinycssloaders::withSpinner(DT::dataTableOutput("emp_standings"),
                                              type = 6),
                 fluidRow(
                   column(4, 
                        #  tags$style(".small-box.bg-yellow { background-color: #FFD700 !important; color: #000000 !important; }"),
                          shinydashboard::valueBoxOutput("first_place"))
                 )
                 ),
        tabPanel("Employee Detail",
                 selectInput("emp",
                             "Select Employee",
                             choices = emps)),
        tabPanel("Live Table", 
                 div(DT::dataTableOutput("live_table_dt"),
                     style = "font-size: 70%;")
                )
      )
    )
)

# Define server logic
server <- function(input, output) {
  tm_standings <- eventReactive(input$calc_standings, {
    test_df <- epl
    index <- 0
    for(i in emps) {
      # i <- emps[2]
      index <- index + 3
      col_1 <- paste0(i,"_index")
      col_2 <- paste0(i,"_diff")
      col_3 <- paste0(i, "_score")
      test_df <- test_df %>% 
        dplyr::mutate(col_1 = as.numeric(NA),
                      col_2 = as.numeric(NA),
                      col_3 = as.numeric(NA)) %>%
        dplyr::rename(!!col_1 := col_1,
                      !!col_2 := col_2,
                      !!col_3 := col_3)
      for(j in 1:20) {
        #   j <- 6
        assign(paste0(i,j), as.numeric(test_df %>%
                                         dplyr::filter(!!as.symbol(i) == (test_df %>% 
                                                                            dplyr::filter(`Ranking` == j))$Standings) %>%
                                         dplyr::select(`Ranking`)))
        
        
        new_diff <- abs(j - get(paste0(i,j)))
        score <- as.numeric(as.character(memisc::cases(
          "5" = new_diff == 0,
          "4" = new_diff == 1,
          "3" = new_diff == 2,
          "2" = new_diff == 3,
          "1" = new_diff == 4,
          "0" = TRUE)))
        test_df[j,index + 6] <- get(paste0(i,j))
        test_df[j,index + 7] <- new_diff 
        test_df[j, index + 8] <- score
      }
      
    }
    result_df <- test_df %>% 
      summarise(kevin_total = sum(Kevin_score),
                laura_total = sum(Laura_score),
                jacob_total = sum(Jacob_score),
                josh_total = sum(Josh_score),
                paul_total = sum(Paul_score),
                colin_total = sum(Colin_score))
    return(result_df)
  })
  output$emp_standings <- DT::renderDataTable({
    DT::datatable(tm_standings())
  })
  output$first_place <- shinydashboard::renderValueBox({
    tm_standings <- tm_standings()
    top_score <- max(tm_standings)
    top_person <- as.data.frame(t(tm_standings)) %>%
      filter(V1 == top_score) %>%
      rownames()
    top_person <- stringr::str_to_title(strsplit(top_person, "_")[[1]][1])
    
    valueBox(value = top_score,
             subtitle = top_person,
             color = "yellow")
  })
  
  
  output$live_table_dt <- DT::renderDataTable({
    DT::datatable(live_table,
                  rownames = FALSE,
                  options = list(pageLength = 20,
                                 ordering = FALSE,
                                 paging = FALSE)
                  )
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)
