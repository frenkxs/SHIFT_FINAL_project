# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT - Interactive sankey  ------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script produces the shiny app for the interactive Sankey plot. 
# For all questions about it, contact Premysl Velek at p.velek@erasmusmc.nl 

# Be careful!! The project structure is slightly different here, as it has to work on its own. That's why there is the sankey data included in the same folder. When running on a local computer it should not make any difference though.

# Load packages and data ---------------------
require(shiny)
require(networkD3)
require(tidyverse)

rm(list = ls())

# use this if you want to upload the app on a server (eg shinyapp)
load("sankey_data.Rdata")

# use this if you want to run it locally on your computer
load(here::here("Data", "sankey_data.Rdata"))

sankey_data <- function(df){
  # A node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name = c(df$from, df$to)) %>% unique
  
  # order nodes so that censored participants are on top
  nodes$order <- 100
  nodes$order[grepl("Disease free", nodes$name, fixed = TRUE)] <- c(1, 2, 3)
  nodes$order[nodes$name == "Depression_1"] <- 4
  nodes$order[nodes$name == "One disease_2"] <- 5
  nodes$order[nodes$name == "One disease_3"] <- 6
  nodes$order[nodes$name == "Cancer_1"] <- 7
  nodes$order[nodes$name == "Depression_2"] <- 8
  nodes$order[nodes$name == "Two diseases_3"] <- 9
  nodes$order[grepl("Died", nodes$name, fixed = TRUE)] <- c(110, 200, 300)
  
  nodes <- arrange(nodes, order)
  
  # With networkD3, connection must be provided using id, not real names => assign ids to nodes
  df$IDfrom = match(df$from, nodes$name) - 1 
  df$IDto =   match(df$to, nodes$name) - 1
  
  nodes$name <- substr(nodes$name, 1, nchar(nodes$name) - 2)
  
  list(df, nodes)
}


# Server -------------
server <- function(input, output) {   
  df <- list()
  
  get_data <- reactive({
    
    if(input$checkbox == FALSE) {
      df[1:2] <- sankey_data(df_final)
      df[3:4] <- sankey_data(df_final_m)
      df[5:6] <- sankey_data(df_final_f)
    }
    else {
      df[1:2] <- sankey_data(df_final_no_zeros)
      df[3:4] <-   sankey_data(df_final_m_no_zeros)
      df[5:6] <-   sankey_data(df_final_f_no_zeros)
    }
    df
  })
  
  
  output$all <- renderSankeyNetwork({
    sankeyNetwork(Links = get_data()[[1]], Nodes = get_data()[[2]],
                  Source = "IDfrom", Target = "IDto",
                  Value = "n", NodeID = "name", fontFamily = "Corbel",
                  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20c);"),
                  fontSize = 15, nodePadding = 8, iterations = 0)
  })
  
  output$men <- renderSankeyNetwork({
    sankeyNetwork(Links = get_data()[[3]], Nodes = get_data()[[4]],
                  Source = "IDfrom", Target = "IDto",
                  Value = "n", NodeID = "name", fontFamily = "Corbel",
                  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20c);"),
                  fontSize = 15, nodePadding = 8, iterations = 0)
    
  })
  
  output$women <- renderSankeyNetwork({
    sankeyNetwork(Links = get_data()[[5]], Nodes = get_data()[[6]],
                  Source = "IDfrom", Target = "IDto",
                  Value = "n", NodeID = "name", fontFamily = "Corbel",
                  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20c);"),
                  fontSize = 15, nodePadding = 8, iterations = 0)
  })
  
}

# UI -------------

ui <- shinyUI(fluidPage(
  
  
  titlePanel("Disease trajectories from single disease to multimorbidity"),
  
  fluidRow(
    column(width = 12,
           helpText("Figure 1 of the article 'Sex-specific patterns and lifetime risk of multimorbidity in the general population: a population-based cohort study'"),
           hr(),
    )
  ),
  

  fluidRow(
    column(width = 3,
      helpText("The columns represent the diagnosis of first three non-communicable diseases in chronological order (from left to right). The height of the columns and the thickness of the stripes are proportional to the number of people with a particular disease."),
      
      helpText("Neurodegenerative diseases include parkinsonism and dementia, heart diseases include coronary heart disease and heart failure, lung diseases include COPD and asthma."),
      hr(),
      checkboxInput('checkbox', 'Hide people with no disease', FALSE)
    ),
    column(width = 9, 
           tabsetPanel(
             tabPanel("All", sankeyNetworkOutput("all")),
             tabPanel("Men", sankeyNetworkOutput("men")),
             tabPanel("Women", sankeyNetworkOutput("women"))
             )
           )
    ),
  )
  )
  

# Run -------------
shinyApp(ui = ui, server = server)
