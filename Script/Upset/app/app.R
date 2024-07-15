# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ------------- SHIFT - Upset data and plot ------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script produces the dataframe needed to produce the upset plot (Figure 2) and the plot itself
# For all questions about it, contact Premysl Velek at p.velek@erasmusmc.nl 

# Be careful!! The project structure is slightly different here, as it has to work on its own. That's why there is the sankey data included in the same folder. When running on a local computer it should not make any difference though.


# Load packages and data ---------------------
require(shiny)
require(tidyverse)
require(ggplot2)
require(ComplexUpset)
require(patchwork)
require(colorspace)
require(shinyBS)

rm(list = ls())

# use this if you want to upload the app on a server (eg shinyapp). 
# In this case you have to comment off lines 29!!!
load("upset_data.Rdata")

# use this if you want to run it locally on your computer
load(here::here("Data", "upset_data.Rdata"))


palette <- swatchplot(diverging_hcl(2, "Blue-Red", l1 = 60))

ui <- fluidPage(
        titlePanel("Intersection diagram with patterns of co-occurrence of diseases within one individual"),
        
        fluidRow(
            column(width = 12,
                   helpText("Figure 2 of the article 'Sex-specific patterns and lifetime risk of multimorbidity in the general population: a population-based cohort study'"),
                   
                   helpText("The diagram visualises co-occurrence of diseases as a matrix in which the rows represent the individual diseases and the columns represent their intersections, ie. the different combinations of co-occurring diseases. All diseases that are part of a given combination are shown as black dots connected with a vertical black line. (If a disease is not part of the combination a grey dot is shown.) The number of participants with a given combination of diseases is shown as a vertical bar on top of the matrix, the number of participants with any one disease is shown as a horizontal bar to the left of the matrix."),
                   
                   helpText("The number of participants with a given combination of disease are shown in absolute numbers (bottom) and as a sex ratio for that combination. The black horizontal line in the sex ratio chart represents the sex ratio in the entire study population."),
                   hr(),
            )
        ),
        
        fluidRow(
            column(width = 2,
                   
                   
                   sliderInput(inputId = "min_size", label = "Minimum number of participants with a given combinations of diseases:",
                               min = 1, max = 20, value = 10),
                   sliderInput(inputId = "min_degree", label = "Minimum number of co-occuring diseases:",
                               min = 0, max = 5, value = 1),
                   sliderInput(inputId = "n_intersections", label = "Number of combinations to display:",
                               min = 5, max = 60, value = 20, step = NULL),
                   selectInput(inputId = "condition", label = "Show only combinations containing:",
                               choices = list("All",
                                              "Coronary heart disease",
                                              "Heart failure",
                                              "Stroke",
                                              "Cancer",                       
                                              "Diabetes",
                                              "Parkinsonism",
                                              "Dementia",
                                              "Depression",
                                              "Asthma",
                                              "COPD")),
                   selectInput(inputId = "sort", label = "Sort by:",
                               choices = list("Set size" = "cardinality",
                                              "Degree" = "degree")),
                   selectInput(inputId = "intersection_type", label = "Intersection type:",
                               choices = list("Exclusive intersection" = "exclusive_intersection", 
                                              "Inclusive intersection" = "inclusive_intersection")),
                   
                   bsTooltip(id = "sort", title = "Set size shows combinations with the greatest number of participants first. Degree shows the combinations with the greatest number of diseases first.", 
                             placement = "right", trigger = "hover", 
                             options = list(container = "body")),
                   
                   bsTooltip(id = "intersection_type", title = "Exclusive intersection shows only participants that have a particular combination of diseases and not any additional diseases.", 
                             placement = "right", trigger = "hover",
                             options = list(container = "body"))
                             
                   
                   ),
                   
            
            column(width = 9, 
                   tabsetPanel(
                       tabPanel("All", plotOutput("all")),
                       tabPanel("Men",   plotOutput("men")),
                       tabPanel("Women", plotOutput("women"))
                   )
            )
        )
    )
        

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    get_data <- reactive({
        if(input$condition == "All"){
            return(upset_data)
        } 
        else if(input$condition == "Coronary heart disease"){
            return(upset_data[upset_data$`Coronary heart disease` == TRUE, ])
        }
        else if(input$condition == "Heart failure"){
            return(upset_data[upset_data$`Heart failure` == TRUE, ])
        }
        else if(input$condition == "Cancer"){
            return(upset_data[upset_data$`Cancer` == TRUE, ])
        }
        else if(input$condition == "Depression"){
            return(upset_data[upset_data$`Depression` == TRUE, ])
        }
        else if(input$condition == "Diabetes"){
            return(upset_data[upset_data$`Diabetes` == TRUE, ])
        }
        else if(input$condition == "Parkinsonism"){
            return(upset_data[upset_data$`Parkinsonism` == TRUE, ])
        }
        else if(input$condition == "Dementia"){
            return(upset_data[upset_data$`Dementia` == TRUE, ])
        }
        else if(input$condition == "COPD"){
            return(upset_data[upset_data$`COPD` == TRUE, ])
        }
        else if(input$condition == "Asthma"){
            return(upset_data[upset_data$`Asthma` == TRUE, ])
        }
        else {
            return(upset_data[upset_data$`Stroke` == TRUE, ])
        }
    })

    output$all <- renderPlot({
        upset(get_data(), conditions,  min_size = input$min_size,
              min_degree = input$min_degree, 
              n_intersections = input$n_intersections,
              sort_intersections_by = input$sort,
              height_ratio = 0.6, name = "Combinations of conditions",
              mode = input$intersection_type,
              base_annotations = list(
                  "Intersection size" = intersection_size(
                      mapping = aes(fill = sex),
                      mode = input$intersection_type) +
                      scale_fill_manual(values = c("male" = palette[1], "female" = palette[2]), 
                                        name = NULL)
              ),
              
              stripes = "white",
              themes = upset_default_themes(text = element_text(size = 20)),
              
              annotations =list(
                  'Sex ratio' = list(
                      aes = aes(x = intersection, fill = sex),
                      geom = list(
                          geom_bar(stat = 'count', position = 'fill'),
                          geom_text(aes(label = !!aes_percentage(relative_to = 'intersection')), 
                              stat='count', position = position_fill(vjust = .5)),
                          geom_hline(yintercept = 0.576),
                          scale_y_continuous(labels = scales::percent_format()),
                          scale_fill_manual(values = c("male" = palette[1], "female" = palette[2]),
                                            guide = FALSE)
                      )
                  )
              )) + 
            patchwork::plot_layout(heights = c(0.5, 1, 0.5))
        
    }, height = 800)
    output$men <- renderPlot({
        upset(get_data() %>% dplyr::filter(sex == "male"), conditions,  min_size = input$min_size,
              min_degree = input$min_degree, 
              n_intersections = input$n_intersections,
              sort_intersections_by = input$sort,
              height_ratio = 0.6, name = "Combinations of conditions",
              mode = input$intersection_type,
              base_annotations = list(
                  "Intersection size" = intersection_size(
                      mapping = aes(fill = sex),
                      mode = input$intersection_type) +
                      scale_fill_manual(values = c("male" = palette[1], "female" = palette[2]), 
                                        name = NULL)
              ),
            
              stripes = "white",
              themes = upset_default_themes(text = element_text(size = 20))) + 
            patchwork::plot_layout(heights = c(1, 0.5))
    }, height = 600)
    output$women <- renderPlot({
        upset(get_data() %>% dplyr::filter(sex == "female"), conditions,  min_size = input$min_size,
              min_degree = input$min_degree, 
              n_intersections = input$n_intersections,
              sort_intersections_by = input$sort,
              height_ratio = 0.6, name = "Combinations of conditions",
              mode = input$intersection_type,
              base_annotations = list(
                  "Intersection size" = intersection_size(
                      mapping = aes(fill = sex),
                      mode = input$intersection_type) +
                      scale_fill_manual(values = c("male" = palette[1], "female" = palette[2]), 
                                        name = NULL)
              ),
              stripes = "white",
              themes = upset_default_themes(text = element_text(size = 20))) + 
            patchwork::plot_layout(heights = c(1, 0.5))
    }, height = 600)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
