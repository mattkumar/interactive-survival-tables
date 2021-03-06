###Author : Matt
###Date   : 2020-09-20
###Purpose: The shiny app displaying the interactive survival tables

#load libs
library(tidyverse)
library(shiny)
library(survminer)
library(DT)
library(sparkline)
library(shinythemes)

#load in pre-computed data
#the script that produced this data is in /code/01_data_prep.R
load(here::here('data', 'pre_computed_data.Rdata'))

#define ui
ui    <- fluidPage( 
                    theme = shinytheme("flatly"),
                    
                    titlePanel("Survival Tables"),
                    
                    fluidRow(
                      column(2,
                             wellPanel(
                               selectInput('option','Select Analysis:', choices=c('Kaplan Meier'=1,'Cumulative Events'=2))
                             )       
                      ),
                      column(5,
                             h3(textOutput("plot_title")),
                             br(),
                             plotOutput('km', height="470", width="675"),
                             h6(textOutput("header")),
                             DT::dataTableOutput('summary_tab', width="700")
                      ),
                      column(5,
                             h3("Drill Down Table"),
                             br(),
                             DT::dataTableOutput('drill_tab', width="650")
                             
                      )
                    )
                  )
                  
#define server
server <- function(input, output) {
  
  #Define Reactive Values
  values <- reactiveValues()
  
  #Store table data source, table title, plot title and plot options, based on user analysis selected 
  observeEvent(input$option,{
    if (input$option == 1) {
      values$data        <- nar_summary_table
      values$plot_title  <- "Kaplan Meier Plot"
      values$plot_type   <- NULL
      values$table_title <- "Number at Risk"
    } else if (input$option == 2) {
      values$data       <- eve_summary_table  
      values$plot_title <- "Cumulative Events Plot"
      values$plot_type   <- "event"
      values$table_title <- "Number of Events"
    }
  })
  
  
  #Plot Title
  output$plot_title <- renderText(values$plot_title)

  #Plot
  output$km <- renderPlot(ggsurvplot(sfit, conf.int = TRUE, palette=c("#82b1ff","#7c4dff"), fun = values$plot_type)) 

  #Summary Table Title
  output$header <- renderText(values$table_title)
 
  #Summary Table
  output$summary_tab <- renderDataTable(
                                     datatable(values$data,
                                                 #Make individual cells selectable, one at a time only. Initialize the selected cells.
                                                 selection = list(mode = 'single', target = 'cell', selected= matrix(c(1,2),ncol=2)),
                                                 rownames  = FALSE,
                                                 #Remove col names to give the illusion the table is part of the figure
                                                 colnames  = rep("", ncol(values$data)),
                                                 options   = list(dom = 't',
                                                                  columnDefs = list(
                                                                                   list(className = 'dt-center', targets = '_all'),
                                                                                   list(visible=FALSE, targets=c(0))))) %>%
                                     formatStyle(
                                         'Treatment',
                                          target = 'row',
                                          color = styleEqual(c("Body Cleansing", "Routine Bath"), c("#82b1ff","#7c4dff")))
                                      )


#Subset the original patient level data (e.g. burn_1) to create the drill down data (e.g. drill_data()), based on user selection in output$summary_tab
drill_data <- reactive({
                        #Require selection by user before doing anything
                        req(input$summary_tab_cells_selected)
    
                        #Retrieve the selected cell coordinates
                        row_coord <- as.integer(input$summary_tab_cells_selected[1])
                        col_coord <- as.integer(input$summary_tab_cells_selected[2])
                        
                        #Note: it's helpful to print these to the console to get an idea of the coordinates
                        #print(row_coord)
                        #print(col_coord)
                     
                        
                        #Remove variables not used, based on what was selected.
                        #Keeping them in interfere with the position coordinates in the next chunk.
                        #e.g. removing the unused ones lets us use the same selection logic on line 120
                        if(input$option==1) {
                            drill_filtered <- burn_1 %>%
                                                select(-starts_with("event_"))
                        } else {
                            drill_filtered <- burn_1 %>%
                                                select(-starts_with("risk_"))
                        }
                 
                        
                        #Using the coordinates, subset the original patient level data
                        drill_filtered <- drill_filtered %>%
                            
                                          #Here we specify what variables we want to see in the drill down - I only want a few clinical ones
                                          #The last variable is always the indicator variable, and is based on the col_coord
                                          select(c(ID, Treatment, Gender, Race, Type, Head, `Swimmer Plot` = sparkline, col_coord+10)) %>%
                                          
                                          #Filter the last variable (i.e. the indicator) to be equal to 1
                                          filter(eval(parse(text=colnames(.)[ncol(.)])) == 1) 
                        
                        
                        #Final filtering on rows
                        if(row_coord == 1) {
                            drill_filtered <- drill_filtered %>% 
                                                filter(Treatment == "Body Cleansing") %>%
                                                select(-c(Treatment, starts_with("event"), starts_with("risk")))
                        } else {
                            drill_filtered <- drill_filtered %>% 
                                                filter(Treatment == "Routine Bath")%>%
                                                select(-c(Treatment, starts_with("event"), starts_with("risk")))
                            
                        }
})


#Show the drilled down data
output$drill_tab <- renderDataTable(DT::datatable(drill_data(),
                                                    escape = FALSE, 
                                                    rownames = FALSE,
                                                    filter = 'top',
                                                    options = list(pageLength = 10,
                                                                   lengthChange = FALSE,
                                                                   #The fnDrawCallback logic borrowed - https://stackoverflow.com/questions/45179410/add-label-to-sparkline-plot-in-datatable
                                                                   fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
                                                                   columnDefs = list(list(className = 'dt-center', targets = '_all'))))
                                                   %>% 
                                                        spk_add_deps())



}

#Run the app
shinyApp(ui = ui, server = server)
