
library(shiny)

ui <- fluidPage(title = "Exploring rent control effects",
                tabsetPanel(              
                    tabPanel(title = "Displacement Risk",
                             verbatimTextOutput("risk"),
                             actionButton(inputId = "go1", 
                                          label = "update"),
                             selectInput (inputId = "countySelection1",
                                          label = "select a county",
                                          choices = c("Alameda County", "Contra Costa County",
                                                      "Marin County", "Napa County",
                                                      "San Francisco County", "San Mateo County",
                                                      "Santa Clara County", "Sonoma County",
                                                      "Solano County", "All counties")),
                             selectInput(inputId = "Year1",
                                         label = "select a year",
                                         choices = c(1990, 2000, 2017))
                    ),
                    tabPanel(title = "affordability in 2017",
                             verbatimTextOutput("afford"),
                             actionButton(inputId = "go2", 
                                          label = "update"),
                             selectInput (inputId = "countySelection2",
                                          label = "select a county",
                                          choices = c("Alameda County", "Contra Costa County",
                                                      "Marin County",
                                                      "San Francisco County", "San Mateo County",
                                                      "Santa Clara County", "Sonoma County",
                                                      "Solano County")),
                             selectInput(inputId = "burdenLevel",
                                         label = "select burden level",
                                         choices = c(1,2,3))
                    ),
                    tabPanel(title = "Housing Production",
                             plotOutput("production"),
                             actionButton("go3", "update graph"),
                             selectInput (inputId = "countySelection3",
                                          label = "select a county",
                                          choices = c("All Counties", "Alameda County", "Contra Costa County",
                                                      "Marin County", "Napa County",
                                                      "San Francisco County", "San Mateo County",
                                                      "Santa Clara County", "Sonoma County",
                                                      "Solano County")),
                             selectInput(inputId = "year4",
                                         label = "start year",
                                         choices = c(1990:2000)),
                             selectInput(inputId = "year5",
                                         label = "end year",
                                         choices = c(1994:2018))
                    )
                )
)

server <- function(input, output) {
    
    #create reactive variables with initial values
    risky1 <- reactiveValues(mean_risk = "Here is your answer")
    afford1 <- reactiveValues(burden_share = "Here is your answer")
    produce1 <- reactiveValues(total = NULL)
    
    #tab 1 
    observeEvent(input$go1, {risky1$mean_risk <- if(input$countySelection1 == "All counties"){
        mean((Vital_Signs_Displacement_Risk_by_county %>%
                  filter(Year == input$Year1))$Displacement_Risk)
        
    }else {mean((Vital_Signs_Displacement_Risk_by_county %>%
                     filter(County == input$countySelection1) %>%
                     filter(Year == input$Year1))$Displacement_Risk)}
    })
    
    #tab 2
    observeEvent(input$go2, {afford1$burden_share <- if(input$countySelection2 == "All counties"){
        mean((vs_affordability_recode %>% filter(Year == 2017) %>% 
                  filter(Housing_Burden2 == input$burdenLevel))$Share)
        
    }else {mean((vs_affordability_recode %>% filter(Geography == input$countySelection2)
                 %>% filter(Year == 2017) %>%
                     filter(Housing_Burden2 == input$burdenLevel))$Share)}
    })
    
    #tab 3
    observeEvent(input$go3, {
        produce1$total <- if(input$countySelection3 == "All Counties"){
            Vital_Signs_Housing_Production_by_county %>%
                filter(between(Year, input$year4, input$year5))
        }
        
        else{Vital_Signs_Housing_Production_by_county %>%
                filter(County == input$countySelection3) %>%
                filter(between(Year, input$year4, input$year5))
        }
    })
    
    
    output$risk <- renderPrint({
        risky1$mean_risk
    })
    
    output$afford <- renderPrint({
        afford1$burden_share
    })
    
    output$production <- renderPlot({
        ggplot(produce1$total, aes(x = produce1$total$TOTALproduction)) + geom_histogram(aes(y = ..density..),
                                                                                         binwidth = 250) + geom_density(alpha = .2, fill = "#FF6666")
    })
}

shinyApp(server = server, ui = ui)