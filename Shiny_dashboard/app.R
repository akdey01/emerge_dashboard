#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(memoise)
library(tm)
library(wordcloud)

# UI stuff

psych_scores <- c("All", "No Data", "High", "Medium", "Low")

ui <- fluidPage(
    fluidRow(
        column(6,
               selectInput("psych", "Psychometric Score", choices = psych_scores)
        )
    ),
    fluidRow(
        column(6, plotOutput("word_cloud")),
        column(6, plotOutput("measures_count"))
    ),
    fluidRow(
        column(12, plotOutput("world_map"))
    )
)
    
# Server stuff
server <- function(input, output) {
    
    ## Create the dataframe for plots
    df_selected <- reactive({
        
        if(input$psych == "All") {res <- df_measures} 
            else {res <- df_measures %>% filter(psych_score == input$psych)}
        
        res       
    })
    
    ## Create dataframe for wordcloud
    df_wordcloud <- reactive({
        getTermMatrix(df_selected())
    })
    
    ## Create dataframe for barplot
    df_barplot <- reactive({data.frame("Dimensions" = c("Psychological \nEmpowerment",
                                              "Social \nEmpowerment",
                                              "Education",
                                              "Legal",
                                              "Political",
                                              "Household / \nIntrafamilial \nRelations",
                                              "Environment and \nSustainability",
                                              "Time Poverty",
                                              "Gender \nBased \nViolence",
                                              "Women's \nEconomic \nEmpowerment",
                                              "Health",
                                              "Maternal and \nChild Health",
                                              "SRH and \nFP",
                                              "All Measures"),
                             "num_measures" = c(unlist(df_selected() 
                                                       %>% select(starts_with("dim")) 
                                                       %>% map(~sum(!is.na(.)))),
                                                nrow(df_selected())))})
    
    
    ## Create dataframe for chloropleth
    
    
    output$word_cloud <- renderPlot({
        v <- df_wordcloud()
        wordcloud(words = v$word, freq = v$freq, min.freq = 1,
                  max.words=150, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
    })
    
    output$measures_count <- renderPlot({
        df_barplot() %>% 
            ggplot(aes(fct_reorder(Dimensions, num_measures) , num_measures)) + 
            geom_bar(stat = "identity", width = 0.9, fill = "#157067") + 
            geom_text(aes(label = num_measures), size = 3, vjust = -0.5, color = "black") +
            xlab("Dimensions") + 
            ylab("Number of measures on EMERGE") + 
            theme_minimal()
    }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
