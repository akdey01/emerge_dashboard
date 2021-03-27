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
citation_scores <- c("All", "No Data", "High", "Medium", "Low")
multi_country <- c("All", "No", "Yes")
short_measure <- c("All", "No", "Yes")

ui <- fluidPage(
    fluidRow(
        column(3,selectInput("psych", 
                "Psychometric Score", choices = psych_scores)),
        column(3,selectInput("cite", 
                "Citation Frequency", choices = citation_scores)),
        column(3,selectInput("multi", 
                "Multi-Country", choices = multi_country)),
        column(3,selectInput("short", 
                "Short Measure", choices = short_measure))
        ),
    fluidRow(
        column(4, plotOutput("word_cloud")),
        column(8, plotOutput("measures_count"))
    ),
    fluidRow(
        column(12, leafletOutput("world_map"))
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
    
    # All plot dataframes 
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
    df_world_map <- reactive({
        getCountrydata(df_selected())
    })
    
    
    # All Outputs
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
    
    
    output$world_map <- renderLeaflet({
        
        # Prepare the text for tooltips:
        mytext <- paste(
            "Country: ", df_world_map()@data$NAME,"<br/>", 
            "Number of measures: ", df_world_map()@data$num_measures, 
            sep="") %>%
            lapply(htmltools::HTML)
        
        # Final Map
        leaflet(df_world_map()) %>% 
            addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 3))  %>% 
            setView(lat=10, lng=0 , zoom=2) %>%
            addPolygons( 
                stroke=FALSE, 
                fillOpacity = 0.5,
                smoothFactor = 0.5,
                color = ~colorNumeric("YlOrRd", num_measures)(num_measures),
                label = mytext,
                labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                )
            ) 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
