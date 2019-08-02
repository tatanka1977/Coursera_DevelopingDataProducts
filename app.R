#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(janitor)
library(plotly)
library(zoo)
df <- read.csv2("./2018_EconomicFreedom.csv")
df <- df[,1:9]
#df <- df[df$Year==2016,]

colnames(df) <- str_remove_all(colnames(df),'X[:digit:][:punct:]{2}')
colnames(df) <- str_replace_all(colnames(df),'[:punct:]'," ")
colnames(df) <- str_replace_all(colnames(df),'   ',' and ')
colnames(df) <- gsub('\\s+', '', colnames(df))
df <- remove_empty(df, which = c("rows", "cols"))

Continents <- read.csv("~/GitHub/Coursera_DevelopingDataProducts/Rshine/myFirstShiny/Continents.txt")
Continents <- Continents[,c(3,6:8)]

df <- merge(df,Continents,by.x="ISOCode",by.y="alpha.3")
df$ISOCode <- df$ISOCode %>% as.factor()
df$Countries <- df$Countries %>% as.factor()
df$region <- df$region %>% as.factor()
df$sub.region <- df$sub.region %>% as.factor()
df$Year <- as.Date(as.yearmon(df$Year) + 11/12, frac = 1)

choices_country <- unique(as.character(df$Countries))
choices_country <- setNames(choices_country,choices_country)
choices_year <- df$Year %>% unique() %>% sort() 
choices_year <- setNames(choices_year,year(choices_year))

pal <- c("red", "blue", "green")
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Economic world data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("year", label = "Select year", choices = choices_year),
            actionButton(inputId = "submit_loc",
                         label = "Submit"),
            # Copy the chunk below to make a group of checkboxes
            prettyCheckboxGroup(
                "checkGroup",
                label = h3("Checkbox group"),
                choices = choices_country,
                shape = "round"
            )
        ),
        
        
        # Show a plot of the generated distribution h3("Summary index map"),
        mainPanel(
            tabsetPanel(
                tabPanel("Lat year map", plotlyOutput("Plotly")),
                tabPanel("Summary Index Trend", plotlyOutput("scatter")),
                type = "pills"
            )
        )
    )
)


server <- function(input, output) {
    
    observeEvent(
        eventExpr = input[["submit_loc"]],
        handlerExpr = {
            dataset <- df %>% filter(Countries==input$checkGroup)  
            dataset_2 <- dataset %>% filter(Countries==input$year)
            output$Plotly <- renderPlotly({
                plot_geo(filter(group_by(dataset,Countries),Year==max(Year)), locationmode = 'world') %>% 
                    add_trace(
                        z = ~SUMMARYINDEX, locations = ~ISOCode,
                        color = ~SUMMARYINDEX, colors = c("green","blue") 
                    )})
            
            output$scatter <- renderPlotly({
                dataset %>% 
                    mutate(onlyyear = year(Year)) %>% 
                    group_by(Countries) %>% 
                    plot_ly(x = ~ onlyyear ) %>% 
                    add_lines(y = ~ SUMMARYINDEX , 
                              color = ~ factor(Countries)) %>% 
                    layout(xaxis=list(title = "Year"),
                           yaxis=list(title = "Summary index"))})

        })
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
