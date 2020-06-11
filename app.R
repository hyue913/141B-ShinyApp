library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(shinythemes)



pagen <- 1:1000
npictures <- 1:10
oda <- c("desc","asc")

ui <- fluidPage(
    theme = shinytheme("slate"),
    titlePanel("Cat Is Justice"),
    sidebarLayout(
        sidebarPanel(
            selectInput('pn', 'Page Number', c("-",pagen)),
            selectInput('np', 'Number of Pictures Shown(Maximum:10)', "-" ),
            selectInput('or', 'Choose An Order', "-"),
            selectInput('cs', 'Show the N-th Cat', "-")
        ),
        mainPanel(
            uiOutput('cp'),
            div(textOutput('inf')),
            div(textOutput('ht')),
            tags$head(tags$style("#inf{color:red;
                                font-size:40px;
                                font-style:italic;
                                }")),
        )
    )
)

server <- function(input, output, session) {
    cat_photos <- reactive({
        if (input$pn != "-" & input$np != "-" & input$or != "-") { 
            r <- GET("https://api.thecatapi.com/v1/images/search?query",
                     query = list(page=input$pn,limit=input$np,order=input$or),
                     key = Sys.getenv("CAT_KEY"))
            json <- content(r, as = "text", encoding = "UTF-8")
            cats <- fromJSON( json, flatten = TRUE)
            cats
        }
    })
    
    cat_photo <- reactive({
        cat <- cat_photos()
        Us <- cat$url
        if (input$cs != "-") {
            nth <- as.numeric(input$cs)
            U <- Us[nth]
            U
        }
    })
    
    cat_breeds <- reactive({
        cat <- cat_photos()
        Br <- cat$breeds
        if (input$cs != "-") {
            nth <- as.numeric(input$cs)
            B <- Br[[nth]]
            if (!is_empty(B)) {
                breed <- B$name
                inf <- paste("The breed of this cat is:", breed)
                inf
            } else{
                inf <- "The breed of this cat not be indentified! Try another one!"
                inf
            }}    
    })
    
    observeEvent(input$pn, {
        updateSelectInput(session, "np", choices = c("-", npictures))
    })
    
    observeEvent(input$np, {
        if (input$np != "-"){
            num_cat = as.numeric(input$np)
            cat_seq = seq(num_cat)
            updateSelectInput(session, "cs", choices = c("-", cat_seq))
        } else {
            updateSelectInput(session, "cs", choices = "-")
        }
        updateSelectInput(session, "or", choices = c("-", oda))
    })
    
    observeEvent(input$or,{
        if (input$np != "-"){
            num_cat = as.numeric(input$np)
            cat_seq = seq(num_cat)
            updateSelectInput(session, "cs", choices = c("-", cat_seq))
        } else {
            updateSelectInput(session, "cs", choices = "-")
        }
        
    })
    # image output
    output$cp <- renderUI({
        tags$img(src = cat_photo(), width = 500, height = 500)
    })
    # first textoutput
    output$inf <- renderText(
        cat_breeds()
    )
    # second textoutput
    output$ht <- renderText(
        "Hint: The photo collection of cats will be regenerated when
        any of the first three features change! Also, most cats can not be indentified with its breeds,
        so, please keep trying to find its breed!"
    )
    
}


shinyApp(ui = ui, server = server)
