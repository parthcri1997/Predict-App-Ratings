library(shinydashboard)
library(shinyalert)
library(reshape2)
library(caret)
library(randomForest)
library(shinyjs)


final_data = read.csv("Final Cleaned Data.csv")

ui <- dashboardPage(
  
  skin = "yellow",
  
  dashboardHeader(title = "App Ratings "),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      selectInput(inputId = "category_parameter", final_data$Category,label = "Select the App Category",choices =final_data$Category,width = "100%"),
      
      numericInput(inputId ="size_parameter", label = "Enter the Size of App in KB", value = "", min = 0, max = NA, step = NA,width = NULL),
      
      numericInput(inputId ="install_parameter", label = "Enter the No. of Installs of Apps", value = "", min = 0, max = NA, step = NA,width = NULL),
      
      sliderInput(inputId = "polarity_parameter", "Polarity :",min = -1, max = 1,value = 0.4, step = 0.1),
      
      numericInput(inputId ="reviews_parameter", label = "Enter the No. of Reviews of Apps", value = "", min = 0, max = NA, step = NA,width = NULL),
      
      selectInput(inputId = "appType_parameter",final_data$Type,label = "Select the Type of App",choices = final_data$Type),
      
      conditionalPanel(condition = "input.appType_parameter == 'Paid' ",
                       
                       numericInput(inputId ="price_parameter", label = "Enter the Price of Apps", value = 1, min = 0, max = NA, step = NA,width = NULL)
                       
      ),
      
      numericInput(inputId ="days_parameter", label = "Enter the No. of Days till Last Update", value = "", min = 0, max = NA, step = NA,width = NULL),
      
      selectInput(inputId = "contentRating_parameter", final_data$Content_Rating,label = "Select Content Rating",choices = final_data$Content_Rating ),
      
      useShinyalert()
    ),
    

    actionButton("Gobutton", "Submit")
    
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML(' .content-wrapper, .right-side {
                              background-color: white;
                              }           
                              
                              '))),
    
    fluidRow(
      
      tags$head(tags$style(".shiny-output-error{color: white;}"))
    
    ),
    
    br(),br(),
    
    fluidRow(
      
      infoBoxOutput("AppCategory"),
      
      infoBoxOutput("size"),
      
      infoBoxOutput("Installs"),
      
      infoBoxOutput("Polarity"),
      
      infoBoxOutput("slider_rangeInput"),
      
      infoBoxOutput("Reviews"),
      
      infoBoxOutput("Type"),
      
      infoBoxOutput("Price"),
      
      infoBoxOutput("Days"),
      
      infoBoxOutput( "contentrating")
      
    )
    )
  
    )

server <- function(input, output,session) {
  
  ##########  selected parameter display      #########                
  
  output$AppCategory<- renderPrint({
    
    
    infoBox(title = "Type of App Category", icon = icon("android"), width = 15,
            fill = TRUE,value = input$category_parameter,color = "teal")
    
  })
  
  output$size <- renderPrint({
    
    infoBox(title = "Size of App", icon = icon("sort-numeric-up"), width = 15,
            fill = TRUE,value =input$size_parameter,color = "purple"
    )
    
  })
  
  output$Installs <- renderPrint({
    
    
    infoBox(title = "No. of Install",icon = icon("download"),width = 15, 
            fill = TRUE,value =input$install_parameter ,color = "olive" )
    
    
  })
  
  output$Polarity <- renderPrint({
    
    infoBox(title = "Polarity",icon = icon("poll"), width = 15,
            fill = TRUE,value = input$polarity_parameter,color = "green")
    
  })
  
  output$Reviews <- renderPrint({
    
    infoBox(title = "No. of Reviews", icon = icon("comments"), width = 15,
            fill = TRUE,value =input$reviews_parameter,color = "navy"
    )
    
  })
  
  output$Type <- renderPrint({
    
    infoBox(title = "Type of App",icon = icon("money-bill-alt"),width = 15, 
            fill = TRUE,value = input$appType_parameter,color = "light-blue" )
    
  })
  
  output$Price <- renderPrint({
    
    infoBox(title = "Price of App",icon = icon("rupee-sign"),width = 15, 
            fill = TRUE,value = ifelse(input$appType_parameter == 'Paid',input$price_parameter,0 ),color = "maroon" )
    
  })
  
  output$Days <- renderPrint({
    
    infoBox(title = "Days till Last Updated ",icon = icon("calendar-alt"),width = 15, 
            fill = TRUE,value =input$days_parameter ,color = "aqua" )
    
  })
  
  output$contentrating <- renderPrint({
    
    infoBox(title = "Content Rating ",icon = icon("users"),width = 15, 
            fill = TRUE,value =input$contentRating_parameter ,color = "olive" )
    
  })
  
  ######   slider parameter  ######
  
  output$slider_rangeInput <- renderUI({
    
    observeEvent(input$polarity_parameter, {
      
      if(input$polarity_parameter!=0.4){
        
        shinyalert("Message !", "Never Think Negative : We have default consider 0.4 because it is average of Positive Polarity.")
        
      }
    })
    
    if(input$polarity_parameter > 0 & input$polarity_parameter <= 1){
      
      
      infoBox(
        "Sentiment Type", paste("Positive"), width = 15, fill = TRUE,icon = icon("credit-card"),
        color = "yellow"
      )
      
      
    }
    
    else if(input$polarity_parameter >= -1 & input$polarity_parameter <= -0.1)
    {
      
      infoBox(
        "Sentiment Type", paste("Negative"), width = 15, fill = TRUE,icon = icon("poll"),
        color = "yellow"
      )
      
      
    }
    else{
        infoBox(
            "Sentiment Type", paste("Neutral"), width = 15, fill = TRUE,icon = icon("credit-card"),
        color = "yellow"
      )  
      
      
    }
  })
  
  ######   On submit button ######
  
  observeEvent(input$Gobutton, {
    
    if(is.numeric(input$size_parameter) & is.numeric(input$install_parameter) & is.numeric(input$reviews_parameter)& is.numeric(input$days_parameter) )
    {
      
      #### For Size can't be 0 ####     
      
      observeEvent(input$size_parameter,{
        
        if(input$size_parameter == 0){
          
          shinyalert("Warning!", "Size of an App can't be 0")
        }
        
        else{
          
          #### No. of Install can't be 0 ####  
          
          observeEvent(input$install_parameter, {
            
            if(input$install_parameter == 0){
              
              shinyalert("Warning!", "No. of Install can't be 0")
              
            }
            else   
            {
              
              
              ######   Final Dialog Box ######
              
              showModal(modalDialog(
                
                title = "Predicted Ratings of an App",easyClose = TRUE )
                
              )
            }
          })
        }
        
      })
    }
    
    else
    {
      shinyalert("Alert!", "please enter numeric values", type = "error")
      
    }	
  })
}

shinyApp(ui, server)