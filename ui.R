library(shiny)


shinyUI(pageWithSidebar(
  headerPanel("Text Predictor - Capstone project"),
  sidebarPanel(
    h3("User Input"),
    br(),
    
    strong(""),
    textInput("text1", "Enter a phrase below:", value = "although it is possible i knew that"),
    br(),

    selectInput("words1", "Maximum predicted words to return",
                choices = list("1" = 1, "2" = 2,
                               "3" = 3, "4" = 4,
                               "5" = 5), selected = 5),
    br(),
    
    tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")), 
    
    br(),
    
    actionButton("button1", "Predict")
    
    
  ),
  mainPanel(
    tabsetPanel(
      
      tabPanel("Results",
               
             textOutput("table1")
               
      ),
      
      tabPanel("Documentation",
               h4("Description"),
               p("A simple text predictor model has been created. Type the phrase in the input box, 
                 and select the number of predictions you want. On clicking the predict button, 
              predictions are provided in the reults." ),
               
               br(),
               
               h4("How?"),
               p("Using a section of blogs, news and twits, common 2-grams and 3-grams were deduced. 
                 The predictor compares the last two words of the given phrase to the priors of either 
                 3-grams or 2-grams and returns suggestions. When there is no match against the n-grams, 
                 most commonly occurring words are returned."),
              
               br(),
               
               h4("Effectiveness"),
               p("Obviously, the model is not very effective since it only compares the last 2 
                 words of the sentence. On extracting more n-grams with very high frequency, the 
                 prediction can be better. This hasn't been done in the present owing to 
                 memory requirements and time constraints to finish the project."),
             
               br(),
               
               h4("What other improvements?"),
               p("The prediction algorithm used here is straightforward.
                 Probability of occurrence of the last word in each n-gram is computed. 
                 The prediction results are then obtained by comparing the (n-1)gram with 
                 the prior of each (n)gram arranged in the decreasing order of their probabilities.")
             
               )
      ))
))