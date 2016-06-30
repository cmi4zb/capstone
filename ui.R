
suppressWarnings(library(shiny))

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel('Predict Next Word'),
  
  # Sidebar layout
  sidebarPanel(
    h4 ('Author: Caroline Iurillo'),
    h4('Date: 25-June-2016' ),
    
    
    h4('Summary: This application will predict the next word in a phrase that the user types in.  The N-Gram Back Off model is used to predict the next word. 
      Twitter, Blog, and News data was aggregated and tokenized into 3 data sets which have frequently used individual words (unigram), groupings of 2 words (bigram) and groupings of 3 words (trigram).  
      When the user types in a phrase the application will use the highest ngram that it can in order to predict the next word.
      (the user types in 2 words, so the trigram data set will be used to predict the next word).  The application will return the next 
      word the most frequently occurs after the user entered text.'),
    
      textInput("inputString", "Enter a partial sentence here",value = ""),
      submitButton("Next Word"),
      
      h4('Please Enter a phrase and click the button to see the predicted next word ')
    ),
    
    mainPanel(
      h4('Predicted Next Word'),
      verbatimTextOutput('prediction'),
      textOutput('text1'),
      textOutput('text2')
    )
  )
    )