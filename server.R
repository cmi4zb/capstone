
#load data sets of unigrams, bigrams and trigrams
load("fDF1.RData");
load("fDF2.RData");
load("fDF3.RData");

#load packages 
(library(tm))
(library(stringr))
(library(shiny))


#function to clean text input
CleanInputString <- function(inputString)
{
  
  
  #Convert input to a courpus
  clean1<- VCorpus(VectorSource(inputString))
  
  # Clean by Converting to lower case, remove punctuations, numbers, white spaces, non letters
  clean1 <- tm_map(clean1, content_transformer(tolower))
  clean1 <- tm_map(clean1, removePunctuation)
  clean1 <- tm_map(clean1, removeNumbers)
  clean1 <- tm_map(clean1, stripWhitespace)
  inputString <- as.character(clean1[[1]])
  inputString <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", inputString)
  
  # Return the cleaned words
  # If cleaning removes everything, return empty string
  if (nchar(inputString) > 0) {
    return(inputString); 
  } else {
    return("");
  }
}
####
###use back off method to predict next word.

PredNextTerm <- function(inputString)
{
  assign("mesg", "in PredNextTerm", envir = .GlobalEnv)
  
  # Grab cleaned string
  inputString <- CleanInputString(inputString);
  
  # use white space to determine individual words
  inputString <- unlist(strsplit(inputString, split=" "));
  #count the number of words
  inputStringLen <- length(inputString);
  
  #set defaults
  nxtTermFound <- FALSE;
  predNxtTerm <- as.character(NULL);
  
  #trigram: 2 words or more prediction  
  #if 2 words or more are typed in use the 3gram data set   
  
  if (inputStringLen >= 2 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    #paste concatenate vectors 
    inputString1 <- paste(inputString[(inputStringLen-1):inputStringLen], collapse=" ");
    
    # Subset the Three Gram data frame 
    searchStr <- paste("^",inputString1, sep = "");
    #grep searches for matches to argument pattern within each element of a character vector
    fDF3Temp <- fDF3[grep (searchStr, fDF3$Word), ];
    
    # Check to see if any matching record returned
    if ( length(fDF3Temp[, 1]) > 1 )
    {
      #predNxtTerm returns the top result for the matches found
      predNxtTerm <- fDF3Temp[1,1];
      nxtTermFound <- TRUE;
      mesg <<- "Next word is predicted using 3-gram."
    }
    #fDF3Temp <- NULL;
    return (fDF3Temp)
  }
  
  if (inputStringLen >= 1 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inputString1 <- inputString[inputStringLen];
    
    # Subset the Two Gram data frame 
    searchStr <- paste("^",inputString1, sep = "");
    fDF2Temp <- fDF2[grep (searchStr, fDF2$Word), ];
    
    # Check to see if any matching record returned
    if ( length(fDF2Temp[, 1]) > 1 )
    {
      predNxtTerm <- fDF2Temp[1,1];
      nxtTermFound <- TRUE;
      mesg <<- "Next word is predicted using 2-gram.";
    }
    #fDF2Temp <- NULL;
    return (fDF2Temp)
  }
  
  if (!nxtTermFound & inputStringLen > 0)
  {
    predNxtTerm <- fDF1$Word[1];
    mesg <- "No next word found, the most frequent word is selected as next word."
  }
  
  nextTerm <- word(predNxtTerm, -1);
  
  if (inputStringLen > 0){
    dfTemp1 <- data.frame(nextTerm, mesg);
    return(dfTemp1);
  } else {
    nextTerm <- "";
    mesg <-"";
    dfTemp1 <- data.frame(nextTerm, mesg);
    return(dfTemp1);
  }
}


##server info


shinyServer(function(input, output) {
  output$prediction <- renderPrint({
    str2 <- CleanInputString(input$inputString);
    strDF <- PredNextTerm(str2);
    input$action;
    msg <<- as.character(strDF[1,2]);
    cat("", as.character(strDF[1,1]))
    cat("\n\t");
    cat("\n\t");
    cat("Count of times predicted phrase was found in documents ", as.character(strDF[1,2]));
  })
  
  output$text1 <- renderText({
    paste("Input Sentence: ", input$inputString)});
  
  output$text2 <- renderText({
    input$action;
    
  })
}
)

