#Load libraries
library(shiny)
library(wordcloud)
library(data.table)
library(DT)
#Load the ngramized text
ngram_frequency_table = fread('ngram_frequency_table_above1.csv')


shinyServer(function(input, output,session){

  
#In the ngramized data search for the word the user has placed and find the best match    
  best_matches_table <- reactive({
    #find the last word the user has placed
    my_sentenced_parsed = unlist(strsplit(input$mysentence,split=' '))
    word_to_match = tolower(my_sentenced_parsed[length(my_sentenced_parsed)])
    matches = c()
    matches_freq = c()
    #Search for matches in the data
    for (i in 1:nrow(ngram_frequency_table)) {
      total_freq = sum(ngram_frequency_table$Freq)
      if (word_to_match == ngram_frequency_table[i,]$ngram_first_component) {
        matches = c(matches, ngram_frequency_table[i,]$ngram_second_component)
        matches_freq = c(matches_freq,(ngram_frequency_table[i,]$Freq/total_freq)*100)
      }
      #stop the algorithm if we get the necessary numbe of matches or if we have searched 
      #through one third of the data and no match has been found
      if(length(matches)==input$num_matches|i >= (nrow(ngram_frequency_table)/3)) break
    }
    #construct the table
    best_matches_table = as.data.frame(cbind(matches,matches_freq))
  })
  
#find the best word by taking the first match
  output$most_probable_word <- renderText({
    temp = best_matches_table()
    #add false save if there is no match 
    if(nrow(temp)==0){
      most_probable_word = 'Sorry, we have no record of such word. 
      Go to "Give your input" tab to give us your suggestion.'
    } else most_probable_word=as.character(temp$matches)[1]
    most_probable_word
    
  })
#create a wordcloud plot  
  output$wc <- renderPlot({
  temp = best_matches_table()
  #add false save if there is no match
  if(nrow(temp)==0){
    print('No graph available for this input.')
  }else{
    wordcloud(temp$matches, as.numeric(as.character(temp$matches_freq)), 
              scale=c(6, .1),colors=brewer.pal(6, "Accent"))
  }

    })
#display the best matches table in order to ask the user which is the one what they asked for  
  output$best_matches_table <- DT::renderDataTable(DT::datatable({
    temp = best_matches_table()
    temp
  })
  )
  
#message after hitting the action button
  observeEvent(input$do, {
    js_string <- 'alert("Thank you for clicking");'
    session$sendCustomMessage(type='jsCode', list(value = js_string))
  })

  

})



