
################################################################################
#### This file runs the NGram algorithm to predict your next word
################################################################################
#Load libraries
library(ngram)

## Data
#Load the files
temp1 = read.table('en_US.news.txt',sep = '\n')
temp2 =read.table('en_US.blogs.txt',sep ='\n')
#merge them
temp = rbind(temp1,temp2)
remove(temp1,temp2)

## Create from a corpus with article documents, corpus from sentences. 

#finction to parse articles into sentences
parse_sentance = function(text){
  #note the text is in a data frame format with a lot of raws and one column  
  sentenced_text = c()
  for(r in 1:nrow(text)){
    print((r/nrow(text))*100)
    sentenes = unlist(strsplit(as.vector(text[r,1]),split = '[.]|[!]|[?]'))
    sentenes = sentenes[nchar(sentenes)>0]#taka care of the empty sentences
    sentenced_text = c(sentenced_text,sentenes)
  }
  return(as.data.frame(sentenced_text))
}

#parse the data into sentences
temp_sentenced = parse_sentance(temp)


## Pre-process the data

#clean the text function 
clean_text = function(text) {
  # remove 's from all words 
  text = gsub(pattern="'s", x=text, replacement = ' ')
  
  # remove \n from all words 
  text = gsub(pattern="\n", x=text, replacement = ' ')
  
  # remove all non-alpha text (numbers, spcial chatacters, etc.)
  text = gsub(pattern="[^[:alpha:]]", x=text, replacement = ' ')
  
  #lower case all characters
  text = tolower(text)
  
  #remove non-english carachters
  text = iconv(text, "latin1", "ASCII", sub="")
  
  #remove more than two spaces 
  text = gsub(pattern="\\s+", x=text, replacement=' ')
  
  # remove trimming white spaces 
  text = trimws(text)

  return (text)
}
#clean the tect
temp_clean = as.data.frame(apply(as.data.frame(temp_sentenced),1,clean_text))
#remove unnecessary files to clean the environment
remove(temp_sentenced,temp)


## Ngrams from the clean sentenced text 

#ngram function
applY_ngramaze = function(x,ngram_size=2){
 # print(x)
  if ((nchar(x) > 0) && (sapply(gregexpr("[A-z]\\W+", x), length) >= ngram_size)) {
    ngs = get.ngrams(ngram(x , n=ngram_size))
  } else{
    ngs = as.vector('None')
  }
  return(ngs)
}


#apply it to the clean sentenced text  
ngrams_table = apply(as.matrix(temp_clean), MARGIN = 1, FUN=applY_ngramaze) 
#Note that it is in the form of a list
#remove unnecessary files
remove(temp_clean)

#unlist it to a vector
ngram_unlist = unlist(ngrams_table)

#create frequency table 
ngram_frequency_table = as.data.frame(table(ngram_unlist))

#order the table by frequency
ngram_frequency_table = ngram_frequency_table[order(ngram_frequency_table$Freq,
                                                    decreasing = TRUE),]

#After Investigating the table, we observe that None has the highest frequency. 
#Since we have designed the ngramaze function to produce None if the sentence 
#cannot be ngramaze, we can remove it 
#Also those terms, such as aaa,zzzzz, have low frequency and we can disregard them,
#since we will take those with highest frequency

ngram_frequency_table = ngram_frequency_table[-1,]
#Remove the unnecessary files
remove(ngram_unlist,ngrams_table)

## Create ngram data set to use it as a prediction
# separate first and second word based on a spaces
split_first_component = function(x){
  components_list = unlist(strsplit(x,split = ' '))
  return(components_list[1])
}

ngram_split_first_component = apply(as.matrix(ngram_frequency_table$ngram_unlist),
                                    MARGIN = 1, split_first_component)  


split_second_component = function(x){
  components_list = unlist(strsplit(x,split = ' '))
  return(components_list[2])
}

ngram_split_second_component = apply(as.matrix(ngram_frequency_table$ngram_unlist),
                                    MARGIN = 1, split_second_component)
#add to the table 
ngram_frequency_table$ngram_first_component = ngram_split_first_component
ngram_frequency_table$ngram_second_component = ngram_split_second_component


remove(ngram_split_first_component,ngram_split_second_component)

#remove ngram unlist column

ngram_frequency_table = ngram_frequency_table[,-1]


## Save two files
#the full file
write.csv(ngram_frequency_table,'ngram_frequency_table.csv',row.names = FALSE)
#Number of observations which happened ones 
sum(ngram_frequency_table$Freq==1)

#remove them, in order to get smaller dataset 
write.csv(ngram_frequency_table[ngram_frequency_table$Freq>1,],
          'ngram_frequency_table_above1.csv',row.names = FALSE)




