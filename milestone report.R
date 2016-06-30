
#read in lines from txt files
Blog<-readLines("en_US.blogs.txt",skipNul = TRUE)
News<-readLines("en_US.news.txt",skipNul = TRUE)
Twitter<-readLines("en_US.twitter.txt",skipNul = TRUE)

#max character length of line
BlogCharMax<-max(nchar(Blog)) #40835
NewsCharMax<-max(nchar(News)) #5760
TwitterCharMax<-max(nchar(Twitter)) #213

#num of lines 
BlogLength<-length(Blog) #899288
NewsLength<-length(News) #77259
TwitterLength<-length(Twitter) #2360148

#number of words
BlogWords <- sum(sapply(gregexpr("\\S+", Blog), length))
NewsWords <- sum(sapply(gregexpr("\\S+", News), length))
TwitterWords <- sum(sapply(gregexpr("\\S+", Twitter), length))


#take sample of lines so they process faster
BlogSample <- Blog[sample(1:length(Blog),8000)]
NewsSample <- News[sample(1:length(News),8000)]
TwitterSample <- Twitter[sample(1:length(Twitter),8000)]

#aggregate all samples into a master sample list
TextSample <- c(BlogSample,NewsSample, TwitterSample)

#write sample file
writeLines(TextSample,"./TextSample.txt")

#summary info for sample data
SampleCharMax<-max(nchar(TextSample))
SampleLength<- length(TextSample)
SampleWords <- sum(sapply(gregexpr("\\S+", TextSample), length))

#summary table

FileSummary <- data.frame(
  fileName = c("Blogs","News","Twitter", "Sample"),
  MaxChar = c(BlogCharMax, NewsCharMax,TwitterCharMax,SampleCharMax),
  lineCount = c(BlogLength, NewsLength, TwitterLength, SampleLength),
  wordCount = c(BlogWords, NewsWords, TwitterWords, SampleLength)                  
)
FileSummary


#packages
install.packages("R.utils")
library(R.utils)
install.packages("tm")
library(tm)
install.packages("RWeka")
library(RWeka)
install.packages("wordcloud")
library(wordcloud)


# convert to Corpus
cleanSample<-VCorpus(VectorSource(TextSample))

#clean Corpus
##cleanSample <- tm_map(cleanSample, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
cleanSample <- tm_map(cleanSample, content_transformer(tolower))
cleanSample <- tm_map(cleanSample, stripWhitespace)
cleanSample <- tm_map(cleanSample, removeNumbers)
cleanSample <- tm_map(cleanSample, removePunctuation)



#tokenize
#token delimiter (what separates words)

##convert cleaned corpus to data frame
cleanSample <- data.frame(text=unlist(sapply(cleanSample, '[',"content")),stringsAsFactors=F)
head(cleanSample)

#determine delimitter for Weka
delim <- " \\t\\r\\n.=+:!?,;\"()"

#set up 1 word,2 word and 3 word tokens
unitoken <- NGramTokenizer(cleanSample, Weka_control(min=1,max=1))
bitoken <- NGramTokenizer(cleanSample, Weka_control(min=2,max=2, delimiters = delim))
tritoken <- NGramTokenizer(cleanSample, Weka_control(min=3,max=3, delimiters = delim))


oneword <- data.frame(table(unitoken))
sortone <- oneword[order(oneword$Freq,decreasing=TRUE),]
sortone$probability<-oneword$Freq/sum(oneword$Freq)
colnames(sortone) <- c("Word", "Freq","Prob")
head(sortone)


twoword<- data.frame(table(bitoken))
sorttwo <- twoword[order(twoword$Freq,decreasing=TRUE),]
sorttwo$probability<-twoword$Freq/sum(twoword$Freq)
colnames(sorttwo) <- c("Word", "Freq","Prob")
head(sorttwo)


threeword<- data.frame(table(tritoken))
sortthree <- threeword[order(threeword$Freq,decreasing=TRUE),]
sortthree$probability<-threeword$Freq/sum(threeword$Freq)
colnames(sortthree) <- c("Word", "Freq","Prob")
head(sortthree)

#word cloud visualisations and frequencies of most popular tokens

wordcloud(words = sortone[1:11,1], freq = sortone[1:11,2], scale = c(5,1), colors = brewer.pal(11,"BrBG"),ordered.colors = TRUE)
head(sortone)
wordcloud(words = sorttwo[1:11,1], freq = sorttwo[1:11,2], scale = c(5,1), colors = brewer.pal(11,"BrBG"),ordered.colors = TRUE)
head(sorttwo)
wordcloud(words = sortthree[1:11,1], freq = sortthree[1:11,2], scale = c(5,1), colors = brewer.pal(11,"BrBG"),ordered.colors = TRUE)
head(sortthree)

##subset data based on frequency to increase speed for prediction
fDF1 <- sortone[sortone$Freq > 1000,]
fDF2 <- sorttwo[sorttwo$Freq > 0,]
fDF3 <- sortthree[sortthree$Freq > 0,]


save(fDF1, file="fDF1.RData");
save(fDF2, file="fDF2.RData");
save(fDF3, file="fDF3.RData");
###







