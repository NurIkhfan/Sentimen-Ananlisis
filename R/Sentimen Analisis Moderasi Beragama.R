library(rtweet)
library(dplyr)
library(tidyr)
library(writexl)

token <- create_token(
  app = "Crawldata_PPKM",
  consumer_key = "CklAdKzfJzRo430E8Du0SDooy",
  consumer_secret = "ULb78BiKhyNWshCmsgYTrKsEryMDavmSy72MaGwSCMwXPWCiJ7",
  access_token = "1248200899278725121-TYrZjTGgWt2KsLk8uDQ15lzGLO1nsZ",
  access_secret = "SX64SfavXMblwHjNR1o2aCvEInxfxdQf1bQyOUhML6pP3",
  set_renv = TRUE
)
# Set keyword, jumlah tweet, dan parameter lainnya. 
keyword <- "Toleransi antar agama"
jumlahtweet <- 5000
bahasa <- "id"
retweet <- FALSE

# Crawl tweet
crawling <- search_tweets(keyword,
                          n = jumlahtweet,
                          include_rts = retweet,
                          type = "recent",
                          lang = bahasa
)

View(crawling)
install.packages("readr")
install.packages("readxl")
install.packages("writexl")

# Menyimpan data
write_xlsx(crawling, "data_mentah_Toleransi antar agama 25 Des - 03 Jan (4).xlsx")

write_as_excel(crawling,
               "data_mentah_Kestabilan antar umat beragama.csv",
               prepend_ids = TRUE,
               na = "",
               fileEncoding = "UTF-8"
)

library(devtools)
install_github("nurandi/katadasaR")

######PRE PROCESSING DATA#########
library(tm)
library(NLP)
library(stringr)
library(caret)
library(dplyr)
library(katadasaR)
library(tau)
library(parallel)

#data_mentah <- read.csv("C:/Users/IKHFAN/Documents/Data Mentah Hasil.csv", header=TRUE, sep=",")
#data_mentah (view)
library (readr)
urlfile = "https://raw.githubusercontent.com/NurIkhfan/Sentimen-Ananlisis/main/Data/Data%20Mentah%20Hasil.csv"
data <- read.csv(url(urlfile))

#merubah file ke dalam corpus
corpusdata <- Corpus(VectorSource(data$text))

#mengubah semua huruf kapital menjadi huruf kecil
data_casefolding <- tm_map(corpusdata,content_transformer(tolower))
inspect(data_casefolding[1:10])

#menghapus url pada dokumen
removeURL <- function(x)gsub("http[^[:space:]]*","",x)
data_URL <- tm_map(data_casefolding, content_transformer(removeURL))
inspect(data_URL[1:10])

#menghapus mention pada dokumen
remove.mention <- function(x)gsub("@\\S+","",x)
data_mention <- tm_map(data_URL, remove.mention)
inspect(data_mention[1:10])

#menghaspus hastag
remove.hashtag <- function(x)gsub("#\\S+","",x)
data_hashtag <- tm_map(data_mention, remove.hashtag)
inspect (data_hashtag[1:10])

#cleaning punctuation
data_punctuation <- tm_map (data_hashtag, content_transformer(removePunctuation))
inspect(data_punctuation[1:10])

#cleaning number
data_nonumber <- tm_map(data_punctuation, content_transformer(removeNumbers))
inspect(data_nonumber[1:10])

#stemming atau menghapus imbuhan sehingga menjadi kata dasar
stem_text<-function(text,mc.cores=1)
{
  stem_string<-function(str)
  {
    str<-tokenize(x=str)
    str<-sapply(str,katadasaR)
    str<-paste(str,collapse = '')
    return(str)
  }
  x<-mclapply(X=text,FUN=stem_string,mc.cores=mc.cores)
  return(unlist(x))
}
data_stemming<-tm_map(data_nonumber,stem_text)
inspect(data_stemming[1:10])

#menghapus kata tidak penting(stopword)
cStopwordID<-readLines("D:/stopwords.csv")
data_stopword<-tm_map(data_stemming,removeWords,cStopwordID)  
inspect(data_stopword[1:10])
str(corpusdata)

#menghapus spasi berlebihan
data_whitespace<-tm_map(data_stopword,stripWhitespace)
inspect(data_whitespace[1:10])

#menyimpan data bersih ke formatCSV
databersih<-data.frame(text=unlist(sapply(data_whitespace,'[')),stringAsFactors=F)
write.csv(databersih,file="D:/#Kuliah AIS/Semester 5/Pengolahan TA/databersih.csv")

#tokenize
library(tokenizers)
corpustext <- Corpus(VectorSource(data$text))
inspect(corpustext[1:10])
text=corpustext
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(text)

#WORD CLOUD
library(wordcloud)
text<-as.character(text)
wordcloud(text, min.freq=5, max.words=500, colors = brewer.pal(8,"Dark2"),
          random.order = FALSE)

####word count
word.count(Data$text)
wordcount()

databersih<-read.csv('D:/#Kuliah AIS/Semester 5/Pengolahan TA/databersih.csv')
text <- Corpus(VectorSource(data$text))
View(text)

install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("Rstem", repos = "http://www.omegahat.net/R", type = "source")
install.packages("sentiment")

library(e1071)
library(caret)
library(syuzhet)

#######SENTIMEN##################
library(SnowballC)
library(Rstem)
library(SentimentAnalysis)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(sentiment)

# classify emotion
class_emo = classify_emotion(databersih, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
# classify polarity
class_pol = classify_polarity(data, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
View(polarity)
View(emotion)

# data frame with results
sent_df = data.frame(text=data, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
write.csv(sent_df, file = 'E:/SEMESTER 7/dataSentimen2.csv')
View(sent_df)
head(sent_df,20)
table(sent_df$emotion)

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  labs(title = "Sentiment Analysis IC",
       plot.title = element_text(size=12))
plotSentiments1 <- function(sentiment_dataframe, title) 
{
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Number of Tweets") + 
    xlab("Emotion Categories")
}

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  labs(title = "Sentiment Analysis ",
       plot.title = element_text(size=12))
plotSentiments2 <- function(sent_df, title)
{
  library(ggplot2)
  ggplot(sent_df, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position="right") +
    ylab("Number of Tweets") +
    xlab("Polarity Categories")
}

#liat jumlah tweets
View(sent_df)
table(sent_df$polarity)

# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = text[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs,"E:/stopwords.csv")
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
tdm

# comparison word cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
                 title.size=1.5, max.words=1000)
