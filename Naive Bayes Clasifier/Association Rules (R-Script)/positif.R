#Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(stringr)

docs<-readLines("positif.csv")
#Load the data as a corpus
docs <- Corpus(VectorSource(docs))
#Remove your own stop word
#specify your stopwords as a character vector
docs <- tm_map(docs, removeWords,
               c("jkt","yogyakarta","jogja","yogya","bali","jakarta","medan","group","boeing","sdh","balikpapan","bandung","makassar","tdk","denpasar","lombok","lbh","lampung","dll","wib","mas","adik","ambon","dumai","yah","thai","sih","moga","hal","kupang","hehehe"))
#Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 25)
#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
#Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 4)
#Asosiasi kata
v<-as.list(findAssocs(dtm, terms =c("terbang","layan","delay","harga",
                                    "jalan","nyaman","makan","ramah","pilih","kursi","tumpang","murah"),
                      corlimit =
                        c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)))
v
#Barplot
k<-barplot(d[1:10,]$freq, las = 2, names.arg =
             d[1:10,]$word,cex.axis=1.2,cex.names=1.2,
           main ="Most frequent words",
           ylab = "Word frequencies",col =topo.colors(10))
termFrequency <- rowSums(as.matrix(dtm))
termFrequency <- subset(termFrequency, termFrequency>=5)
text(k,sort(termFrequency, decreasing = T)-
       1,labels=sort(termFrequency, decreasing = T),pch = 6, cex = 1)

