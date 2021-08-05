#Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#library("syuzhet")
#library("ggplot2")

#load sampletext from file
filepath <- "https://shared.djambred.my.id/sampletext.txt"
#text <- readLines(file.choose(""))

#set data to corpus
docs <- Corpus(VectorSource(text))
#mecahin kalimat

#inspect doc
inspect(docs)

#text transformation
#menghapus spasi
toSpace <- content_transformer(function(x , pattern) gsub(pattern, " ", x ))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#Convert the text to lower case
#merubah yang ada huruf kapital menajdi huruf kecil semua
docs <- tm_map(docs, content_transformer(tolower))

# Remove Numbers
#mengahpus angka
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
#menghapus titik koma dihapus
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
docs <- tm_map(docs, removeWords, c("together", "freedom"))

# Remove puntuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Make Matrix text with frequen
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

# Make wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 50, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

barplot(words = d$word, d$freq, las = 2, names.arg = d$word,
        col = "lightgreen", main = "Top 5 most frequent words",
        ylab = "Word frequencies")

findAssocs(dtm, terms = c("wil", "color", "governor"), corlimit = 0.25)

findAssocs(dtm, terms = findFreqTerms(dtm, lowfreq = 15), corlimit = 0.25)

syuzhet_vector <- get_sentiment(text, method="syuzhet")

head(syuzhet_vector)
summary(syuzhet_vector)

bing_vector <- get_sentiment(text, method = "bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

rbind(
  sign(head(syuhzet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

d<-gect_nrc_sentiment(text)
head(d,10)

#transpose
td<-data.frame(d)
#td
#the function rowSums computes column sums across rows for each level of a grouping variable
td_new <- data.frame(rowSums(td[2:8]))
#transformation and cleaning
names(td_new)[1]<-"count"
td_new<- cbind("sentiment" = rownames(td_new), td_new)
#td_new
#td
rownames(td_new)<-NULL
td_new2<-td_new[1:3,]
#plot one - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

plot2 <- ggplot(data = td_new2, aes(x = percentage, y = count, color = genus,)) +
  labs(x="percentage", y = "count")
barplot(
  sort(colSums(prop.table(d[, 1:7]))),
  horiz = FALSE,
  cex.names = 0.7,
  las = 1,
  main = "sentimental Feel", xlab = "Percentage"
)
