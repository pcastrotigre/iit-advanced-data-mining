install.packages("gridExtra")

library("tm")  
library("SnowballC")  
library("wordcloud")  
library("RColorBrewer") 
library("gridExtra")

directory = "/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/20news-bydate/20news-bydate-train"
dataset <- list.dirs(directory)
dataset <- dataset[c(2:length(dataset))]

get_dtm <- function(data2) {
  news <- Corpus(DirSource(data2, recursive=TRUE),readerControl = list(reader=readPlain))
  news <- tm_map(news, removeWords,"Subject")
  news <- tm_map(news, removeWords,"Organization")
  news <- tm_map(news, removeWords,"Writes")
  news <- tm_map(news, removeWords,"From")
  news <- tm_map(news, removeWords,"Lines")
  news <- tm_map(news, removeWords,"Expires")
  news <- tm_map(news, removeWords," NNTP-Posting-Host")
  news <- tm_map(news, removeWords,"article")
  news <- tm_map(news, content_transformer(tolower)) ## Convert to Lower Case
  news <- tm_map(news, removeWords, stopwords("english")) ## Remove Stopwords
  news <- tm_map(news, removePunctuation) ## Remove Punctuations
  news <- tm_map(news, stemDocument) ## Stemming
  news <- tm_map(news, removeNumbers) ## Remove Numbers
  news <- tm_map(news, stripWhitespace) ## Eliminate Extra White Spaces
  news <- tm_map(news , PlainTextDocument)
  return(DocumentTermMatrix(news,control=list(wordLengths=c(4,Inf))))
}

get_tdm <- function(data2) {
  news <- Corpus(DirSource(data2, recursive=TRUE),readerControl = list(reader=readPlain))
  news <- tm_map(news, removeWords,"Subject")
  news <- tm_map(news, removeWords,"Organization")
  news <- tm_map(news, removeWords,"Writes")
  news <- tm_map(news, removeWords,"From")
  news <- tm_map(news, removeWords,"Lines")
  news <- tm_map(news, removeWords,"Expires")
  news <- tm_map(news, removeWords," NNTP-Posting-Host")
  news <- tm_map(news, removeWords,"article")
  news <- tm_map(news, content_transformer(tolower)) ## Convert to Lower Case
  news <- tm_map(news, removeWords, stopwords("english")) ## Remove Stopwords
  news <- tm_map(news, removePunctuation) ## Remove Punctuations
  news <- tm_map(news, stemDocument) ## Stemming
  news <- tm_map(news, removeNumbers) ## Remove Numbers
  news <- tm_map(news, stripWhitespace) ## Eliminate Extra White Spaces
  news <- tm_map(news , PlainTextDocument)
  return(TermDocumentMatrix(news,control=list(wordLengths=c(4,Inf))))
}

get_SSE_kmeans <- function(c, kmeans){
  errors <- c()
  set.seed(1268)
  for (i in kmeans){
    cl <- kmeans(c, i)
    errors <- c(errors,cl$tot.withinss)
  }
  return(errors)
}

#
# SAVE NUMBER OF UNIQUE WORDS PER GROUP
#
save_file_unique_words <- function(dataset){
  rows <- c()
  x <- data.frame()
  for (i in 1:length(dataset))
  {
    dtm <- get_dtm(dataset[i])
    x[i,1] <- dtm$nrow
    x[i,2] <- dtm$ncol
    w <- unlist(strsplit(dataset[i],"/"))
    rows <- c(rows,w[length(w)])
  }
  colnames(x) <- c("Number of Docs","Number of Unique Words")
  rownames(x) <- rows
  
  pdf("unique_words.pdf", height=30, width=12)
  grid.table(x)
  dev.off()
}
save_file_unique_words(dataset)

#
# COMBINING DATASETS
#
experiment_1 <- c("/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/20news-bydate/20news-bydate-train/talk.politics.guns",
             "/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/20news-bydate/20news-bydate-train/rec.sport.baseball",
             "/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/20news-bydate/20news-bydate-train/comp.sys.ibm.pc.hardware")

experiment_2 <- c("/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/20news-bydate/20news-bydate-train/comp.os.ms-windows.misc",
             "/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/20news-bydate/20news-bydate-train/talk.religion.misc")

experiment_3 <- c("/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/Reuters/training/interest",
                  "/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/Reuters/training/gold",
                  "/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/Reuters/training/money")

experiment_4 <- c("/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/Reuters/training/acq",
                  "/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/Reuters/training/corn")


dtm <- get_dtm(experiment_4)


#
# TF-IDF CLUSTERING
#

dtm_tfxidf2<- weightTfIdf(dtm)

m <- as.matrix(dtm_tfxidf2)
rownames(m) <- 1:nrow(m)

norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)

kmean <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
sse <- get_SSE_kmeans(m_norm,kmean)
plot(kmean,sse,type='b',xlab="Number of Clusters",ylab="Within groups sum of squares")

#
# SVD
#

dimensions <- c(50) 
sing <- svd(dtm)

reduce <- function(sing, dim) { 
  u<-as.matrix(sing$u[, 1:dim]) 
  v<-as.matrix(sing$v[, 1:dim]) 
  d<-as.matrix(diag(sing$d)[1:dim, 1:dim]) 
  #Create the new approximated matrix 
  return(as.matrix(u%*%d%*%t(v),type='blue')) 
} 

print_top_words_by_concept <- function(dtm, svd, dimension, n){
  x <- data.frame(row.names=paste("Concept",1:dimension))
  for (i in 1:dimension){
    sv <- sort.list(svd$v[, i], decreasing = TRUE)
    
    terms <- paste(dtm$dimnames$Terms[head(sv, n)],collapse = " ")
    x[i,1] <- terms
  }
  colnames(x) <- c("Terms")
  
  pdf("mydf.pdf", height=30, width=12)
  grid.table(x)
  dev.off()
}

# Print top terms by concept
print_top_words_by_concept(dtm,sing,dimensions[length(dimensions)],10)    


# Choose best k for LSA
dtm2 <- as.matrix(dtm)
for (i in dimensions){
  lsa_matrix <- reduce(sing,i)
  cat(sprintf("normDTM:%s normLSA:%s normDIF:%s\n",norm(dtm2),norm(lsa_matrix),norm(dtm2 - lsa_matrix, "F")))
}

# CLustering each k-dimensional documents and vectors
for (i in dimensions){
  lsa_matrix <- reduce(sing,i)
  m_norm <- norm_eucl(lsa_matrix)
  
  kmean <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
  sse <- get_SSE_kmeans(m_norm,kmean)
  plot(kmean,sse,type='b',xlab="Number of Clusters",ylab="Within groups sum of squares")
}

directory = "/Users/petter/Dropbox/Documents/MSc/Advanced_Data_Mining/assignment1/Reuters/training"

dataset <- list.dirs(directory)
dataset <- dataset[c(2:length(dataset))]

save_file_unique_words(dataset)


