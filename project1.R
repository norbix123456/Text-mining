library(tm)
library(SnowballC)
library(ggplot2)
library(dplyr) # from version 1.0.0 only
library(wordcloud)
library(cluster) 
library(fpc)
library(topicmodels)
library(lsa)
library("plyr")
library("stringr")
library("ldatuning")

setwd("C:\\Users\\Norbix\\Downloads")
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\Dorpusproject1csvka"

docs<- VCorpus(DirSource(directory=file_loc,encoding="UTF-8"))
writeLines(as.character(docs[[1]]))
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers)
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("–", " ", docs[[j]])
  docs[[j]] <- gsub("’", " ", docs[[j]])
  docs[[j]] <- gsub("“", " ", docs[[j]])
  docs[[j]] <- gsub("…", " ", docs[[j]])
  docs[[j]] <- gsub("‘", " ", docs[[j]])
  docs[[j]] <- gsub(")", " ", docs[[j]])
  docs[[j]] <- gsub("”", " ", docs[[j]])
}

docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removeWords, stopwords("English"))
StW<-read.table("C:/Users/Norbix/Desktop/studia/sem5/DatamininginBusiness/StopWords.txt")
StWW<-as.character(StW$V1)
docs <- tm_map(docs, removeWords, StWW)
docs <- tm_map(docs, stripWhitespace)
for (j in seq(docs)) {
  docs[[j]]<-stemDocument(docs[[j]], language = "english") 
} 
writeLines(as.character(docs[[1]]))


write.csv(docs[[10]],file="C:/Users/Norbix/Desktop/studia/sem5/DatamininginBusiness/finalcsvcorpusproject1/docs10.csv")
docs[[6]]

dtm <- DocumentTermMatrix(docs)
dtm
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20),bounds = list(global = c(2,Inf))))
dtmr
m0 <- as.matrix(dtm)
write.csv(m0, file="C:/Users/Norbix/Desktop/studia/sem5/DatamininginBusiness/exceldlaproject1/DocumentTermMatrix.csv")







#################BEGGINING#######################################
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\bolt.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:214)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\brotherbear.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:206)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\chickenlittle.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:293)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\iceage.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:423)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\iceage2.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:259)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\iceage3.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:136)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\lilo.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:421)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\madagascar.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:331)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\overthehedge.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:298)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\rio.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:219)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\robots.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:297)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\shrek.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:706)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\shrek2.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:651)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\sindbad.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:92)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\temp\\temp\\treasure.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:309)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\wytwurniefilmowe\\disney.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:1443)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\wytwurniefilmowe\\20th_Century_FOX.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:1334)
file_loc <- "C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\wytwurniefilmowe\\DreamWorks.csv"
# change TRUE to FALSE if you have no column headings in the CSV
x <- read.csv(file_loc, header = TRUE)
x$doc_id<-(1:2078)
x <- x %>% select(doc_id,text)
corp <- VCorpus(DataframeSource(x))
dtm <- DocumentTermMatrix(corp)




dtmr <-DocumentTermMatrix(corp, control=list(wordLengths=c(3, 20),bounds = list(global = c(2,Inf))))
dtmr1 = removeSparseTerms(dtmr, 0.70) # Remove sparse terms from a dtmr
dtmr1
doc_length <- as.data.frame(rowSums(as.matrix(dtm)))
doc_length
max_length<-max(doc_length)
max_length
min_length<-min(doc_length)
min_length
aver_length<-mean(rowSums(as.matrix(dtm)))
aver_length
freqr <- colSums(as.matrix(dtm))
length(freqr)
freq <- sort(freqr, decreasing=TRUE)
head(freq, 14)
tail(freq, 14)


##############PROPER ANALYSIS#################
findAssocs(dtmr,"film",0.25)

set.seed(42)
wordcloud(names(freq),freq, min.freq=70,colors=brewer.pal(6, "Dark2"))



raw.sum=apply(dtm,1,FUN=sum)
raw.sum

mmm<-nrow(dtm[raw.sum==0,])
mmm

if (mmm==0) {
  dtm2<-dtm
  NN<-nrow(dtm)
  NN
} else {
  dtm2<-dtm[raw.sum!=0,]
  NN<-nrow(dtm2)
}
dtm2

system.time({
  tunes <- FindTopicsNumber(
    dtm = dtm2,
    topics = c(2:15),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
    method = "Gibbs",
    control = list(seed = 12345),
    mc.cores = 4L,
    verbose = TRUE
  )
})
FindTopicsNumber_plot(tunes)



burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 3

ldaOut <-LDA(dtm2, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)
#setwd("C:\\Users\\ninar\\OneDrive\\Documents\\Dedaktyka\\DUAN\\")
#____topics keywords___________________________
ldaOut.terms <- as.matrix(terms(ldaOut, 10))
ldaOut.terms
#___topics probability per document_____________
topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities
#___Topic proportion over Corpus________________
col.sum=apply(topicProbabilities,2,FUN=sum) # 1 - rows sum, 2- columns sum
col.sum<-as.matrix(col.sum)
dim(col.sum)
sum.TP=col.sum/sum(col.sum)
sum.TP
#___topics by Documents_________________________
ldaOut.topics <- as.matrix(topics(ldaOut))
rownames(ldaOut.topics)<-as.character(rownames(dtm2))
ldaOut.topics
nrow(ldaOut.topics)
Comment<-seq(1, NN, by=1)
Comment
wf=data.frame(Comment=Comment, Topics=ldaOut.topics)
wf
#________________________________________________________
#___________Building Sub-Corpus of Topic 1_________________

topic1<-wf[wf[2] == 1,]              #find Topic 1
topic1$Comment
length(topic1$Comment)

#number of comments with Topic 1
kk1<-nrow(topic1)
kk1
kk<-nrow(dtm2)
kk

list1<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==1) {               #find Topic 1
    list1<-c(list1,i)}
  i=i+1
}
length(list1)

wf1=NULL
for (i in 1:kk) {
  for (j in 1:kk1) {
    if (i==list1[j]){
      c <- data.frame(file=as.character(wf[list1[j],1]),document=as.character(corp[[i]]))
      wf1=rbind(wf1,c)
    }
  }
}
wf1
wf1$document[1]

#___________Corpus Creating_________________________________________
Topic_1_docs <- Corpus(VectorSource(as.character(wf1$document))) #Corpus for Topic 1
writeLines(as.character(Topic_1_docs[[1]])) #Corpus for Topic 1
#___________Writing in CSV File_ with Comments of Topic 1 __________

mycorpus_dataframe <- data.frame(text=wf1$document, stringsAsFactors=F)
mycorpus_dataframe
write.csv(mycorpus_dataframe,'Film_plot.csv', row.names=FALSE) #File with documents for Topic 1
                  #TOPIC 2
###################################################################################
###################################################################################

topic1<-wf[wf[2] == 2,]              #find Topic 2
topic1$Comment
length(topic1$Comment)

#number of comments with Topic 1
kk1<-nrow(topic1)
kk1
kk<-nrow(dtm2)
kk

list1<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==2) {               #find Topic 2
    list1<-c(list1,i)}
  i=i+1
}
length(list1)

wf1=NULL
for (i in 1:kk) {
  for (j in 1:kk1) {
    if (i==list1[j]){
      c <- data.frame(file=as.character(wf[list1[j],1]),document=as.character(corp[[i]]))
      wf1=rbind(wf1,c)
    }
  }
}
wf1
wf1$document[1]

#___________Corpus Creating_________________________________________
Topic_2_docs <- Corpus(VectorSource(as.character(wf1$document))) #Corpus for Topic 1
#___________Writing in CSV File_ with Comments of Topic 1 __________

mycorpus_dataframe <- data.frame(text=wf1$document, stringsAsFactors=F)
mycorpus_dataframe
write.csv(mycorpus_dataframe,'Film_spectrum.csv', row.names=FALSE) #File with documents for Topic 1
                        #TOPIC 3
###########################################################################################
###########################################################################################

topic1<-wf[wf[2] == 3,]              #find Topic 3
topic1$Comment
length(topic1$Comment)

#number of comments with Topic 1
kk1<-nrow(topic1)
kk1
kk<-nrow(dtm2)
kk

list1<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==3) {               #find Topic 3
    list1<-c(list1,i)}
  i=i+1
}
length(list1)

wf1=NULL
for (i in 1:kk) {
  for (j in 1:kk1) {
    if (i==list1[j]){
      c <- data.frame(file=as.character(wf[list1[j],1]),document=as.character(corp[[i]]))
      wf1=rbind(wf1,c)
    }
  }
}
wf1
wf1$document[1]

#___________Corpus Creating_________________________________________
Topic_3_docs <- Corpus(VectorSource(as.character(wf1$document))) #Corpus for Topic 1
#___________Writing in CSV File_ with Comments of Topic 1 __________

mycorpus_dataframe <- data.frame(text=wf1$document, stringsAsFactors=F)
mycorpus_dataframe
write.csv(mycorpus_dataframe,'Film_recommendation.csv', row.names=FALSE) #File with documents for Topic 1

                                    #TOPIC 4
##############################################################################################
##############################################################################################



topic1<-wf[wf[2] == 4,]              #find Topic 4
topic1$Comment
length(topic1$Comment)

#number of comments with Topic 1
kk1<-nrow(topic1)
kk1
kk<-nrow(dtm2)
kk

list1<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==4) {               #find Topic 4
    list1<-c(list1,i)}
  i=i+1
}
length(list1)

wf1=NULL
for (i in 1:kk) {
  for (j in 1:kk1) {
    if (i==list1[j]){
      c <- data.frame(file=as.character(wf[list1[j],1]),document=as.character(corp[[i]]))
      wf1=rbind(wf1,c)
    }
  }
}
wf1
wf1$document[1]

#___________Corpus Creating_________________________________________
Topic_4_docs <- Corpus(VectorSource(as.character(wf1$document))) #Corpus for Topic 1
#___________Writing in CSV File_ with Comments of Topic 1 __________

mycorpus_dataframe <- data.frame(text=wf1$document, stringsAsFactors=F)
mycorpus_dataframe
write.csv(mycorpus_dataframe,'Film_plot.csv', row.names=FALSE) #File with documents for Topic 1





setwd("C:\\Users\\Norbix\\Desktop\\studia\\sem5\\DatamininginBusiness\\topikidoclusteringu\\treasure")
wd<-"C:/Users/Norbix/Desktop/studia/sem5/DatamininginBusiness/topikidoclusteringu/treasure"
docs <- Corpus(DirSource(wd))
dtm <- DocumentTermMatrix(docs)
dtm
getwd()
filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm)<-filenames 
d1 <- dist(dtm, method="euclidian")
# make the clustering
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=1)
setwd("C:\\Users\\Norbix\\Downloads")













# #___________Writing in CSV File_ with Comments of Topic 1 __________
# 
# mycorpus_dataframe <- data.frame(text=wf1$document, stringsAsFactors=F)
# mycorpus_dataframe
# write.csv(mycorpus_dataframe,'Topic 1_Comments.csv', row.names=FALSE) #File with documents for Topic 1


#####################SENTIMENT_ANALYSIS########################


neg=scan("negative-words.txt", what="character", comment.char=";" )
pos=scan("positive-words.txt", what="character", comment.char=";" )


#__________Initialization of the Sentiment analysis Procedure_______________

score.sentiment = function(docs, pos.words, neg.words, .progress='none')
{
  scores = laply(docs_s, function(docs, pos.words, neg.words) {
    
    word.list = str_split(docs, '\\s+')
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=docs)
  return(scores.df)
}


#___________________Topic 1 Sentiment Scoring _______________________

result=c()

#docs<-Topic_1_docs # You need to replace it into Topic_2_docs, ...Topic_5_docs in the next steps
docs<-corp
m1=c()
for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  print(docs_s)
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text)
  #print(newRow1)
  m1<- rbind(m1,newRow1)
  #print(m1)
}
head(m1)
m1[1:3,]
#______________________Statistics______________________________________________
summary(m1$Score)
minn<-min(m1$Score)
minn
maxx<-max(m1$Score)
maxx
mmm<-maxx-minn
mmm
#__________Topic 1___Histograms_______________________________________________


#______________________Histogram_1____________________________________________
h<-hist(m1$Score,
        main="Histogram for the Sentiment by Topic _______",
        xlab="Scores",
        ylab="Number of of Opinions",
        right=FALSE,
        border="blue",
        col="green",
        freq=TRUE,
        las=1,
        xlim=c(minn,maxx),
        breaks=mmm
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex = 0.8, pos = 1)
m1$Score
h$count



