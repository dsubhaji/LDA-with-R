library(XLConnect)
library(lda)
library(ggplot2)
library(reshape2)
library(tm)
library(SnowballC)
#change the path to the place where te dataset is stored
path<-"/run/media/suppu/Data/r lang/datasets/JSS-papers-1984-2014.xlsx"
data=readWorksheetFromFile(path,"Data")
data=data[,5]  #read the abstract only
sw <- c(stopwords("english"),"na","NA","content")
data<-lapply(data,tolower)
#remove stopwords
i<-1
while(i<3726)
{
  data[[i]]<-removeWords(data[[i]],sw);
  i<-i+1
}
data<-lapply(data,tolower)
data<-lapply(data,removePunctuation)
data<-lapply(data,removeNumbers)
databack<-data #backup of the data
#clean entries to " " which have no data entered in them
i<-1
while(i<=length(data))
{
  if(is.na(data2[[i]]))
  {
    data[[i]]<-" "
  }
  i<-i+1
}
j<-1
while(j<3726)
{
  data[[j]]<-strsplit(data[[j]]," ");
  temp<-unlist(data[[j]])
  i<-1
  while(i<=length(temp))
  {
    if(temp[i]=="")
    {
      temp<-temp[-i]
    }
    else
    {
      i<-i+1
    }
  }
  data[[j]]<-relist(temp,temp)
  j<-j+1
}
data2<-data
j<-1
while(j<3726)
{
  temp<-unlist(data2[[j]])
  i<-2
  while(i<=length(temp))
  {
    temp[1]<-paste(temp[1],temp[i])
    i<-i+1
  }
  data2[[j]]<-temp[1]
  j<-j+1
}
#create a character vector for dictionary
dict<-""
k<-1
while(k<=3725)
{
  dict<-paste(dict,data2[[k]])
  k<-k+1
}
dict<-strsplit(dict," ")
dict<-unlist(dict)
#now stem the document and complete it
stemCompletion_mod <- function(x,dict=dictCorp) {
  stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="prevalent"),sep="", collapse=" "))
}
dictCorp<-Corpus(VectorSource(dict))
Data<-rep(list(NA),3725)
i<-1
while(i<=length(data))
{
  if(is.na(data2[[i]]))
  {
    data[[i]]<-" "
  }
  i<-i+1
}
k<-1
while(k<=900)
{
  temp<-data[[k]]
  temp<-lapply(temp,stemDocument)
  temp<-unlist(temp)
  i<-2
  while(i<=length(temp))
  {
    temp[1]<-paste(temp[1],temp[i])
    i<-i+1
  }
  temp<-temp[1]
  temp<-unlist(temp)
  try<-Corpus(VectorSource(temp))
  stemmed<-lapply(try,stemCompletion_mod)
  stemmed<-unlist(stemmed)
  Data[k]<-stemmed
  print(k)
  k<-k+1
}
#now generating the topic model
docs<-lexicalize(Data)
vocab=docs$vocab
docs=docs$documents
K<-10
result<-lda.collapsed.gibbs.sampler(docs,K,vocab,500,0.1,0.01,compute.log.likelihood=TRUE)
top.words <- top.topic.words(result$topics, 10, by.score=TRUE)
N <- 10
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
