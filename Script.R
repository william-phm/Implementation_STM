#Loading the Documents
library(readtext)
PDF_text <- lapply(2003:2021,function(x){readtext(paste0("C:/Users/willi/Downloads/ECON4670/Policy Addresses/",x,".pdf"))})
PDF_text <- Reduce(function(x,y)merge(x,y,all=T),PDF_text)

#Load the documents into a corpus object
library(quanteda)
PDF <- corpus(PDF_text)
summary(PDF)

#Creating objects from PDF corpus

#Removing numbers,symbols,punct,url and comomn English stopwords (Sourced from stopwords-iso)
toks_PDF <- tokens(PDF,remove_punct = T,remove_symbols = T,remove_numbers = T,split_hyphens = T) %>% tokens_remove(
  pattern=c(stopwords(source = "stopwords-iso"),"hong","kong"))

dfm_PDF <- dfm(toks_PDF) %>% dfm_trim(min_termfreq = 2,min_docfreq = 2)

#Storing summary statistics on tokens and types before and after transformation
Stat_PDF <- data.frame(ntype(PDF),ntoken(PDF),ntype(toks_PDF),ntoken(toks_PDF))

#Sentiment Analysis
library(quanteda.dictionaries)

#Count occurrences of keywords in each Laver-Garry group and convert into relative frequency, 
#saved into a data-frame
DF_dfm_PDF_LG <- dfm_PDF %>% dfm_lookup(data_dictionary_LaverGarry) %>% 
                 dfm_weight(scheme = "prop") %>% 
                 convert(to = "data.frame")

#Structural Topic Model
library(stm)

#Convert dfm to stm data format
STM_PDF <- dfm(toks_PDF) %>% convert(to = "stm")

#Adding metadata covariates
STM_PDF$meta$Year <- 2003:2021
STM_PDF$meta$Chief_Executive <- c(rep("Tung Chee-hwa",3),rep("Sir Donald Tsang",7),rep("Leung Chun-ying",5),rep("Carrie Lam",4)) %>% as.factor()

#Searching for Optimal K
Search_K <- searchK(STM_PDF$documents,STM_PDF$vocab,seq(10,30,2),
                    data = STM_PDF$meta,
                    prevalence = ~Chief_Executive+Year)

#Plotting Diagnostics
plot(Search_K)

ggplot(Search_K$results,aes(as.numeric(semcoh),as.numeric(exclus),label=K))+
  geom_line()+
  geom_text()+
  xlab("Semantic Coherence")+ylab("Exclusivity")


STM_PDF_M <- stm(STM_PDF$documents,STM_PDF$vocab,K = 18,data = STM_PDF$meta,prevalence = ~Chief_Executive+Year)

#Validation and identification

#Show the top-words in each Topic
STM_PDF_M_labels <- labelTopics(STM_PDF_M)

#Plotting by keywords and proportions
plot(STM_PDF_M,labeltype = "frex",n=6)

#Estimate and plot the effect of yearly changes
STM_PDF_M_effects <- estimateEffect(1:18~Year+Chief_Executive,STM_PDF_M,metadata = STM_PDF$meta)
par(mfrow=c(3,6))
for (i in 1:18) {
  plot(STM_PDF_M_effects,"Year",method = "continuous",topics = 1,
       main = paste(STM_PDF_M_labels$frex[i,1:3]),
       ylab = "",printlegend = F)
}

#Plotting topic differences based on Chief_Executive
par(mfrow=c(2,3))
Chief_Executive_Name <- c("Tung Chee-hwa","Sir Donald Tsang","Leung Chun-ying","Carrie Lam")
for (i in 1:4) {
  for (j in i:4) {
    if(i!=j){
    plot(STM_PDF_M_effects,"Chief_Executive",method = "difference",
         cov.value1 = Chief_Executive_Name[i],
         cov.value2 = Chief_Executive_Name[j],
         main = paste(Chief_Executive_Name[i],"against",Chief_Executive_Name[j]),
         verbose.labels = F)
    }
  }
}


#Plotting and data-wrangling
library(tidyverse)
library(ggplot2)
library(wordcloud)

#Transforming the data-frame from wide to long
DF_dfm_PDF_LG <- DF_dfm_PDF_LG %>% pivot_longer(!doc_id,names_to = "Category",values_to = "Relative Frequency")

#Plot a line graph on the relative frequency of Economic mentions
ggplot(DF_dfm_PDF_LG %>% filter(str_detect(Category,"INSTI")),aes(doc_id,`Relative Frequency`,group=Category,colour=Category)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 90))
#Plot a wordcloud of the most prominent wordcloud (#Topic 18)
cloud(STM_PDF_M,topic = 18,scale = c(3,.25))