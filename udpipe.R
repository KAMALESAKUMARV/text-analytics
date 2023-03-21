#importing NLP based Library
library(udpipe) #for data prep in NLP

#Importing Dataset
data(brussels_listings , package = 'udpipe')

#Viewing dataset
View(brussels_listings)

#segregating a column
x <- table(brussels_listings$neighbourhood)
View(x)

#sorting
x <- sort(x)
View(x)

#Importing text visualiztion library
library(textplot) #for complex relations in text

#word frequency bar chart 
textplot_bar(x, panel = "Locations",col.panel = "darkgrey",xlab = "Listings" , 
                    cextext = 0.75 , addpct = TRUE , cexpct= 0.5)

#importing Dataset

data(brussels_reviews_anno, package = 'udpipe' )
View(brussels_reviews_anno)

#segregating data

y <- subset (brussels_reviews_anno , xpos %in% "NN" & language %in% "nl" & ! is.na(lemma))

View(y)


#document Term frequencies

y <- document_term_frequencies(y , document = "doc_id",term = "lemma")

View(y)

#document term matrix

dtm <- document_term_matrix(y)
dtm

#Removing low frequency words

dtm <- dtm_remove_lowfreq(dtm, maxterms = 60)
dtm

#correlation matrix
cor <- dtm_cor(dtm)
View(cor)

#importing libraries
library(glasso)  #For graphical lasso : Estimation of Gaussian Graphical models

#word correlation plot
#textplot_correlation_glasso(cor, exclude_zero =TRUE)

#word COocurrence graph
#segregating data

w <- subset(brussels_reviews_anno,xpos %in% "JJ" & language %in% "fr")

View(w)


#concurrence terms
w <- cooccurrence(w,group = "doc_id", term ="lemma")
View(w)

#cooccurence plot 
textplot_cooccurrence(w, topn = 25, subtitle = "showing only top 25")

#dependency parsing
#creating data

sentence = "hey friends, Welcome to the class. lets learn about tect analytics"

#tokenize and POS tag for each word in data 
z <- udpipe(sentence, "english")
View(z)

#importing relational data visualization library
library(ggraph)

#dependency parser plot 
textplot_dependencyparser(z)