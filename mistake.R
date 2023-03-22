require(readtext)

data_modydick <- text(readtext(https://www.gutenberg.org/files/2701/2701-0.txt))

data_mobydick 

names(data_mobydick) <- "Moby Dick"

texplot_xray(
            kwic(tokens(data_modydick), pattern = "whale"),
            kwic(tokens(data_modydick), pattern = "ahab"))

library(quanteda.textmodels)

data(data_corpus_irishbudget2010 , package = "quanteda.textmodels")

dt_dfm <- dfm(tokens(data_corpus_irishbudget2010))
dt_dfm 


refscores <- c(rep(NA, 4), 1, -1, rep(NA, 8))
refscores

ws <- textmodel_wordscores(dt_dfm , y = refscores , smooth = 1)

textplot_scale1d(ws, highlighted = c("minister" , "have", "our" , "budget"),
                 highlighted_color ="red")

pred <- predict(ws, se.fit = TRUE)
pred


textplot_scale1d(pred , margin = "documents", 
                 groups = docvars(data_corpus_irishbudget2010,
                                  "party"))


prelbg <- predict(ws, se.fit = TRUE, rescaling = "lbg")
prelbg


textplot_scale1d(pred_lbg,margin = "documents", 
                 groups = docvars(data_corpus_irishbudget2010, "party"))

wf <- textmodel_wordfish(dfm(tokens(data_corpus_irishbudget2010)), dir = c(6,5))

wf


textplot_scale1d(wf, margin = "features", 
                 highlighted =c("government","global","children","bank","economy",
                                " the","citizenship","productivity"," deficit"),
                 highlighted_color = "red")

textplot_scale1d(wf , groups = data_corpus_irishbudget2010$party)


ca <- textmodel_ca(dt_dfm)
ca

summary(ca)


textplot_scale1d(ca, margin = "documents", groups = docvars(data_corpus_irishbudget2010 ,
                                                            "party"))