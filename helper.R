# Set WD
#setwd("C:/Users/BrewJR/Documents/fakepaper")

# Set seed
set.seed(sample(1:10000, 1))

# Read in cleaned up words and part of speech / sentiment
words <- read.csv("joe_words.csv")
words$word <- tolower(words$word)

# Put everything into lowercase
adjective <- tolower(words$word[which(words$pos == "adjective")])
verb <- tolower(words$word[which(words$pos == "verb")])
noun <- tolower(words$word[which(words$pos == "noun")])


# Write function to get words by type
joe <- function(pos, sentiment = NULL, reg = NULL, neg_reg = NULL, number = 1){
  
  x <- words
  
  if(!is.null(sentiment)){
    x <- x[which(x$pos == pos & x$sentiment == sentiment),]
  }
  if(!is.null(reg)){
    x <- x[which(grepl(reg, x$word)),]
  }
  if(!is.null(neg_reg)){
    x <- x[which(!grepl(neg_reg, x$word)),]
  }
  x <- x$word[which(x$pos == pos)]
  if(length(x) > 0){
    return(sample(x, number))
  } else{
    return(sample(words$word[which(words$pos == pos & words$sentiment == sentiment)], 1))
  }
  
}

# Write generate paper function
GeneratePaper <- function(title,
              author,
              to,
              sentiment,
              journal){
  
  
  ############
  line1 <- paste0("Dear ", to, ",")
  
  ############
  l2nega <- paste0("It was with a great deal of ",
                   joe("noun", "negative"), 
                   " that I read ",
                   author,
                   "'s most ",
                   joe("adjective", "negative"), 
                   " article entitled '",
                   title, "'.")
  l2negb <- paste0("I was very ",
                   joe(pos = "adjective", sentiment = "negative", reg = "ed", neg_reg = "need"),
                   " to read ",
                   author,
                   "'s most ",
                   joe("adjective", "negative"), 
                   " article entitled '",
                   title, "'.")
  l2negc <- paste0(author,
                   "'s ",
                   "most ",
                   joe(pos = "adjective", sentiment = "negative"),
                   " article ('",
                   title, 
                   "')",
                   " was a perfect example of everything that is ",
                   joe("adjective", "negative"),
                   " and ", 
                   joe("adjective", "negative"),
                   " with the field of epidemiology today.")
  
  
  l2posa <- paste0(author, 
                   "'s '",
                   title,
                   "' was a most ",
                   joe("adjective", "positive", reg = "ous", neg_reg = "famous"),                   " break from the typical ",
                   joe("noun", "negative"),
                   " that our field usually produces these days.")
  l2posb <- paste0("I was absolutely ",
                   joe("adjective", "positive"),
                   " upon reading ",
                   author,
                   "'s most ",
                   joe("adjective", "positive"),
                   " article entitled '",
                   title,
                   "'.")
  l2posc <- paste0(author,
                   "'s ",
                   joe("adjective", "positive"),
                   " '",
                   title,
                   "' was a complete ",
                   joe("noun", "positive", neg_reg = "ion"),
                   ", especially when compared to the usual ",
                   joe("noun", "negative"),
                   "  published by this journal",
                   ".")
  
  if(sentiment == "negative"){
    line2 <- sample(c(l2nega, l2negb, l2negc), 1)
  } else{
    line2 <- sample(c(l2posa, l2posb, l2posc), 1)
  }
  
  ############
  l3nega <- paste0("It is rare to come across such a ",
                   joe("adjective", "negative"),
                   " example of ",
                   joe("noun", "negative"),
                   " filled with so many ",
                   joe("noun", "negative", neg_reg = "ness"),
                   "s.")
  l3negb <- paste0("In my many years of reading ",
                   journal, 
                   ", this is the first time I've found myself so ",
                   joe("adjective", "negative", reg = "*ed"),
                   " by an article that I felt compelled to write.")
  l3negc <- paste0("Even by the standards of ",
                   journal,
                   ", this article was noteworthy for being both ",
                   joe("adjective", "negative"), 
                   " and ",
                   joe("adjective", "negative"),
                   ", a rare combination indeed.")
  
  l3posa <- paste0("What a joy it was to read such a fine example of ",
                   joe("noun", "positive", "inte"),
                   " and ",
                   joe("noun", "positive"),
                   ", a rare combination in today's increasingly ",
                   joe("adjective", "negative", neg_reg="ly"),
                   " field.")
  
  l3posb <- paste0("Few studies so ",
                   joe("adjective", "positive", "succ", neg_reg = "ly"),
                   "ly combine such ",
                   joe("adjective", "positive", reg = NULL, neg_reg = "ly"),
                   " design with ",
                   joe("adjective", "positive"),
                   " analytical approaches.")
  
  l3posc <- paste0("I have never come across a finer example of ",
                   joe("adjective", "positive"), 
                   " methods and ",
                   joe("adjective", "positive"),
                   " writing, and I ",
                   joe("verb", "positive"),
                   " the authors for ",
                   joe("verb", "positive"),
                   "ing in such ",
                   joe("adjective", "positive"),
                   " ways. ")
  if(sentiment == "negative"){
    line3 <- sample(c(l3nega, l3negb, l3negc), 1)
  } else{
    line3 <- sample(c(l3posa, l3posb, l3posc), 1)
  }
  
  ############
  l4negasub1 <- paste0("The authors lack any and all ", 
                       joe("adjective", "positive"),
                       " evidence for their secondary findings. ")
  l4negasub2 <- paste0("The authors' methods are both ", 
                       joe("adjective", "negative"),
                       " and ",
                       joe("adjective", "negative", "ed"),
                       ". ")
  l4negasub3 <- paste0("The authors' insistence on the ",
                       joe("noun", "positive", "ence"),
                       " of their findings ignore nearly a decade of ",
                       joe("adjective", "positive", neg_reg = "ly"),
                       " work in an increasingly ",
                       joe("adjective", "positive"),
                       " field. ")
  l4negasub4 <- paste0("The authors' conclusions are not only unsupported by their (",
                       joe("adjective", "negative"),
                       ") data, but they also appear to contradict it ",
                       joe("adjective", "negative", neg_reg = "ly"),
                       "ly. ")
  
  
  l4nega <- paste0("The article's shortcomings are three-fold: 1) ",
                   l4negasub1, " 2) ",
                   l4negasub2, " 3) ",
                   l4negasub3)
  l4negb <- paste0("The article has two ",
                   joe("adjective", "negative"),
                   " shortcomings: 1) ",
                   l4negasub4, " 2) ",
                   l4negasub2)
  
  l4posasub4 <- paste0("the authors' ",
                       joe("adjective", "positive", neg_reg = "ly"),
                       " analysis and ",
                       joe("adjective", "positive", neg_reg = "ly"),
                       " interpretation of their results more than adequately compensate ",
                       "for the ",
                       joe("noun", "negative", "ence"), 
                       " of their study design. ")
  
  l4posa <- paste0(l4negasub3,
                   " That said, ",
                   l4posasub4)
  l4posb <- paste0("The authors'",
                   joe("adjective", "positive", neg_reg = "ly"),
                   " analysis and ",
                   joe("adjective", "positive", neg_reg = "ly"),
                   " interpretation of their results more than adequately compensate ",
                   "for the ",
                   joe("noun", "negative", "ence"), 
                   " of their study design.  That said, it is worth pointing out ",
                   "2 noteworthy problems: ",
                   "1) ",
                   l4negasub4, 
                   " 2) ",
                   l4negasub3,
                   " These criticsms should not detract from ", 
                   author,
                   "'s ",
                   "overall message (which I found to be ",
                   joe("adjective", "positive"),
                   ") or the article's ",
                   joe("noun", "positive", "est"),
                   ". ")
  
  if(sentiment == "negative"){
    line4 <- sample(c(l4nega, l4negb), 1)
  } else{
    line4 <- sample(c(l4posa, l4posb), 1)
  }
  
  ############
  l5nega <- paste0("I sincerely hope that ",
                   author,
                   " will reconsider their conclusions, and that in the future ",
                   journal,
                   " will be a bit more careful in selecting articles they deem ",
                   joe("adjective", "positive"),
                   " enough for publication. ")
  l5negb <- paste0("Though this article contains elements of ",
                   joe("adjective", "positive", neg_reg = "ly"),
                   " insight, ",
                   author,
                   " muddled their message with ",
                   joe("adjective", "negative", neg_reg = "ly"),
                   "ly ", 
                   joe("adjective", "negative"),
                   " analyses of otherwise ",
                   joe("adjective", "positive"),
                   " data. ")
  l5negc <- paste0("I encourage ",
                   author,
                   " to withdraw their paper from publication, given the ",
                   joe("adjective", "negative"), 
                   " flaws in the design, analysis and interpretation of",
                   " what otherwise could have been a ",
                   joe("adjective", "positive"), 
                   " contribution to our field. ")
  l5negd <- paste0("J'accuse ",
                   author,
                   " of ",
                   joe("adjective", "negative"), " ",
                   joe("noun", "negative", "ism"),
                   " and implore ",
                   journal,
                   " to withdraw this article's publication entirely. ")
  
  l5posa <- paste0("I applaud ",
                   author,
                   " for their ",
                   joe("adjective", "positive"),
                   " contribution to our field, ",
                   " and encourage others to follow their example. ")
  l5posb <- paste0(author,
                   "'s study represents a ",
                   joe("adjective", "positive"),
                   " and ",
                   joe("adjective", "positive"),
                   " shift in the way epidemiologists ",
                   joe("verb", "positive", "ize"),
                   " our understanding of ",
                   joe("noun", "positive", "ism"),
                   ". ")
  l5posc <- paste0(author,
                   "'s most serious shortcoming was that they did not fully ",
                   joe("verb", "positive", "ize"), 
                   " the ",
                   joe("noun", "positive", "ance"),
                   " of their findings. ")
  l5posd <- paste0("'",
                   title,
                   "'",
                   " deserves widespread dissemination for its ",
                   joe("verb", "positive", neg_reg = "ly"),
                   "ly ",
                   joe("adjective", "positive"),
                   " implications for future studies of the population in question. ")
  
  l5pos1 <- sample(c(l5posb, l5posc), 1)
  l5pos2 <- sample(c(l5posa, l5posd), 1)
  
  l5neg1 <- l5negb
  l5neg2 <- sample(c(l5nega, l5negc, l5negd), 1)
  
  if(sentiment == "negative"){
    line5 <- paste0(l5neg1, l5neg2)
  } else{
    line5 <- paste0(l5pos1, l5pos2)
  }
  
  x <- list(line1, line2, line3, line4, line5)
  return(x)
  
}

# GeneratePaper(title = "HIV Prevention",
#               author = "Landers et al",
#               to = "editor",
#               sentiment = "positive",
#               journal = "AJPH")

# # Set some paper parameters
# title <- "HIV Prevention Needs Among Street-Based Male Sex Workers in Providence, Rhode Island"
# author <- "Landers et al"
# to <- "Editor"
# sentiment <- "negative"
# journal <- "AJPH"

# 
# ############
# line1 <- paste0("Dear ", to, ",")
# 
# ############
# l2nega <- paste0("It was with a great deal of ",
#                  joe("noun", "negative"), 
#                  " that I read ",
#                  author,
#                  "'s most ",
#                  joe("adjective", "negative"), 
#                  " article entitled '",
#                  title, "'.")
# l2negb <- paste0("I was very ",
#                  joe(pos = "adjective", sentiment = "negative", reg = "ed"),
#                  " to read ",
#                  author,
#                  "'s most ",
#                  joe("adjective", "negative"), 
#                  " article entitled ",
#                  title, ".")
# l2negc <- paste0(author,
#                  "'s ",
#                  "most ",
#                  joe(pos = "adjective", sentiment = "negative"),
#                  " article ('",
#                  title, 
#                  "'')",
#                  " was a perfect example of everything that is ",
#                  joe("adjective", "negative"),
#                  " and ", 
#                  joe("adjective", "negative"),
#                  " with the field of epidemiology today.")
# 
# 
# l2posa <- paste0(author, 
#                  "'s '",
#                  title,
#                  "' was a most ",
#                  joe("adjective", "positive"),
#                  " break from the typical ",
#                  joe("noun", "negative"),
#                  " that our field usually produces these days.")
# l2posb <- paste0("I was absolutely ",
#                  joe("adjective", "positive"),
#                  " upon reading ",
#                  author,
#                  "'s most ",
#                  joe("adjective", "positive"),
#                  " article entitled '",
#                  title,
#                  "'.")
# l2posc <- paste0(author,
#                  "'s ",
#                  joe("adjective", "positive"),
#                  " '",
#                  title,
#                  "' was a complete ",
#                  joe("noun", "positive"),
#                  ", especially when compared to the usual ",
#                  joe("noun", "negative"),
#                  "  published by this journal",
#                  ".")
# 
# if(sentiment == "negative"){
#   line2 <- sample(c(l2nega, l2negb, l2negc), 1)
# } else{
#   line2 <- sample(c(l2posa, l2posb, l2posc), 1)
# }
# 
# ############
# l3nega <- paste0("It is rare to come across such a ",
#                  joe("adjective", "negative"),
#                  " example of ",
#                  joe("noun", "negative"),
#                  " filled with so many ",
#                  joe("noun", "negative"),
#                  "s.")
# l3negb <- paste0("In my many years of reading ",
#                  journal, 
#                  ", this is the first time I've found myself so ",
#                  joe("adjective", "negative", reg = "*ed", neg_reg = "ly"),
#                  " by an article that I felt compelled to write.")
# l3negc <- paste0("Even by the standards of ",
#                  journal,
#                  ", this article was noteworthy for being both ",
#                  joe("adjective", "negative"), 
#                  " and ",
#                  joe("adjective", "negative"),
#                  ", a rare combination indeed.")
# 
# l3posa <- paste0("What a joy it was to read such a fine example of ",
#                  joe("noun", "positive", "inte"),
#                  " and ",
#                  joe("noun", "positive"),
#                  ", a rare combination in today's increasingly ",
#                  joe("adjective", "negative"),
#                  " field.")
# 
# l3posb <- paste0("Few studies so ",
#                  joe("adjective", "positive", "succ"),
#                  "ly combine such a ",
#                  joe("adjective", "positive", reg = NULL, neg_reg = "ly"),
#                  " design with ",
#                  joe("adjective", "positive"),
#                  " analytical approaches.")
# 
# l3posc <- paste0("I have never come across a finer example of ",
#                  joe("adjective", "positive"), 
#                  " methods and ",
#                  joe("adjective", "positive"),
#                  " writing, and I ",
#                  joe("verb", "positive"),
#                  " the authors for ",
#                  joe("verb", "positive"),
#                  "ing in such a ",
#                  joe("adjective", "positive"),
#                  " manner.")
# if(sentiment == "negative"){
#   line3 <- sample(c(l3nega, l3negb, l3negc), 1)
# } else{
#   line3 <- sample(c(l3posa, l3posb, l3posc), 1)
# }
# 
# ############
# l4negasub1 <- paste0("The authors lack any and all ", 
#                      joe("adjective", "positive"),
#                      " evidence for their secondary findings.")
# l4negasub2 <- paste0("The authors' methods are both ", 
#                      joe("adjective", "negative"),
#                      " and ",
#                      joe("adjective", "negative", "ed"),
#                      ".")
# l4negasub3 <- paste0("The authors' insistence on the ",
#                      joe("noun", "positive", "ence"),
#                      " of their findings ignore nearly a decade of ",
#                      joe("adjective", "positive"),
#                      " work in an increasingly ",
#                      joe("adjective", "positive"),
#                      " field.")
# l4negasub4 <- paste0("The authors' conclusions are not only unsupported by their (",
#                      joe("adjective", "negative"),
#                      ") data, but they also appear to contradict it ",
#                      joe("adjective", "negative"),
#                      "ly.")
# 
# 
# l4nega <- paste0("The article's shortcomings are three-fold: 1) ",
#                  l4negasub1, " 2) ",
#                  l4negasub2, " 3) ",
#                  l4negasub3)
# l4negb <- paste0("The article has two ",
#                  joe("adjective", "negative"),
#                  " shortcomings: 1) ",
#                  l4negasub4, " 2) ",
#                  l4negasub2)
# 
# l4posasub4 <- paste0("the authors' ",
#                      joe("adjective", "positive", neg_reg = "ly"),
#                      " analysis and ",
#                      joe("adjective", "positive", neg_reg = "ly"),
#                      " interpretation of their results more than adequately compensate ",
#                      "for the ",
#                      joe("noun", "negative", "ence"), 
#                      " of their study design.")
# 
# l4posa <- paste0(l4negasub3,
#                  " That said, ",
#                  l4posasub4)
# l4posb <- paste0("The authors'",
#                  joe("adjective", "positive", neg_reg = "ly"),
#                   " analysis and ",
#                   joe("adjective", "positive", neg_reg = "ly"),
#                   " interpretation of their results more than adequately compensate ",
#                   "for the ",
#                   joe("noun", "negative", "ence"), 
#                   " of their study design.  That said, it is worth pointing out ",
#                  "2 noteworthy problems: ",
#                  "1) ",
#                  l4negasub4, 
#                  " 2) ",
#                  l4negasub3,
#                  " These criticsms should not detract from ", 
#                  author,
#                  "'s ",
#                  "overall message (which I found to be ",
#                  joe("adjective", "positive"),
#                  ") or the article's ",
#                  joe("noun", "positive", "est"),
#                  ".")
# 
# if(sentiment == "negative"){
#   line4 <- sample(c(l4nega, l4negb), 1)
# } else{
#   line4 <- sample(c(l4posa, l4posb), 1)
# }
# 
# ############
# l5nega <- paste0("I sincerely hope that ",
#                  author,
#                  " will reconsider their conclusions, and that in the future ",
#                  journal,
#                  " will be a bit more careful in selecting articles they deem ",
#                  joe("adjective", "positive"),
#                  " enough for publication.")
# l5negb <- paste0("Though this article contains elements of ",
#                  joe("adjective", "positive"),
#                  " insight, ",
#                  author,
#                  " muddled their message with a ",
#                  joe("adjective", "negative"),
#                  "ly ", 
#                  joe("adjective", "negative"),
#                  " analysis of otherwise ",
#                  joe("adjective", "positive"),
#                  " data. ")
# l5negc <- paste0("I encourage ",
#                  author,
#                  " to withdraw their paper from publication, given the ",
#                  joe("adjective", "negative"), 
#                  " flaws in the design, analysis and interpretation of",
#                  " what otherwise could have been a ",
#                  joe("adjective", "positive"), 
#                  " contribution to our field.")
# l5negd <- paste0("J'accuse ",
#                  author,
#                  " of ",
#                  joe("adjective", "negative"), " ",
#                  joe("noun", "negative", "ism"),
#                  " and implore ",
#                  journal,
#                  " to withdraw this article's publication entirely.")
# 
# l5posa <- paste0("I applaud ",
#                  author,
#                  " for their ",
#                  joe("adjective", "positive"),
#                  " contribution to our field, ",
#                  " and encourage others to follow their example.")
# l5posb <- paste0(author,
#                  "'s study represents a ",
#                  joe("adjective", "positive"),
#                  " and ",
#                  joe("adjective", "positive"),
#                  " shift in the way epidemiologists ",
#                  joe("verb", "positive", "ize"),
#                  " our understanding of ",
#                  joe("noun", "positive", "ism"),
#                  ".")
# l5posc <- paste0(author,
#                  "'s most serious shortcoming was that they did not fully ",
#                  joe("verb", "positive", "ize"), 
#                  " the ",
#                  joe("noun", "positive", "ance"),
#                  " of their findings.")
# l5posd <- paste0("'",
#                   title,
#                   "'",
#                   " deserves widespread dissemination for its ",
#                   joe("verb", "positive"),
#                   "ly ",
#                   joe("adjective", "positive"),
#                   " implications for future studies of the population in question.")
# 
# l5pos1 <- sample(c(l5posb, l5posc), 1)
# l5pos2 <- sample(c(l5posa, l5posd), 1)
# 
# l5neg1 <- l5negb
# l5neg2 <- sample(c(l5nega, l5negc, l5negd), 1)
# 
# if(sentiment == "negative"){
#   line5 <- paste0(l5neg1, l5neg2)
# } else{
#   line5 <- paste0(l5pos1, l5pos2)
# }
# 
# paste0(line1, line2, line3, line4, line5)

# Words from harvard
# words <- read.csv("harvard/words.csv")
# 
# # Make a part of speech (pos) column in words
# words$pos <- NA
# words$pos <- ifelse(grepl("noun|Noun|NOUN", words$Othtags) &
#                       !grepl("pro|PRO|Pro", words$Othtags), "noun",
#                     ifelse(grepl("verb|VERB|Verb|SUPV", words$Othtags) &
#                              !grepl("ad|AD|Ad", words$Othtags), "verb",
#                            ifelse(grepl("ADJ|Adj|adj|Modif", words$Othtags), "adjective",
#                                   NA)))
# words$word <- words$Entry
# words <- words[,c("word", "pos", "Positiv", "Negativ")]
# words <- words[which(!is.na(words$pos)),]
# 

# words$sentiment <- ifelse(words$Positiv == "Positiv", "positive", 
#                           ifelse(words$Negativ == "Negativ", "negative", NA))
# words$word <- gsub("[0-9]", "", words$word) 
# words$word <- gsub("#", "", words$word)
# # Write csv
# write.csv(words, "harvard/joe_words.csv")
















# 
# 
# 
# # Read in negative / positive words
# #Read in Hu and Liu's opinion lexicon
# hu.liu.pos = scan("negAndPosWords/positive-words.txt",
#                   what="character", comment.char=";")
# hu.liu.neg = scan("negAndPosWords/negative-words.txt",
#                   what="character", comment.char=";")
# 
# #Add a few of my own words
# pos.words <- c(hu.liu.pos, "better")
# neg.words <- c(hu.liu.neg, "wait")





# link <- "http://ajph.aphapublications.org/toc/ajph/current"
# webPage <- readLines(link)
# webPage <- webPage[grepl("entryAuthor", webPage)][1]
# ts$pui[i] <- as.numeric(gsub(paste0("  <span class=\"nobr\"><span class=\"b\">",
#                                     "|", "</span>&nbsp;in</span>"), 
#                              "",
#                              webPage)) 