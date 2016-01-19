# Input tweets for the 4 Names with an additinal tag of FIFA

library(twitteR)
library(ggplot2)
library (plyr)
library (stringr)


ronaldotags <- c('#cr7', '#cristianoronaldo', '#ronaldo')

ronaldo.list <- vector()
for (tag in ronaldotags){
  tweets <- unique(searchTwitter(tag, n=10))
  ronaldo.list <- union(ronaldo.list, strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)) 
}

ronaldo.df = twListToDF(ronaldo.list)  

messitags <- c('#messi', '#leomessi', '#lionelmessi')

messi.list <- vector()
for (tag in messitags){
  tweets <- unique(searchTwitter(tag, n=10))
  messi.list <- union(messi.list, strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)) 
}

messi.df = twListToDF(messi.list)  

source('~/GITHUB/twitter-miner/sentiscore.R')

# Convert text to factor
ronaldo.df$text <- as.factor(ronaldo.df$text)
messi.df$text <- as.factor(messi.df$text)

write.csv(ronaldo.df, file="ronaldo.csv", row.names=FALSE)
write.csv(messi.df, file="messi.csv", row.names=FALSE)
ronaldo.df=read.csv("ronaldo.csv")
messi.df=read.csv("messi.csv")

pos.words <- scan('data/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
neg.words <- scan('data/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')


# Calculate all the scores
ronaldo.scores = score.sentiment(ronaldo.df$text, pos.words, neg.words, .progress='text')
messi.scores = score.sentiment(messi.df$text, pos.words, neg.words, .progress='text')

ronaldo.scores$Name = 'Cristiano Ronaldo'
messi.scores$Name = 'Lionel Messi'


# Final outputs
hist(ronaldo.scores$score)
hist(messi.scores$score)

table(ronaldo.scores$score)
table(messi.scores$score)

head(all.scores)
all.scores = rbind(ronaldo.scores, messi.scores)

table(all.scores$score,all.scores$Name)

ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_bar(mapping=aes(x=score, fill=Name), binwidth=1) +
  facet_grid(Name~.) + # make a separate plot for each hashtag
  theme_bw() + scale_fill_brewer() # plain display, nicer colors
