library(twitteR)
library(maps)
library(plyr)
library(stringr)
library(bitops)
library(scales)

#Cities to analyze
cities=data.frame(
  CITY=c('Edinburgh', 'London', 'Glasgow', 'Birmingham', 'Liverpool', 'Manchester',
         'New York', 'Washington', 'Las Vegas', 'San Francisco', 'Chicago','Los Angeles'),
  COUNTRY=c("UK", "UK", "UK", "UK", "UK", "UK", "USA", "USA", "USA", "USA", "USA", "USA"))
data(world.cities)
cities2=world.cities[which(!is.na(match(
str_trim(paste(world.cities$name, world.cities$country.etc, sep=",")),
str_trim(paste(cities$CITY, cities$COUNTRY, sep=","))
))),]
cities2$SEARCH=paste(cities2$lat, cities2$long, "10mi", sep = ",")
cities2$CITY=cities2$name
#Download tweets
tweets=data.frame()
for (i in 1:nrow(cities2))
{
  tw=searchTwitter("I FEEL", n=26, geocode=cities2[i,]$SEARCH)
  tweets=rbind(merge(cities[i,], twListToDF(tw),all=TRUE), tweets)
}
#Save tweets
write.csv(tweets, file="tweets.csv", row.names=FALSE)
#Import csv file
city.tweets=read.csv("tweets.csv")
#Download lexicon from http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar
hu.liu.pos = scan('data/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('data/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
#Function to clean and score tweets
source('~/GITHUB/twitter-miner/sentiscore.R')

cities.scores=score.sentiment(city.tweets[1:nrow(city.tweets),], hu.liu.pos, hu.liu.neg, .progress='text')
cities.scores$pos2=apply(cities.scores, 1, function(x) regexpr(",",x[2])[1]-1)
cities.scores$CITY=apply(cities.scores, 1, function(x) substr(x[2], 1, x[3]))
cities.scores=merge(x=cities.scores, y=cities, by='CITY')
df1=aggregate(cities.scores["score"], by=cities.scores, FUN=length)
names(df1)=c("CITY", "TWEETS")
cities.scores2=cities.scores[abs(cities.scores$score)>0,]
df2=aggregate(cities.scores2["score"], by=cities.scores2, FUN=length)
names(df2)=c("CITY", "TWEETS.SENT")
df3=aggregate(cities.scores2["score"], by=cities.scores2, FUN=mean)
names(df3)=c("CITY", "TWEETS.SENT.SCORING")
#Data frame with results
df.result=join_all(list(df1,df2,df3,cities2), by = 'CITY', type='full')
#Plot results
radius <- sqrt(df.result$pop/pi)
symbols(100*df.result$TWEETS.SENT/df.result$TWEETS, df.result$TWEETS.SENT.SCORING, circles=radius,
        inches=0.85, fg="white", bg="gold", xlab="Sentimental Tweets", ylab="Scoring Of Sentimental Tweets (Average)",
        main="How Do Cities Feel?")
text(100*df.result$TWEETS.SENT/df.result$TWEETS, df.result$TWEETS.SENT.SCORING, paste(df.result$CITY, df.result$country.etc, sep="-"), cex=1, col="gray50")