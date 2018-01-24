library(sentimentr)

docs=readLines(file.choose())

#dd="C://Users//user//Desktop//tertiary//Rprogramming//RtextMining//datasets//corpus"
#docs=DirSource(directory=dd)
#docs=DirSource(directory="path/to/your/directory")

sentiment(docs)
plot(sentiment(docs))

sentiment_by(docs)

get_sentences(docs)

extract_sentiment_terms(docs)


############################ creating boxplot

## example 1 > presidential candidates

president=read.csv(file.choose())   # choose the tweets hillary and trump
View(president)

president$text=as.character(president$text)
president_sentiments=sentiment(president$text)
View(president_sentiments)

nrow(president)
nrow(president_sentiments)   # the number of rows are not the same 
#> because each line has several sentences


new_president_sentiments=aggregate(president_sentiments$sentiment, list(president_sentiments$element_id), FUN=sum)
nrow(new_president_sentiments)


president$sentiments=new_president_sentiments$x  # add and extra column
View(president)

boxplot(president$sentiments~president$handle)


## example 2 > airlines

airlines=read.csv(file.choose())   # choose the tweets airlines
View(airlines)

airlines$text=as.character(airlines$text)
airlines_sentiments=sentiment(airlines$text)
View(airlines_sentiments)

nrow(airlines)
nrow(airlines_sentiments)   # the number of rows are not the same > because each line has several sentences


new_airlines_sentiments=aggregate(airlines_sentiments$sentiment, list(airlines_sentiments$element_id), FUN=sum)
nrow(new_airlines_sentiments)


airlines$sentiments=new_airlines_sentiments$x  # add and extra column
View(airlines)

boxplot(airlines$sentiments~airlines$airline)
