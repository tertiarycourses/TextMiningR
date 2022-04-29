library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)
library(magrittr)


#######################################################################
#                                Authentication
#######################################################################

key="Mblrycq3sF4JTIEfBDvzbSefr"
secret="sVSnam6T7fOCRDlig8tcbJTBMb1sHfYTL94Dfp5soFpbA4fxD8"
tktoken="446052007-HX8Q2RIYvd4YTYInwMvyDOxK41hZKBaMLTlFEl5e"
tksecret="Bt7Wkuxwj1qWFL0nFOp8GA25Rnw5iCjs892EVcBvt86qC"
cacert="C://Users//user//Desktop//tertiary//Rprogramming//RtextMining//httrOauth//cacert.pem"

setup_twitter_oauth(key,secret,tktoken,tksecret)


################################################################################
#			Searching Tweets
################################################################################

# search latest 10 tweets
myTweets=searchTwitter("Trump", n=100)  #can specify any length

class(myTweets)
length(myTweets)
head(myTweets)


for (i in 1:length(myTweets)) {
  print(paste("Tweet = ", myTweets[[i]]$getText()  ))
  print(paste("User = ",  myTweets[[i]]$getScreenName() ))
}


df <- do.call("rbind", lapply(myTweets, as.data.frame))
write.csv(df,file="twitterList.csv")
