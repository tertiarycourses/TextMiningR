#######################################################################
#                                Authentication
#######################################################################

library(Rfacebook)
library(Rook)
require(RCurl)
require(rjson)
library(jsonlite)
library(httpuv)
library(RColorBrewer)
library(magrittr)

#Get facebook for handle through this website : http://findmyfbid.com/

# option 1
# go to facebook developer page > tools & support & graph API explorer
# this key can expire > but can regenerate
APIkey="EAACEdEose0cBAMNuZCSfAisLhirwUUMF47YXBy3s0SvqYAgUoTC6zAdWigFMUb2bPRwNpWLzQrwg7wIhOnUNkEjcueGh02Cxjfk3TJq1jI94wQlK845ZA7UrkigZAaVrPgPj0vtaxZCOl30qZASFaRyRci2a2Apw51dBGwMb17ZCQpxKfNOAVZB1UURSEKzedYZD"


# option 2
app_id="236845553454666"
app_secret="7d452777f8c0c97546f9c09f75ef7381"
fb_oauth=fbOAuth(app_id="236845553454666", app_secret="7d452777f8c0c97546f9c09f75ef7381", extended_permissions = TRUE)
# a message will appear > copy the URL and go to setting of the facebook app. click
## settings tab on left side and choose "add platform" (at bottom)
# add the url in the fiel "site url" and save
save(fb_oauth, file="C://Users//user//Desktop//fb_oauth")
load("C://Users//user//Desktop//fb_oauth")




##################### analysing a facebook page ########################3

myfacebookpage="nike"

page=getPage(myfacebookpage, fb_oauth, n=5000)  # set n to high value to caputre all posts

page2=getPage(myfacebookpage, fb_oauth, n=5000, since='2017/01/01', until='2017/8/23')


# convert Facebook data metrics

format.facebook.date=function(datestring) {
                      date=as.POSIXct(datestring, format="%Y-%m-%dT%H:%M:%S+0000", tz="GMT")
}

aggregate.metrics=function(metric){
                     m=aggregate(page[[paste0(metric, "_count")]], list(month=page$month),mean)
                     m$month=as.Date(paste0(m$month, "-15"))
                     m$metric=metric
                     return(m)
}

# create a data frame
page$datetime=format.facebook.date(page$created_time)
page$month=format(page$datetime, "%Y-%m")
df.list=lapply(c("likes","comments","shares"), aggregate.metrics)
df=do.call(rbind, df.list)

# visualize results
library(ggplot2)
library(scales)
ggplot(df, aes(x=month, y=x, group=metric)) + geom_line(aes(color=metric)) + scale_x_date(date_breaks="years", labels=date_format("%Y")) + scale_y_log10("Avearge count per post", breaks=c(10,100,1000,10000,50000))+ theme_bw() + theme(axis.title.x= element_blank())


############################ analysing posts #################################

post_id=head(page$id, n=1)  # get ID of most recent post
post=getPost(post_id, fb_oauth, n=1000, likes=TRUE, comments=FALSE)
# this collects a list of 1000 users who like the most recent post > gender, language, country


# unless the users have public profiles, most of their information will be returned as NA
page_posts <- getPage(page="nike", token=fb_oauth, n=20, feed=TRUE)
other_users <- getUsers(page_posts$from_id, token=fb_oauth, private_info=TRUE)


###### scapping comments


# if you wish to find the data and focus on particlar topic or keyword
subset_data=subset(page, grepl("sport", page$message))

#### function to scrape all the comments on a page

test= list()

for (i in 1:length(page$id)){
  test[[i]]=getPost(post=page$id[i], token = fb_oauth, comments = TRUE, likes = FALSE)
  if (nrow(test[[i]][["comments"]]) > 0) {
  write.csv(test[[i]], file = paste0("test", i, ".csv"), row.names = F)
   }
}

df <- do.call("rbind", lapply(test, as.data.frame))
