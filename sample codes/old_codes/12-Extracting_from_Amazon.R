library(stringr)
library(utils)  


Amazon = function(url,        # first click on show all or top review
                    n )       # Number of pages to extarct
{           
  text_page=character(0)   # define blank file
  pb <- txtProgressBar()    # define progress bar
  url = unlist(str_split(url,"&"))[1]
  for(i in 0:n){           # loop for url
    p =i*10	
    e = "&rating=1,2,3,4,5&reviewers=all&type=all&sort=most_helpful&start="
    url0 = paste(url,e,p,sep="")           # url in correct format
    text = readLines(url0)     # Read URL       
    text_start = grep("a-size-base review-text",text)   # review start marker
    text_stop = grep("a-row a-spacing-top-small review-comments", text)  # review end marker
    setTxtProgressBar(pb, i)             # print progress bar
    if (length(text_start) == 0) break    # check for loop termination, i.e valid page found      
    for(j in 1:length(text_start))        # Consolidate all reviews     
    {
      text_temp = paste(paste(text[(text_start[j]+1):(text_stop[j])]),collapse=" ")
      text_page = c(text_page,text_temp)
    }
    #Sys.sleep(1)
  }
  text_page =gsub("<.*?>", "", text_page)       # regex for Removing HTML character 
  text_page = gsub("^\\s+|\\s+$", "", text_page) # regex for removing leading and trailing white space
  return(text_page)       # return reviews
}


###### sample page

url = "http://www.amazon.in/Apple-iPhone-7-Black-32GB/product-reviews/B01LZKSVRB/ref=dpx_acr_txt?showViewpoints=1"
Hp = Amazon (url,5)
length(Hp)

Hp1 <- as.data.frame(Hp)
Hp2 <- write.table(Hp, 'C://Users//user//Desktop//Amazon_Hp.txt')


# go back to exercise 1 and repeat the process
