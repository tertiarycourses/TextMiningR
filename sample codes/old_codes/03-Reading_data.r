################################# quick pre-reading ##########################
library(readr)

### > data can be form blogs, emails, documents  OR social media data


read_lines() # returns a vector of strings representing each line in the file
             # reads line by line, not entire corpus, 

read_file() # reads entire file as one vector, with breaks represented as \n


# when copying from internet to text file > save as UTF-8

art1=read_lines(file.choose())

art2=read_file(file.choose())

art3=read.csv(file.choose())

# convert to corpus

library(tm)
library(SnowballC)
textCorpus=Corpus(VectorSource(art1))  # convert to corpus

textCorpus  # view
inspect(textCorpus[[1]]) # view first line

textCorpus=tm_map(textCorpus, tolower) # convert all to lower
textCorpus=tm_map(textCorpus, removePunctuation)
textCorpus=tm_map(textCorpus, removeNumbers)
textCorpus=tm_map(textCorpus, removeWords, stopwords("english")) # remove common words
textCorpus=tm_map(textCorpus, removeWords,c("RT @","dwayne"))  # remove user defined words
textCorpus=tm_map(textCorpus, stemDocument)
textCorpus=tm_map(textCorpus, stripWhitespace)
#textCorpus=tm_map(textCorpus, PlainTextDocument)

# can check inspect(textCorpus[[1]]) again


textTdm=TermDocumentMatrix(textCorpus)   # to convert to tdm
textTdm



#################################################################################################
#				reading files from folders
################################################################################################# 


# folder with 1000s of PDFs
pdfdest = "C://Users//user//Desktop//testing//pdf//"
docxdest= "C://Users//user//Desktop//testing//docx//"
htmldest="C://Users//user//Desktop//testing//html//"
txtdest="C://Users//user//Desktop//testing//txt//"


# make a vector of PDF file names
mypdffiles = list.files(path = pdfdest, pattern = "pdf",  full.names = TRUE)
mydocxfiles = list.files(path = docxdest, pattern = "docx",  full.names = TRUE)
myhtmlfiles = list.files(path = htmldest, pattern="\\.(htm|html)$",  full.names = TRUE)

#################################### convert PDF to corpus ##############################

# convert each PDF file that is named in the vector into a text file 
# text file is created in the same directory as the PDFs
# note that my pdftotext.exe is in a different location to yours
# get from http://www.foolabs.com/xpdf/download.html

lapply(mypdffiles, function(i) system(paste('"C:/Program Files/xpdf/bin64/pdftotext.exe"', 
             paste0('"', i, '"')), wait = FALSE) )


library(tm)
              
txtfilelist=list.files(pdfdest, pattern = ".txt")

filenames2=paste(pdfdest,txtfilelist[1:length(txtfilelist)],sep="")
newfiles2=paste(txtdest,txtfilelist[1:length(txtfilelist)],sep="")
file.rename(filenames2,newfiles2)

# convert to corpus
pdfCor = Corpus(DirSource(txtdest), 
              readerControl=list(reader=readPlain, 
                                 load=TRUE));


pdfCor=tm_map(pdfCor, tolower) # convert all to lower
pdfCor=tm_map(pdfCor, removePunctuation)
pdfCor=tm_map(pdfCor, removeNumbers)
pdfCor=tm_map(pdfCor, removeWords, stopwords("english")) # remove common words
pdfCor=tm_map(pdfCor, removeWords,c("dwight","dwayne"))  # remove user defined words
pdfCor=tm_map(pdfCor, stemDocument)
pdfCor=tm_map(pdfCor, stripWhitespace)
pdfCor=tm_map(pdfCor, PlainTextDocument)

# see files in corpus
inspect(pdfCor)
# see content of corpus
pdfCor[[1]]$content

################################# convert HTML to corpus ####################################

library(RCurl)
library(XML)
library(tm)

# myfile.txt contains the website addresses we want to scrape
# very difficult with websites with https://  >>> http:// (should be OK)
urlList = list();
urlList = readLines(file.choose(), warn=FALSE)   # look for url.txt under datasets


#library(XML)
#doc=xmlTreeParse(file.choose(),useInternal=TRUE)   # look for breakfast under dataasets
#rootNode=xmlRoot(doc)

htmlParse=function(url){
library(rvest)
url2=read_html(url)  
doc.html = htmlTreeParse(url2,useInternalNodes = TRUE)
doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
doc.text = gsub('\\n', ' ', doc.text)
doc.text = paste(doc.text, collapse = ' ')
return(doc.text)
}

# evaluate input and convert to text

htmlOutput=lapply(urlList, htmlParse)
htmlCor= as.VCorpus(htmlOutput)

htmlCor=tm_map(htmlCor, tolower) # convert all to lower
htmlCor=tm_map(htmlCor, removePunctuation)
htmlCor=tm_map(htmlCor, removeNumbers)
htmlCor=tm_map(htmlCor, removeWords, stopwords("english")) # remove common words
htmlCor=tm_map(htmlCor, removeWords,c("dwight","dwayne"))  # remove user defined words
htmlCor=tm_map(htmlCor, stemDocument)
htmlCor=tm_map(htmlCor, stripWhitespace)
htmlCor=tm_map(htmlCor, PlainTextDocument)

# see files in corpus
inspect(htmlCor)
# see content of corpus
htmlCor[[1]]$content

################################# convert DOCX into corpus #########################################

library(qdapTools)
data=lapply(mydocxfiles, read_docx)

docxCor= as.VCorpus(data)

docxCor=tm_map(docxCor, tolower) # convert all to lower
docxCor=tm_map(docxCor, removePunctuation)
docxCor=tm_map(docxCor, removeNumbers)
docxCor=tm_map(docxCor, removeWords, stopwords("english")) # remove common words
docxCor=tm_map(docxCor, removeWords,c("dwight","dwayne"))  # remove user defined words
docxCor=tm_map(docxCor, stemDocument)
docxCor=tm_map(docxCor, stripWhitespace)
docxCor=tm_map(docxCor, PlainTextDocument)

# see files in corpus
inspect(docxCor)
# see content of corpus
docxCor[[1]]$content


########################################################################################################
#					reading files from internet
########################################################################################################

################################ read XML files
library(XML)
doc=xmlTreeParse(file.choose(),useInternal=TRUE)   # look for breakfast under dataasets
rootNode=xmlRoot(doc)

xmlName(rootNode)
names(rootNode)
rootNode[[1]]
rootNode[[1]][[1]]

# loop and give the xmlValue to each element
xmlSApply(rootNode, xmlValue)


names=xpathSApply(rootNode,"//name",xmlValue)   
price=xpathSApply(rootNode,"//price",xmlValue)

names
price


################################ read JSON files
library(jsonlite)
data=fromJSON(file.choose())  # look for iris.json under datasets
names(data)
names(data$owner)
names(data$owner$id)


################################# reading tables from websites

library(rvest)
library(dplyr)

url="https://www.google.com/finance?q=hal&ei=oAlqWcnPMYXtuAS14br4BA"
data1=read_html(url)%>% html_node("table.snap-data") %>% html_text %>% 
  gsub("\n"," ",data1)

library(XML)
url="http://finviz.com/quote.ashx?t=hal"
allTables=readHTMLTable(url)
length(allTables)
tblHAL=allTables[9]  # the table we want




####################### HTML to text #############################

htmlToText <- function(input, ...) {
  require(RCurl)
  require(XML)
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    return(NULL)
  }
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }

  html.list <- lapply(input, evaluate_input)
  text.list <- lapply(html.list, convert_html_to_text)
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}

