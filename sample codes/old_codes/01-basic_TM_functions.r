#########################################################################
#                             paste()
##########################################################################

a="dwight"; b="was"; c="here"
paste(a,b,c)   # default seperator is space
paste(a,b,c,sep="")
paste(a,b,c,sep=",")

#genrating sequence Ram1, Ram2 and so on
paste("Ram", seq(1:20), sep="") # we put nothing as seperator


##########################################################################
#                           strsplit()
##########################################################################

a="Today is a good day"

strsplit(a," ") # "Today" "is" "a" "good" "day"
strsplit(a,"") # "T" "o" "d" "a" "y" .... individual letters, split by blank

############################################################################
#                       substr() and sub()
############################################################################

x="Dwight Nuwan Fonseka"
substr(x,1,3)  # get 1st letter to 3rd letter
substr(x,1,3)="Dwa"   #"Dwi" replaced with "Dwa"
x

substr(x,1,6)   # "Dwight"
substr(x,8,12)  #  "Nuwan"
substr(x,13,20) # "Fonseka"


################################################################################
#                           nchar()
################################################################################

nchar(x)  # use above example
#returns number of characters


###############################################################################
#				gsub() and grep()
###############################################################################

x = "dwight is a hero"
gsub("dwight", "dwayne", x)

y=c("Dwight", "is", "a", "heroine")


grep("hero",x, value=F)   #>1       (we get the location if we put value=F)
grep("hero",y, value=F)   #>4

grep("hero",x, value=T)   #>"hero"  (we get the word itself containing "hero")
grep("hero",y, value=T)   #> heroine


#################################################################################
#                           working with strings
#################################################################################

mystring="NORTH KOREA fired a missile early on Tuesday that flew over Japan and landed in the Pacific waters off the northern region of Hokkaido, South Korea and Japan said, in a sharp escalation of tensions on the Korean peninsula. The United States, Japan and South Korea considered that launch to have been a ballistic missile test while NORTH KOREA said it was a rocket carrying a communications satellite into orbit. South Korea's military said the latest missile was launched from the Sunan region near the NORTH KOREA capital Pyongyang."

numstring="My password is '$%^&*%$@007'. I have 150 R scripts, 110 Python scripts and 20 Java scripts."


tolower(mystring)
toupper(mystring)
strsplit(mystring, " ") # split string after the space


############################### working with gsub ################################

gsub("Korea", "KOREA", mystring)
# gsub substitutes all the occurances of the "tweet" into "twitter"
# take not that only "tweet" and not "Tweet" is substituted
# it is case sensitive


sub("Korea", "KOREA", mystring)
# sub substitutes only the first occurance of "tweet" into "twitter"

gsub("Korea", "KOREA", mystring, ignore.case=T)
# all occurances of "tweet" and "Tweet" are substituted
# case sensitvity ignored

################### Advanced Methods ##################################


######## need to check the regular expression syntax for gsub
# to find the code  eg . "\\D" refers to digits and so on

#             \\d   >> digit
#             \\D   >> not digit
#             \\s   >> space
#             \\S   >> not space
#             \\w   >> word
#             \\W   >> not word
#             \\t   >> tab

gsub("\\d", "", numstring)
# replaces all digits with "" (blank)

gsub("\\D", "", numstring)
# replaces anything not with a digit with "" (blank)

gsub("\\s", "", numstring)
# replaces all the spaces with "" (blank)



gsub("[et]", "dwi", numstring)
# replaces alphabets "e" and "t" with "dwi"

gsub("[[:punct:]]","", numstring)
# removing punctuation

gsub("[^[:graph:]]","",numstring)
# removing anything but graphical characters

################# package stringr for more text manipulation

library(stringr)

str_c(c(mystring,numstring))
# adding two strings together

str_count(numstring, "s")
# count the alphabet "s" occurances

str_locate_all(numstring, "1")
# locate specific numbers or alphabet

str_replace(numstring, "1", "999")
# replcae similiar to gsub function

str_replace(numstring, "\\d", "")
# replace first digit occurance > similiar to sub

str_replace_all(numstring, "\\d", "")
# replaces all the numbers with blank >> like gsub

