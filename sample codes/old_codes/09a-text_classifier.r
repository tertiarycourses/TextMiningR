###### text extractor

MONKEYLEARN_KEY="92ea330047a7afe2dabd64fe87e7e22d922f302e"

devtools::install_github("ropensci/monkeylearn")
library("monkeylearn")

#you can get the code for the entirty extractor from the website
#below is one example

browseURL("https://app.monkeylearn.com/main/extractors/ex_isnnZRbS/tab/api-tab/")

text <- "In the 19th century, the major European powers had gone to great lengths to maintain a balance of power throughout Europe, 
resulting in the existence of a complex network of political and military alliances throughout the continent by 1900.
[7] These had started in 1815, with the Holy Alliance between Prussia, Russia, and Austria. 
Then, in October 1873, German Chancellor Otto von Bismarck negotiated the League of the Three Emperors 
(German: Dreikaiserbund) between the monarchs of Austria-Hungary, Russia and Germany."


#text=readLines(file.choose())
#text=text[1:10]

### location
output = monkeylearn_extract(request = text, key=MONKEYLEARN_KEY, extractor_id = "ex_isnnZRbS")

output

attr(output, "headers")


#### key words

# If the documentation of the extractor you use states it has parameters,
#  you can pass them as a named list, see below.

text <- "A panel of Goldman Sachs employees spent a recent Tuesday night at the
Columbia University faculty club trying to convince a packed room of potential
recruits that Wall Street, not Silicon Valley, was the place to be for computer
scientists.\n\n The Goldman employees knew they had an uphill battle. They were
 fighting against perceptions of Wall Street as boring and regulation-bound and
 Silicon Valley as the promised land of flip-flops, beanbag chairs and million-dollar
  stock options.\n\n Their argument to the room of technologically inclined students
  was that Wall Street was where they could find far more challenging, diverse and,
   yes, lucrative jobs working on some of the worlds most difficult technical problems.\n\n
   Whereas in other opportunities you might be considering, it is working one type of data
   or one type of application, we deal in hundreds of products in hundreds of markets, with
    thousands or tens of thousands of clients, every day, millions of times of day worldwide,
     Afsheen Afshar, a managing director at Goldman Sachs, told the students."


# You can find extractors and their IDs


output = monkeylearn_extract(text, key=MONKEYLEARN_KEY,
                              extractor_id = "ex_y7BPYzNG",
                              params = list(max_keywords = 3))
output




output2 = monkeylearn_extract(text, key=MONKEYLEARN_KEY,
                              extractor_id = "ex_y7BPYzNG",
                              params = list(max_keywords = 1))
output2

attr(output2, "headers")


### Important info

text <- "Hi, my email is john@example.com and my credit card is 4242-4242-4242-4242 so you can charge me with $10. My phone number is 15555 9876. We can get in touch on April 16, at 10:00am"
text2 <- "Hi, my email is mary@example.com and my credit card is 4242-4232-4242-4242. My phone number is 16655 9876. We can get in touch on April 16, at 10:00am"

output3 <- monkeylearn_extract(request = c(text, text2),key=MONKEYLEARN_KEY, extractor_id = "ex_dqRio5sG")
output3

## Topics

text1 <- "my dog is an avid rice eater"
text2 <- "i want to buy an iphone"
request <- c(text1, text2)
monkeylearn_classify(request, key=MONKEYLEARN_KEY,
                     classifier_id = "cl_oFKL5wft")

monkeylearn_classifiers(private = FALSE)


## language detection

text1 <- "Hauràs de dirigir-te al punt de trobada del grup al que et vulguis unir."
text2 <- "i want to buy an iphone"
text3 <- "Je déteste ne plus avoir de dentifrice."
request <- c(text1, text2, text3)
monkeylearn_classify(request, key=MONKEYLEARN_KEY,
                     classifier_id = "cl_oJNMkt2V")


## general topic classifier

text1 <- "Let me tell you about my dog and my cat. They are really friendly and like going on walks. They both like chasing mice."
text2 <- "My first R package was probably a disaster but I keep learning how to program."
request <- c(text1, text2)
monkeylearn_classify(request, key=MONKEYLEARN_KEY,
                     classifier_id = "cl_5icAVzKR")
