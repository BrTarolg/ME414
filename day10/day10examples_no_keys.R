## Code for API examples from Day 10, Social Media
rm(list = ls())

## ########################
## Libraries
## ########################

library(quanteda)
library(twitteR)
library(Rfacebook)
library(twfyR)

## ########################
## Facebook
## ########################

# Access token: https://developers.facebook.com/tools/explorer/
#browseURL("https://developers.facebook.com/tools/explorer/")
token <- "XXXXXXX"

# Store user names
obama <- "barackobama"
trump <- "DonaldTrump"

## Scraping most recent 200 posts from Obama and McCain FB pages
obama <- getPage(obama, token = token, n = 150)
trump <- getPage(trump, token = token, n = 150)

## Create a corpus object
fbdata <- rbind(obama, trump)
fbcorpus <- corpus(fbdata$message, docvars = fbdata[, -which(names(fbdata)=="message")])

## Key Words in Context
kwic(corpus_subset(fbcorpus, subset = docvars(fbcorpus)$from_name == "Barack Obama"), "america")
kwic(corpus_subset(fbcorpus, subset = docvars(fbcorpus)$from_name == "Donald J. Trump"), "america")

## Create a DFM
fbDfm <- dfm(fbcorpus, remove = c(stopwords("english"), "ofa.bo", "http"), stem=TRUE, verbose = FALSE, groups = "from_name", remove_punct = T)

# viewing the dfm using word clouds
pdf("obamaVtrumpFacebook.pdf", width = 9, height = 9)
textplot_wordcloud(fbDfm, comparison = T, remove_symbols = T, remove_punct = T)
dev.off()
system("open obamaVtrumpFacebook.pdf")

## ########################
## Twitter
## ########################

## data for authentication
## get your keys at apps.twitter.com

key <- "XXXXXXXXX"
cons_secret <- "XXXXXXXXX"
token <- "XXXXXXXXX"
access_secret <- "XXXXXXXXX"

# Authorize the Twitter access
setup_twitter_oauth(key, cons_secret, token, access_secret)

# downloading 500 most recent tweets for an account
tw <- userTimeline("realDonaldTrump", n = 500, includeRts = TRUE)
twDf <- twListToDF(tw)

# Create corpus object
twCorpus <- corpus(twDf$text, docvars = twDf[, -which(names(twDf)=="text")])

# How many of Trump's last 500 tweets were retweets?
table(twCorpus[["isRetweet"]], useNA = "ifany")

## Create DFM object
twDfm <- dfm(twCorpus,  remove = c("amp", "rt", "https", "t.co", "will", "@realdonaldtrump", stopwords("english"),"#*", "@*","t.co"), remove_symbols = T, remove_punct = T)

textplot_wordcloud(twDfm, min.freq = 5, random.order = FALSE)

## Analyze just hashtags
twDfmHT <- dfm(twCorpus, select = c("#*"))
textplot_wordcloud(twDfmHT, min.freq = 2, random.order = FALSE)

## analyze just usernames
twDfmUN <- dfm(twCorpus, select = c("@*"))
textplot_wordcloud(twDfmUN, min.freq = 1, random.order = FALSE)


## Analyse metadata
twTrump <- getUser("realDonaldTrump")

# How many followers does Trump have?
twTrump$followersCount

# How many people does Trump follow?
twTrump$friendsCount

# Who does Trump follow?
trumpFollows <- twTrump$getFriends()
as.character(unlist(lapply(trumpFollows, function(x) x$name)))

# Why does Trump follow Piers Morgan?
piersTw <- userTimeline("piersmorgan", n = 200, includeRts = F)
piersTwDf <- twListToDF(piersTw)
piersTwCorpus <- corpus(piersTwDf$text, docvars = piersTwDf[, -which(names(piersTwDf)=="text")])
kwic(piersTwCorpus, "trump")


## #########################
## Searching using REST API
## #########################

searchTweets <- searchTwitter("brexit", n = 1000, since = "2017-01-01")
searchTweetsDf <- twListToDF(searchTweets)
searchTweetsCorpus <- corpus(searchTweetsDf$text, docvars = searchTweetsDf[, -which(names(searchTweetsDf)=="text")])
searchTweetsDfm <- dfm(searchTweetsCorpus, verbose = FALSE, 
                       remove = c("amp", "rt", "https", "t.co", "will", stopwords("english")), remove_punct = T, remove_symbols = T)

topfeatures(searchTweetsDfm, 30)
topfeatures(dfm_select(searchTweetsDfm, "#*"), 30)


## #########################
## Using the twfy API
## #########################

## Set API key
api_key <- "XXXXXXXXX"
set_twfy_key(api_key)

# Get speeches for Dianne Abbott

speeches <- getDebates(person = 10001, type = "commons")
class(speeches)
dim(speeches)

## Convert to corpus
speech_corpus <- corpus(speeches$body)

## Convert to dfm
speech_dfm <- dfm(speech_corpus, remove = c("will", stopwords("english")),
                  remove_punct = TRUE)

## Trim dfm
speech_dfm <- dfm_trim(speech_dfm, max_count = 400, min_count = 10)

## Convert dfm to tfidf weights
speech_idf <- tfidf(speech_dfm)

## Plot words
textplot_wordcloud(speech_idf, min.req = 10, random.order = FALSE, rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
