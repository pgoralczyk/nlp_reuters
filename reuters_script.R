setwd("d:/kaggle/reuters/")
dir()

library(reshape2)
library(scales)
library(tidytext)
library(dplyr)
library(lubridate)
library(ggplot2)

#preparing sentiments labels
get_s
get_sentiments("nrc") %>% 
  head()

# nrc_fear <- get_sentiments('nrc') %>%
#   filter(sentiment == 'fear')

data(stop_words)

flist <- c("reuters-newswire-2011.csv", "reuters-newswire-2012.csv",
           "reuters-newswire-2013.csv", "reuters-newswire-2014.csv",
           "reuters-newswire-2015.csv", "reuters-newswire-2016.csv",
           "reuters-newswire-2017.csv")

processReutersFile <- function(fname) {
  a <- read.csv(fname,
                header = TRUE,
                stringsAsFactors = FALSE)
  a <- a %>%
    mutate(pdt = as.Date(as.character(publish_time / 10000), "%Y%m%d"))
  words <- a %>% 
    unnest_tokens(word, headline_text) 
  words <- anti_join(words, stop_words, by = 'word')
  words <- left_join(words, get_sentiments("nrc"), by = "word")
  a11 <- a %>%
    group_by(pdt) %>%
    summarise(articles = n()) %>%
    left_join(
      group_by(words, pdt, sentiment) %>%
        summarise(words_count = n())
    ) %>%
    dcast(pdt + articles ~ sentiment, value.var = "words_count")
  return(a11)
}

head(f1)
f1 <- processReutersFile(flist[1])
f2 <- processReutersFile(flist[2])
f3 <- processReutersFile(flist[3])
f4 <- processReutersFile(flist[4])
f5 <- processReutersFile(flist[5])
f6 <- processReutersFile(flist[6])
f7 <- processReutersFile(flist[7])

dreut <- rbind(f1, f2, f3, f4, f5, f6, f7)

write.csv(dreut, "dreut.csv")

dreut <- mutate(dreut,
                wday = factor(weekdays(pdt),
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                         "Friday", "Saturday", "Sunday")),
                wkend = ifelse(wday %in% c("Friday", "Saturday", "Sunday"), 1, 0)
                )

dreut %>%
  select(pdt, articles) %>%
  unique() %>%
  filter(week(pdt) == 1 ) %>%
  ggplot(aes(x = weekdays(pdt),
             y = articles)) +
  geom_line(aes(group = 1)) 

ggplot(data = dreut,
       aes(x = wday,
           y = negative/articles)) +
  geom_boxplot() +
  facet_wrap(~year(pdt)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20)) +
  scale_y_continuous(labels = percent) +
  labs(y = "% of articles with negative sentiment")


#trend - excluding weekend
ggplot(data = filter(dreut, wday == 'Wednesday' ),
       aes(x = pdt,
           y = fear/articles)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))


filter(dreut, wday == 'Saturday' ) %>%
  mutate(positive_prop = positive / articles,
         negative_prop = negative / articles) %>%
  select(pdt, positive_prop, negative_prop) %>%
  melt(id.vars = "pdt") %>% 
  ggplot(aes(x = pdt,
             y = value,
             colour = variable)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  labs(title = "Positive and negative sentiment in news headlines",
       subtitles = "As proportion of all analysed articles",
       y = "% of all articles",
       x = "Date")


ggplot(data = filter(dreut, wday == 'Wednesday' ),
       aes(x = day(pdt),
           y = negative/articles)) +
  geom_line(aes(group = year(pdt))) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~month(pdt))


#sentiments...
factor(weekdays(dreut$pdt[1:10]),
       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                  "Friday", "Saturday", "Sunday"))

dreut %>%
  ggplot(aes(x = pdt,
             y = positive / articles)) +
  geom_line() +
  ylim(c(0, 1)) +
  theme_bw()



#yearly patterns?

#monthly, weekly?

#correlations
dreut[, c("anger", "anticipation")] 
dreut_n <- dreut[, c("anger", "anticipation", "disgust", "fear", 
          "joy", "sadness", "surprise", "trust")] / dreut$articles

