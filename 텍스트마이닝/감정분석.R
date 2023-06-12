library(readr)
library(textclean)
dic <- read_csv("knu_sentiment_lexicon.csv")
New_emo <- New_s %>% mutate(id=row_number())
New_emo <- New_emo %>% unnest_tokens(input = value,output = word,token = "words",drop = F)
New_emo <- New_emo %>% left_join(dic,by="word") %>% mutate(polarity=ifelse(is.na(polarity),0,polarity))
New_emo <- New_emo %>% group_by(id,value) %>% summarise(score=sum(polarity)) %>% ungroup()

New_emo %>% select(score,value) %>% arrange(-score)
New_emo %>% select(score,value) %>% arrange(score)

freq_emo <-  New_emo %>% mutate(sentiment=ifelse(score>=1,"pos",ifelse(score<=-1,"neg","neu")))
#시각화
freq_emo <-  freq_emo %>% count(sentiment) %>% mutate(ratio=n/sum(n)*100)
ggplot(freq_emo,aes(x=sentiment,y=n,fill=sentiment))+geom_col()+geom_text(aes(label=n),vjust=-0.3)+
  scale_x_discrete(limits=c("pos","neu","neg"))
