New_lpd <- New_s %>% mutate(id=row_number())
New_lpd <- New_lpd %>% unnest_tokens(input = value,output = word,token = extractNoun,drop = F) %>% 
  filter(str_count(word)>1) %>% group_by(id) %>% distinct(word,.keep_all = T) %>% ungroup() %>% 
  select(id,word)
count_lpd <- New_lpd %>% add_count(word) %>% filter(n<=200) %>% select(-n)
count_lpd %>% count(word,sort = T) %>% print(n=200)
stopword <- c("관련",'경우','하게','하기','들이','하지','하다','때문','이날','가운데')
count_lpd <- count_lpd %>% filter( !word%in%stopword)
count_lpd_doc <- count_lpd %>% count(id,word,sort=T)
library(tm)
library(topicmodels)
library(textclean)
dtm_comment <- count_lpd_doc %>% cast_dtm(document = id,term = word,value = n)
lda_model <- LDA(dtm_comment,k=8,method = "Gibbs",control = list(seed=1234))
term_topic <- tidy(lda_model,matrix='beta')
term_topic
terms(lda_model,10)

doc_topic <- tidy(lda_model,matrix='gamma')
doc_class <- doc_topic %>% group_by(document) %>% slice_max(gamma,n=1)
doc_class$document <- as.integer(doc_class$document)
doc_class %>% arrange(document)
new1 <- News %>% mutate(id=row_number())
news_comment_topic <- new1 %>% left_join(doc_class,by=c('id'='document'))
news_comment_topic %>% select(id,topic)
news_comment_topic <- news_comment_topic %>% na.omit()
news_comment_topic %>% select(id,topic)
news_comment_topic
top_term <- term_topic %>% group_by(topic) %>% slice_max(beta,n=6,with_ties = F) %>% 
  summarise(term=paste(term,collapse=", "))

top_term

count_topic <- news_comment_topic %>% count(topic)
count_topic_word <- count_topic %>% left_join(top_term,by='topic') %>% 
  mutate(topic_name=paste("TOPIC",topic))
count_topic_word

ggplot(count_topic_word,aes(x=reorder(topic_name,n),y=n,fill=topic_name))+
  geom_col(show.legend = F)+
  coord_flip()+
  geom_text(aes(label=n),hjust=-0.2)+
  geom_text(aes(label=term),
            hjust=1.05,
            col='white',
            fontface='bold')+
  scale_y_continuous(limits = c(0,25))+
  labs(x=NULL)

comment_topic <- news_comment_topic %>% mutate(text=str_squish(replace_html(text))) %>% 
  arrange(-gamma)
comment_topic %>%filter(topic==8)
