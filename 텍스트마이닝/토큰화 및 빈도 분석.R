library(KoNLP)
library(tidytext)
library(ggwordcloud)
library(showtext)

New_token <- New_s %>% unnest_tokens(input = value,output = word,
                                     token = extractNoun)
New_token <- New_token %>% count(word,sort=T) %>% filter(str_count(word)>1)
#전세라는 단어는 해당 키워드이기에 제외함 + 데이터수가 너무 많아 300개로 줄임
New_token <- New_token %>% filter(New_token$word!="전세") %>% head(300)

font_add_google(name="Nanum Gothic",family = "NG")
showtext_auto()
#시각화
ggplot(New_token,aes(label=word,size=n,col=n))+
  geom_text_wordcloud(seed = 1234,family="NG")+
  scale_radius(limits = c(1,NA),range = c(1,20))+
  scale_color_gradient(low="#1E90FF",high = "#000080")+
  theme_minimal()
