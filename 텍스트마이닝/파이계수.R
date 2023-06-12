library(widyr)
library(tidyr)
New_phi <- New_s %>% mutate(id=row_number())
New_phi <- New_phi %>% unnest_tokens(input=value,output = word,
                                     token = SimplePos22,drop=F)
New_phi <- New_phi %>% separate_rows(word,sep = "[+]")
noun <- New_phi %>% filter(str_detect(word,"/n")) %>% mutate(word=str_remove(word,"/.*$"))

pvpa <- New_phi %>% filter(str_detect(word,"/pa|pv")) %>% mutate(word=str_replace(word,"/.*$","다"))

New_phi <- bind_rows(noun,pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)

New_phi <- New_phi %>% add_count(word) %>% filter(n>=20) %>% 
  pairwise_cor(item = word,feature = id,sort=T)
New_phi

word_list <- c("피해자","임차인","임대인","공인중개사")
New_v <- New_phi %>% filter(item1%in%word_list) %>% group_by(item1) %>% 
  slice_max(correlation,n=10)
New_v$item1 <- factor(New_v$item1,levels = word_list)
ggplot(New_v,aes(x=reorder_within(item2,correlation,item1),y=correlation,
                 fill=item1))+
  geom_col(show.legend = F)+
  facet_wrap(~item1,scales = "free")+
  coord_flip()+
  scale_x_reordered()+
  labs(x=NULL)

