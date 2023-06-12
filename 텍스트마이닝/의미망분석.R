library(tidygraph)
library(ggraph)
#의미망 분석
set.seed(1234)
graph_phi <- New_phi %>% filter(correlation>=0.58) %>% as_tbl_graph(directed = F) %>% 
  mutate(centrality=centrality_degree(),group=as.factor(group_infomap()))

set.seed(1234)
ggraph(graph_phi,layout = "fr")+
  geom_edge_link(color="gray40",
                 aes(edge_alpha=correlation,
                     edge_width=correlation),
                 show.legend = F)+
  scale_edge_width(range = c(1,4))+
  geom_node_point(aes(size=centrality,
                      color=group),
                  show.legend = F)+
  scale_size(range = c(5,10))+
  geom_node_text(aes(label=name),repel = T,size=5)+
  theme_graph()


#바이그램
New_bi <- New_s %>% mutate(id=row_number())
New_bi <- New_bi %>% unnest_tokens(input=value,output = word,
                                   token = SimplePos22,drop=F)
New_bi <- New_bi %>% separate_rows(word,sep = "[+]")
noun <- New_bi %>% filter(str_detect(word,"/n")) %>% mutate(word=str_remove(word,"/.*$"))

pvpa <- New_bi %>% filter(str_detect(word,"/pa|pv")) %>% mutate(word=str_replace(word,"/.*$","다"))

New_bi<- bind_rows(noun,pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)
New_bi <- New_bi %>% group_by(id) %>% summarise(sentence=paste(word,collapse = " "))
New_bi <- New_bi %>% unnest_tokens(sentence,output=bigram,token = "ngrams",n=2)
New_bi_sep <- New_bi %>% separate(bigram,c("word1","word2"),sep = " ")
pair_New_bi <- New_bi_sep %>% count(word1,word2,sort=T) %>% na.omit()

set.seed(1234)
graph_bigram <- pair_New_bi %>% filter(n>=13) %>% as_tbl_graph(directed = F) %>% mutate(centrality=centrality_degree(),
                                                                                        group=as.factor(group_infomap()))
ggraph(graph_bigram,layout = "fr")+geom_edge_link(color="gray50",alpha=0.5)+
  geom_node_point(aes(size=centrality,color=group),show.legend = F)+
  scale_size(range = c(4,8))+
  geom_node_text(aes(label=name),repel = T,size=5)+theme_graph()
