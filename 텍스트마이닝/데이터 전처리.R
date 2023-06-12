library(dplyr)
library(stringr)



New <- News
New <- New$text %>% ifelse(str_detect(.,"기자 ="),str_extract_all(.,"기자 =(.*)"),.) %>% gsub("기자","",.) %>% 
  str_replace_all(.,pattern="[^가-힣]"," ")  %>% str_squish()%>% as_tibble()
New_s <- distinct(New)
New_s
