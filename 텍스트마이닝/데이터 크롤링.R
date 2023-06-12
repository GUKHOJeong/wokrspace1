library(XML)
library(rvest)
library(xml2)
# 네이버 검색창에서 원하는 키워드와 해당되는 페이지로 URL을 만들고 거기에 해당되는 네이버 뉴스 링크 URL제작
URLS <- function(keyword,page){
  urls <- NULL
  #keyword URLencode (한글을 URL코드로 인코딩 )
  keyword <- URLencode(keyword)
  #페이지수가 1,11,21로 바뀜, seq를 이용해서 1에서부터 10씩 늘리고 length.out을 위 page에서 만큼만 설정 가능 ex) page=4 이면 1,11,21,31 
  page <- seq(1,by=10,length.out=page)
  for (p in page){
    url<- paste0("https://search.naver.com/search.naver?where=news&sm=tab_pge&query=",keyword,"&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=101&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:all,a:all&start=",p)
    html <- read_html(url)
    #xpath로 이용하여 html 구조 찾기
    html <- htmlParse(html)
    html <- xpathSApply(html, "//div/div/div[1]/div[2]/a[2]",xmlGetAttr,"href")
    for (html in html){
      urls <- rbind(urls,html)
    }
  }
  return(urls)
}
#위에서 만든 네이버 URL를 이용해서 네이버 뉴스를 크롤링
text <- function(urls){
  i <- 1
  news <- NULL
  for (urls in urls){
    data <- NULL
    html <- htmlParse(read_html(urls))
    html <- xpathSApply(html, '//*[@id="dic_area"]/text()',xmlValue)
    text <- paste(html, collapse = "")
    data <- data.frame(Num=i,text=text)
    i=i+1
    news <- rbind(news,data)
  }
  return(news)
}
# 위 두 함수를 합친 함수 
Make_news <-function(keyword,page){
  url <- URLS(keyword,page)
  text <- text(url)
  return(text)
}
news <- Make_news("전세사기",15)

