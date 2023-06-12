
#market name은 market명과 한글 명을 가져옴 ( market 명은 모든 함수에 사용 한글명은 시각화때 이용)
marketname <- fromJSON("https://api.upbit.com/v1/market/all?isDetails=false") %>% select(market,korean_name)
marketname <- marketname %>% filter(str_detect(marketname$market,"KRW")==TRUE) 
marketname <- marketname[1:5,]


#원하는 날짜(date) ex) 2022-01-01 부터 가져오고 싶으면 2022-01-02 입력,원하는 자료 개수(count),해당 사이트 최대 가져올수있는 일수가 200개
dataamount <- function(date,num){
  # 개수의 풀더 존재 여부
  ifelse(dir.exists(paste0(num,'_datas')),FALSE,dir.create(paste0(num,'_datas')))
  # 개수의 데이터의 그룹(200개씩)
  zip <- num%/%200
  # 200개 이하의 데이터를 따로 구축
  least <- num %%200
  #에러를 찾기 위한 코드(대부분의 에러는 429에러 발생)
  counts=0
  marketinfo <- NULL
  for (name in marketname$market){
    counts=counts+1
    #날짜 계산을 위한 날짜형 변형
    date <- as.Date(date)
    
    tryCatch({
      # 200개 이하 일때
      if (zip==0){
        marketdate<- fromJSON(paste0("https://api.upbit.com/v1/candles/days?count=",least,"&to=",date,"%2000%3A00%3A00&market=",name))
        marketinfo <- rbind(marketinfo,marketdate)
      }else{
        #zip=1이상일 경우
        for (count in 1:zip){
          #zip이 1일경우
          if (count==1) {
            marketdate<- fromJSON(paste0("https://api.upbit.com/v1/candles/days?count=200&to=",date,"%2000%3A00%3A00&market=",name))
            marketinfo <- rbind(marketinfo,marketdate)
          }
          #zip이 2이상일 경우
          else if(count>=2){
            #date에서 200일씩 차감
            day <- date -((count-1)*200)
            marketdate<- fromJSON(paste0("https://api.upbit.com/v1/candles/days?count=200&to=",day,"%2000%3A00%3A00&market=",name))
            marketinfo <- rbind(marketinfo,marketdate)
          }
        }
        #200개 이상일때 나머지 일수 계산
        if(zip>=2){
          days <- date-(zip*200)
          marketdate<- fromJSON(paste0("https://api.upbit.com/v1/candles/days?count=",least,"&to=",days,"%2000%3A00%3A00&market=",name))
          marketinfo <- rbind(marketinfo,marketdate)
        }else if(zip==1){
          days <- date-200
          marketdate<- fromJSON(paste0("https://api.upbit.com/v1/candles/days?count=",least,"&to=",days,"%2000%3A00%3A00&market=",name))
          marketinfo <- rbind(marketinfo,marketdate)
        }
        
      }
      #해당 날짜 부터 데이터가 없을경우 패스
      if (is.null(marketdate)){
        next
      }else{
        #존재하면 csv파일로 작성
        
        
        print(c(name,counts))
      }
    })
    Sys.sleep(4)
  }
  marketinfo$candle_date_time_utc <- gsub("[[:punct:]]","",marketinfo$candle_date_time_utc)%>% substr(3,8) 
  marketinfo<- marketinfo %>% select(candle_date_time_utc,trade_price,market)
  write.csv(marketinfo,paste0(num,'_datas/total_info.csv'),row.names = F)
}
for (i in 1:5){
  print(i)
}


group <- function(num){
  marketreturn <- NULL
  a <- NULL
  for (name in marketname$market) {
    if(file.exists(paste0(num,'_datas/total_info.csv'))){
      #데이터 가져오기 & 날짜형태로 변환
      price <- read.csv(paste0(num,'_datas/total_info.csv'))
      price$candle_date_time_utc <- price$candle_date_time_utc%>% as.character() %>% as.Date(format='%y%m%d')
      
      #시계열데이터로 변환
      price <- price%>% select(candle_date_time_utc,trade_price) %>% as.xts()
      
      #auto.arima실행으로 arima 계수 구하기
      mf <- auto.arima(price)
      
      
      # 각 마켓별 arima 데이터프레임으로 가져오기
      a <- rbind(a,c(market=name,ar=length(mf$model$phi),i=length(mf$model$Delta),ma=length(mf$model$theta)))
      
      
      # 모델 구하기 및 예측
      model <- Arima(price,c(length(mf$model$phi),length(mf$model$Delta),length(mf$model$theta)))
      fore <- forecast(model,h=5)
      
      #수익률 계산 및 마켓 별 arima 계수 측정
      return <- read.csv(paste0(num,'_datas/',name, '_info.csv'))
      return <- return[1,]%>% select(market,trade_price) %>% mutate(forec=fore$mean[1],returns=(forec-trade_price)/(trade_price)*100)
      marketreturn<- rbind(marketreturn,return)
    }
    
  }
  #수익률 + 마켓별arima 계수 합치기 / 수익률 별로 정렬(이유는 수익룰이 0이면 ARIMA=0,1,0이므로 각 ARIMA구별이 쉬워짐)
  info <- merge(marketreturn,a)
  info <- info[order(-info$returns),]
  print(info)
  #시각화 
  vision <- marketreturn[order(-marketreturn$returns),]
  #상위 3개 암호화폐 추출
  vision <- vision[1:3,]
  i=1
  par(mfrow=c(3,1))
  for (name in vision$market) {
    #csv 파일 읽기
    price <- read.csv(paste0(num,'_datas/total_info.csv'))
    price$candle_date_time_utc <- price$candle_date_time_utc%>% as.character() %>% as.Date(format='%y%m%d')
    price <- price%>% select(candle_date_time_utc,trade_price) %>% as.xts()
    # 수익률 상위 3개 (i가 작으면 최상위 상품) 
    if (i==1) {
      mf <- auto.arima(price)
      model <- Arima(price,c(length(mf$model$phi),length(mf$model$Delta),length(mf$model$theta)))
      fore1<- forecast(model,h=5)
      # 시각화
      plot(fore1,ann=FALSE)
      # 한글명
      title(main=marketname$korean_name[marketname$market==name])
    }else if(i==2){
      mf <- auto.arima(price)
      model <- Arima(price,c(length(mf$model$phi),length(mf$model$Delta),length(mf$model$theta)))
      fore2 <- forecast(model,h=5)
      plot(fore2,ann=FALSE)
      title(main=marketname$korean_name[marketname$market==name])
    }else{
      mf <- auto.arima(price)
      model <- Arima(price,c(length(mf$model$phi),length(mf$model$Delta),length(mf$model$theta)))
      fore3<- forecast(model,h=5)
      plot(fore3,ann=FALSE)
      title(main=marketname$korean_name[marketname$market==name])
    }
    i=i+1
  }
}


group(230)




pp <- dataamount("2023-06-07",530)
program <- function(date,num){
  dataamount(date,num)
  group(num)
}

program("2023-06-07",365)

a <- 
# p_200<- read.csv("data/KRW-ADA_info.csv")
# p_200$candle_date_time_utc <- p_200$candle_date_time_utc%>% as.character() %>% as.Date(format='%y%m%d')
# p_200 <- p_200[1:200,]%>% select(candle_date_time_utc,trade_price) %>% as.xts()
# p_200 <- arima(p_200,c(1,1,1))
# tsdiag(p_200)
# 
# p_all<- read.csv("data/KRW-ADA_info.csv")
# p_all$candle_date_time_utc <- p_all$candle_date_time_utc%>% as.character() %>% as.Date(format='%y%m%d')
# p_all <- p_all%>% select(candle_date_time_utc,trade_price) %>% as.xts()
# p_all <- arima(p_50,c(2,2,1))
# tsdiag(p_all)
# 
# par(mfrow=c(2,1))
# Acf(diff(p_200))
# Pacf(diff(p_200))
