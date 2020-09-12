library(rvest)
library(httr)

#마지막에 data라는 곳에 모든 정보 저장
data <- list()

#i는 코스피, 코스닥 구분
for (i in 0:1){
  ticker <- list()
  url <- paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',i,'&page=1')
  
  url_get <- GET(url)
  
  #코스피, 코스닥에서 가장 마지막 페이지 찾는 것
  navi.final = read_html(url_get, encoding = "EUC-KR") %>%
    html_nodes("tr") %>%
    html_nodes("td.pgRR") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    strsplit("=") %>%
    unlist() %>%
    .[3] %>%
    as.numeric()
  
  for(j in 1:navi.final){
    #각 페이지에 해당하는 url
    
    url <- paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',i,'&page=',j)
    down_table = GET(url)
    
    Sys.setlocale("LC_ALL", "English") #table 불러오기 전에는 항상 로케일 언어를 바꿔준다.
    
    read_html(down_table,encoding="EUC-KR") %>%
      html_table(fill = TRUE) %>%
      .[[2]] -> table
    
    Sys.setlocale("LC_ALL", "Korean") #table 다운받은 후에는 읽어오려면 다시 언어 한국어로 바꿔야지 한국어로 읽어옴
    
    table <- table[!is.na(table$N),] #값 없는 행 빼주기
    rownames(table) <- NULL
    subset(table, select=-c(토론실)) -> table #토론실 데이터 빼주기 이것으로 표 정리 완료
    
    #이제 종목코드 추출하자
    read_html(down_table, encoding="EUC-KR") %>%
      html_nodes("tbody") %>%
      html_nodes("td") %>%
      html_nodes("a[href]") %>%
      html_attr("href") -> tar# strsplit("=") 이런식으로는 못한다. 지금 리스트가 여러가지라
      #sapply 함수 적용하는 법 알아두기
    
    tar_1 = sapply(tar, function(x){
      substr(x,nchar(x)-5,nchar(x))
    }) %>% unique()
    
    #테이블에 이제 종목코드 다 넣어줄거다
    table$N = tar_1
    colnames(table)[1] = "종목코드"
    
    ticker[[j]] = table

    Sys.sleep(0.5)
      
  }
  #리스트를 이제 데이터 프레임으로 묶는다.
  ticker = do.call(rbind, ticker)
  data[[i+1]] = ticker
}
  do.call(rbind, data) -> data

write.csv(data,"종목코드.csv")