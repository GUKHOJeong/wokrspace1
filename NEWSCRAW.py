# -*- coding: utf-8 -*-
"""Untitled27.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1Jtn60_tMq2uuDhDhyy-Ean20kefSkF15
"""


import requests
import pandas as pd
from bs4 import BeautifulSoup
from urllib import parse
headers = {'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.114 Safari/537.36'}

def url_make(keyword,code,number):
  #리턴
  urls=[]
  num=1
  page=[]
  #한글을 url로 인코딩
  key=parse.quote(keyword) 
  #페이지
  for i in range(number):
    if i== 0:
      page.append(1)
    else:
      i=i*10+1
      page.append(i)
  #url 넣는 곳 
  for p in page :
    url="https://search.naver.com/search.naver?where=news&sm=tab_pge&query="+str(key)+"&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=50&mynews=1&office_type=1&office_section_code=1&news_office_checked="+str(code) +"&nso=so:r,p:all,a:all&start="+str(p)
    new= requests.get(url, headers=headers)
    new.content
    #html 구조 파악
    soup= BeautifulSoup(new.content, 'html.parser')
    #뽑고싶은 내용의 위치
    new_list = soup.select('.bx >div.news_wrap.api_ani_send > div > div.news_info > div.info_group > a:nth-child(3)')
    #간혹 3번째가 아닌 4번째에 네이버 뉴스 링크가 걸려 있기에 추가
    new_list.extend(soup.select('.bx >div.news_wrap.api_ani_send > div > div.news_info > div.info_group > a:nth-child(4)'))
    for link in new_list:
      link=link.get('href')
      urls.append(link)
  return urls

#언론사 코드는 깃허브에 추가 예정
number={"1032":"경향","1214":"mbc","1056":"KBS","1025":"중앙"}
#넣고 싶은 언론사를 위 딕션너리 code를 보고 작성
co=[]

def items(urllist,code):
  con_list=[]
  for link in urllist:
    con= requests.get(link, headers=headers)
    con.content
    #html 구조 파악
    cons= BeautifulSoup(con.content, 'html.parser')
    #본문 내용있는 섹터 선택
    content=cons.select_one("#dic_area")
    #섹터에 있는 text만 가져옴
    #newsEndContents
    con_list.append(content.text)
  df= pd.DataFrame({'news': con_list})
  df['num'] = number[str(code)]   
  return df

def make_news(code_list,number,keyword):
  df = None
  for num in code_list:
    
    # url_make 함수 : 원하는 키워드를 url로 인코딩 하여 해당 네이버 뉴스 url를 만들어주는 함수
    url_list = url_make(keyword,num,number)

    # items 함수: url과 카테고리를 알려주면 데이터프레임을 만들어주는 함수
    df_temp = items(url_list,num)
    #진행상황 알기 위한 구문
    print(str(num)+'번 코드에 대한 데이터를 만들었습니다.')

    if df is not None: 
      # 이전 데이터프레임과 지속적으로 결합
      df = pd.concat([df, df_temp])
    else:
      df = df_temp

  return df

Yoon=make_news(co,1,"윤석열")
#df를 csv 파일로 추출 - utf-8 필수 안하면 한글 깨짐
Yoon.to_csv('Yoon.csv',encoding='utf-8-sig',index=False)
