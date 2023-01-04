# install.packages('tm')
# install.packages('rvest')
# install.packages('httr')
# 
# library('rvest')
# library('httr')
# library('stringr')


############################
######  잡플래닛 접근 ######
############################

# 잡플레닛 로그인 주소
url <- 'https://www.jobplanet.co.kr/users/sign_in'

# 유저아이디와 비밀번호로 위 url에 점급하기
resp <- POST(url=url, body = list('user[email]' = "이메일@주소",
                                  'user[password]' = "비밀번호"))

# 접근이 성공적인지 확인.
status_code(x=resp)

# 쿠키정보를 저장
myCookies <- set_cookies(.cookies=unlist(x=cookies(x=resp)))


############################
#####  특정 기업 조회 ######
############################
# 쿠팡
company_url <- "회사 url"

# 위에 저장한 쿠키를 이용하여 로그인
resp <- GET(url=company_url, config=list(cookies=myCookies))
# 접근확인
status_code(x=resp)


company_name <- resp %>% read_html() %>% html_node("h1.name") %>% html_text()
company_name

# 총 리뷰 개수
revCnt <- resp %>% read_html() %>% html_nodes(css='li.viewReviews > a > span.num.notranslate') %>% html_text() %>%  as.numeric()
revCnt

# 리뷰 element 만 저장하기
rev_items <- resp %>% read_html() %>% html_nodes(css='section.content_ty4')
rev_items

# 회사코드 및 리뷰코드 추출
rev_items %>% html_attr(name='data-company_id')
rev_items %>% html_attr(name='data-content_id')

# 직종 추출
rev_items %>% html_nodes(css='div.content_top_ty2 span:nth-child(2)')
# 현/전직 구분 
rev_items %>% html_nodes(css='div.content_top_ty2 span:nth-child(4)')
# 근무지역 
rev_items %>% html_nodes(css='div.content_top_ty2 span:nth-child(6)')
# 등록일자 
rev_items %>% html_nodes(css='div.content_top_ty2 span:nth-child(8)') %>% html_text()


# 5점만점 별점
rev_items %>% html_nodes(css = 'div.star_score') %>% html_attr(name = 'style')

rev_items %>% html_nodes(css = 'dl dd:nth-child(3) div div') %>% html_attr(name = 'style')

rev_items %>% html_nodes(css = 'dl dd:nth-child(5) div div') %>% html_attr(name = 'style')

rev_items %>% html_nodes(css = 'dl dd:nth-child(7) div div') %>% html_attr(name = 'style')

rev_items %>% html_nodes(css = 'dl dd:nth-child(9) div div') %>% html_attr(name = 'style')

rev_items %>% html_nodes(css = 'dl dd:nth-child(11) div div') %>% html_attr(name = 'style')

# 장점, 단점, 바라는점, 성장, 추천 추출
rev_items %>% html_nodes(css = 'dl dd:nth-child(2) span') %>% html_text()

rev_items %>% html_nodes(css = 'dl dd:nth-child(4) span') %>% html_text()

rev_items %>% html_nodes(css = 'dl dd:nth-child(6) span') %>% html_text()

rev_items %>% html_nodes(css = 'p.etc_box strong') %>% html_text()

rev_items %>% html_nodes(css = 'p.txt.recommend.etc_box') %>% html_text()


getHtmlText <- function(x, css) {
  
  result <- x %>% 
    html_node(css = css) %>% 
    html_text()
  
  return(result)
}


# 별점수집함수
getHtmlRate <- function(x, css, name) {
  result <- x %>% html_node(css = css) %>% 
    html_attr(name = name) %>% str_remove_all(pattern = '(width:)|(%;)') %>% 
    as.numeric()
  
  return(result)
}


# 리뷰 수집 -> df로 변환
getData <- function(x) {
  
  # 리뷰가 포함된 html
  rev_items <- x %>% read_html() %>% html_nodes(css = 'section.content_ty4')
  
  # df 로  
  df <- 
    data.frame(
      compName = x %>% read_html() %>% html_node(css = 'h1.name') %>% html_text(),
      compCode = rev_items %>% html_attr(name = 'data-company_id'),
      revCode = rev_items %>% html_attr(name = 'data-content_id'),
      jobType = getHtmlText(x = rev_items, css = 'div.content_top_ty2 span:nth-child(2)'),
      empStat = getHtmlText(x = rev_items, css = 'div.content_top_ty2 span:nth-child(4)'),
      loc = getHtmlText(x = rev_items, css = 'div.content_top_ty2 span:nth-child(6)'),
      postDate = rev_items %>% html_nodes(css='div.content_top_ty2 span:nth-child(8)') %>% html_text(),
      stars = getHtmlRate(x = rev_items, css = 'div.star_score', name = 'style'),
      promo = getHtmlRate(x = rev_items, css = 'dl dd:nth-child(3) div div', name = 'style'),
      welSal = getHtmlRate(x = rev_items, css = 'dl dd:nth-child(5) div div', name = 'style'),
      wlb   = getHtmlRate(x = rev_items, css = 'dl dd:nth-child(7) div div', name = 'style'),
      culture = getHtmlRate(x = rev_items, css = 'dl dd:nth-child(9) div div', name = 'style'),
      board   = getHtmlRate(x = rev_items, css = 'dl dd:nth-child(11) div div', name = 'style'),
      adv = getHtmlText(x = rev_items, css = 'dl dd:nth-child(2) span'),
      disadv = getHtmlText(x = rev_items, css = 'dl dd:nth-child(4) span'),
      comment = getHtmlText(x = rev_items, css = 'dl dd:nth-child(6) span'),
      growth = getHtmlText(x = rev_items, css = 'p.etc_box strong'),
      recc = getHtmlText(x = rev_items, css = 'p.txt.recommend.etc_box')
    )
  
  return(df)
}

# 긁어와야 할 총 페이지 수 (한 페이지당 5개 리뷰 있음.)
pages <- ceiling(x = revCnt / 5)
print(x = pages)

rm(res)
res <- getData(x = resp)

# 페이지수만큼 데이터 수집
for (page in 2:pages) {
  uri <- str_c(company_url, '?page=', page)
  
  # http 요청
  resp <- GET(url = uri, config = list(cookies = myCookies))
  
  df <- getData(x = resp)
  
  res <- rbind(res, df)

  rm(resp, df)
}

# write.csv(res,'xx.csv')
