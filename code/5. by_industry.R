# library(dplyr)

# 데이터 병합
it_one <- read.csv('it_one.csv')
it_two <- read.csv('it_two.csv')
it_data <- rbind(it_one,it_two)
it_data$ind <- "IT/Web/Comm"

manuf_one <- read.csv('manuf_one.csv')
manuf_two <- read.csv('manuf_two.csv')
manuf_data <- rbind(manuf_one, manuf_two)
manuf_data$ind <- "Manufacturing"

edu_one <- read.csv('edu_one.csv')
edu_two <- read.csv('edu_two.csv')
edu_data <- rbind(edu_one, edu_two)
edu_data$ind <- "Education"

# 3가지 산업군 하나의 df로 묶기
df_list <- list(it_data, manuf_data, edu_data)
# 빈 테이블 만들기
satTable <- data.frame(industry=character(3),
                       curr_sat=numeric(3), curr_ex=numeric(3),
                       ex_sat=numeric(3), ex_un=numeric(3))

# 빈 테이블을 채워줄 함수
getTable <- function(df_list){
  count = length(df_list)
  for(df in df_list){
    df <- split(df, df$empStat) # 현/전직원으로 각 df 분류
    df_curr_sat <- df$현직원 %>% filter(star >= 50)
    df_curr_un <- df$현직원 %>% filter(star < 50)
    df_ex_sat <- df$전직원 %>% filter(star >= 50)
    df_ex_un <- df$전직원 %>% filter(star < 50)
    satTable[count,] <- c(df_curr_sat[count,]$ind,
                          nrow(df_curr_sat), nrow(df_curr_un),
                          nrow(df_ex_sat), nrow(df_ex_un))
    count =  count -1
  }
  # reverse row order
  return(satTable[order(nrow(satTable):1),])
}

# 함수 호출
satTable <- getTable(df_list)
satTable



it_df<- split(it_data, it_data$empStat) # 현/전직원으로 각 df 분류
manuf_df<- split(manuf_data, manuf_data$empStat) # 현/전직원으로 각 df 분류
edu_df<- split(edu_data, edu_data$empStat) # 현/전직원으로 각 df 분류

# 만족도 기준으로 추가 분류
it_df_curr_sat <- it_df$현직원 %>% filter(star >= 50)
it_df_ex_un <- it_df$전직원 %>% filter(star < 50)

manuf_df_curr_sat <- manuf_df$현직원 %>% filter(star >= 50)
manuf_df_ex_un <- manuf_df$전직원 %>% filter(star < 50)

edu_df_curr_sat <- edu_df$현직원 %>% filter(star >= 50)
edu_df_ex_un <- edu_df$전직원 %>% filter(star < 50)

# 단순회귀를 위한 필요한 칼럼반 사용
it_df_ex_un <- it_df_ex_un[c('star', 'promo', 'welSal', 'wlb', 'culture', 'board')]
it_df_curr_sat <- it_df_curr_sat[c('star', 'promo', 'welSal', 'wlb', 'culture', 'board')]

manuf_df_ex_un <- manuf_df_ex_un[c('star', 'promo', 'welSal', 'wlb', 'culture', 'board')]
manuf_df_curr_sat <- manuf_df_curr_sat[c('star', 'promo', 'welSal', 'wlb', 'culture', 'board')]

edu_df_ex_un <- edu_df_ex_un[c('star', 'promo', 'welSal', 'wlb', 'culture', 'board')]
edu_df_curr_sat <- edu_df_curr_sat[c('star', 'promo', 'welSal', 'wlb', 'culture', 'board')]

# get coefficients()
it_coeff <- summary(lm(data=it_df_curr_sat, star~.)) %>% coefficients()
manuf_coeff <- summary(lm(data=manuf_df_curr_sat, star~.)) %>% coefficients()
edu_coeff <- summary(lm(data=edu_df_curr_sat, star~.)) %>% coefficients()

it_coeff_un <- summary(lm(data=it_df_ex_un, star~.)) %>% coefficients()
manuf_coeff_un <- summary(lm(data=manuf_df_ex_un, star~.)) %>% coefficients()
edu_coeff_un <- summary(lm(data=edu_df_ex_un, star~.)) %>% coefficients()

# check
# it_coeff
# it_coeff_un

# manuf_coeff
# manuf_coeff_un

# edu_coeff
# edu_coeff_un


# 설명력 구하기 
# summary(lm(data=df_curr_sat, star~promo))
# summary(lm(data=df_curr_sat, star~welSal))
# summary(lm(data=df_curr_sat, star~wlb))
# summary(lm(data=df_curr_sat, star~culture))
# summary(lm(data=df_curr_sat, star~.))
# 
# summary(lm(data=df_curr_sat, star~board+culture))
# summary(lm(data=df_curr_sat, star~board+welSal))
# summary(lm(data=df_curr_sat, star~board+promo))
# summary(lm(data=df_curr_sat, star~board+wlb))
# 
# summary(lm(data=df_curr_sat, star~board+welSal+promo))
# summary(lm(data=df_curr_sat, star~board+welSal+culture))
# summary(lm(data=df_curr_sat, star~board+welSal+wlb))
# 
# summary(lm(data=df_curr_sat, star~board+culture+welSal+promo))
# summary(lm(data=df_curr_sat, star~board+culture+welSal+wlb))
# 
# summary(lm(data=df_curr_sat, star~board+culture+welSal+wlb+promo))
# 
# 
# 
# summary(lm(data=df_curr_sat, star~promo))
# summary(lm(data=df_ex_un, star~welSal))
# summary(lm(data=df_ex_un, star~wlb))
# summary(lm(data=df_ex_un, star~culture))
# summary(lm(data=df_ex_un, star~board))
# 
# summary(lm(data=df_ex_un, star~board+culture))
# summary(lm(data=df_ex_un, star~board+welSal))
# summary(lm(data=df_ex_un, star~board+promo))
# summary(lm(data=df_ex_un, star~board+wlb))
# 
# summary(lm(data=df_ex_un, star~board+welSal+promo))
# summary(lm(data=df_ex_un, star~board+welSal+culture))
# summary(lm(data=df_ex_un, star~board+welSal+wlb))
# 
# summary(lm(data=df_ex_un, star~board+culture+welSal+promo))
# summary(lm(data=df_ex_un, star~board+culture+welSal+wlb))
# 
# summary(lm(data=df_ex_un, star~.))



# extract keywords
# library('tm')
# library('KoNLP')
# library('Sejong')
# library('wordcloud2')
# # FOR MAC
# par(family='AppleGothic')

useNIADic()

mergeUserDic(data.frame(c("워라벨", "워라밸", "교대근무", 
                          "보수적", "재택근무",
                          "높은연봉", "낮은연봉",
                          "자율출퇴근", "네임벨류",
                          "네임밸류", "배울점",
                          "탄력근무",
                          "글로벌", "유연근무"
), c("ncn")))

kor_stopwords <- c("않습니", "있었습니", "맛있습니", "여기", "얼마",
                   "잡혀있", "이것", "같습니", "로우", "말하", "되어있",
                   "있었", "좋았습니", "^ㅎ", "스러움", "좋았", "다운",
                   "해도", "많습니", "어느정도", "부바", "좋습니", "맛있",
                   "하지", "등등", "때문", "하게", "일하", "들이", "로운", "좋겠",
                   "하기", "경우", "만큼", "차이", "고과", "자기", "지금",
                   "좋겠습니", "해서", "하나", "개인", "하다")

exNouns <- function(x){
  paste(extractNoun(as.character(x)), collapse=" ")
}

# it_data, manuf_data, edu_data 로 수정해줘야함
df <- split(it_data, it_data$empStat)
df_curr_sat <- df$현직원 %>% filter(star >= 50)
df_ex_un <- df$전직원 %>% filter(star < 50)
posData <- Corpus(VectorSource(df_curr_sat$adv))
negData <- Corpus(VectorSource(df_ex_un$disadv))

pos_nouns <- sapply(posData, exNouns)
neg_nouns <- sapply(negData, exNouns)


posCorpus <- Corpus(VectorSource(pos_nouns))
posCorpus <- tm_map(posCorpus, removeNumbers)
posCorpus <-tm_map(posCorpus, removeWords, kor_stopwords) 

negCorpus <- Corpus(VectorSource(neg_nouns))
negCorpus <- tm_map(negCorpus, removeNumbers)
negCorpus <-tm_map(negCorpus, removeWords, kor_stopwords) 


posTdm <- TermDocumentMatrix(posCorpus, control=list(wordLengths=c(4,36)))
negTdm <- TermDocumentMatrix(negCorpus, control=list(wordLengths=c(4,36)))


pos_df <- as.data.frame(as.matrix(posTdm)) 
neg_df <- as.data.frame(as.matrix(negTdm)) 


pos_wordResult <- sort(rowSums(pos_df), decreasing=TRUE) 
pos_shortWords <- pos_wordResult[1:50]
neg_wordResult <- sort(rowSums(neg_df), decreasing=TRUE) 
neg_shortWords <- neg_wordResult[1:50]


poswords <- names(pos_wordResult)
negwords <- names(neg_wordResult)


pos_words_df <- data.frame(word=poswords, freq=pos_wordResult)
neg_words_df <- data.frame(word=negwords, freq=neg_wordResult)


wordcloud2(pos_words_df, color='skyblue', fontFamily="AppleGothic")
wordcloud2(neg_words_df, color='crimson', fontFamily="AppleGothic")

