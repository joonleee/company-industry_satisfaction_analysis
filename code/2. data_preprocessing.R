# getwd()
# setwd('/Users/pogo/Google Drive/My Drive/Rproject/newData/')
# # install.packages('stringr')
# library('stringr')
# library('rJava')
# library('KoNLP')
# library('tm')
# library('wordcloud')
# library('Sejong')
# library('dplyr')

data <- read.csv('xx.csv', stringsAsFactors = FALSE)

# get rid of row number, company name, and post date
data <- data[-c(1, 2, 8)]

# rename columns because using korean is annoying
names(data) <- c("compCode", "revCode", "jobType", "empStat", "loc", "star", "promo", "welSal", "wlb", "culture", "board", "adv", "disadv", "comment", "growth", "recc")
str(data)

# replace growth with simple values and save as factor
# growth 0 -> 비슷 growth 1 -> 성장
data$growth <- as.factor(ifelse(data$growth=="비슷", 0, ifelse(data$growth=="성장", 1, NA)))

# replace reccomendation with simple values and save as factor
# recc 0 -> 추천X recc 1 -> 추천O
data$recc <- as.factor(ifelse(data$recc=="이 기업을 추천하지 않습니다.", 0, ifelse(data$recc=="이 기업을 추천 합니다!", 1, NA)))

# remove extra characters in empStat, save as factor
data$empStat <- as.factor(unlist(str_extract_all(data$empStat,"[가-힣]{3}")))

# remove extra characters and doublespaces in adv
data$adv <- (gsub('\\s+', ' ', gsub('[^[:alnum:]]', ' ', data$adv)))

# remove extra characters and doublespaces in disadv
data$disadv <- (gsub('\\s+', ' ', gsub('[^[:alnum:]]', ' ', data$disadv)))

# remove extra characters and doublespaces in comment
data$comment <- (gsub('\\s+', ' ', gsub('[^[:alnum:]]', ' ', data$comment)))

data$calcStar <- rowMeans(data[,c("promo", "welSal", "wlb", "culture", "board")])

write.csv(data, 'xx.csv')