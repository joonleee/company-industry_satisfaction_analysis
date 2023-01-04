# library('tm')
# library('KoNLP')
# library('Sejong')
# library('wordcloud2')


# FOR MAC
# par(family='AppleGothic')

data <- read.csv('xx.csv')

useNIADic()

# add words to dictionary
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
                   "좋겠습니", "해서", "하나", "개인", "하다", "해주", "안보")

exNouns <- function(x){
  paste(extractNoun(as.character(x)), collapse=" ")
}

posData <- Corpus(VectorSource(data$adv))
negData <- Corpus(VectorSource(data$disadv))
comData <- Corpus(VectorSource(data$comment))

pos_nouns <- sapply(posData, exNouns)
neg_nouns <- sapply(negData, exNouns)
com_nouns <- sapply(comData, exNouns)

posCorpus <- Corpus(VectorSource(pos_nouns))
posCorpus <- tm_map(posCorpus, removeNumbers)
posCorpus <- tm_map(posCorpus, removeWords, kor_stopwords) 

negCorpus <- Corpus(VectorSource(neg_nouns))
negCorpus <- tm_map(negCorpus, removeNumbers)
negCorpus <-tm_map(negCorpus, removeWords, kor_stopwords) 

comCorpus <- Corpus(VectorSource(com_nouns))
comCorpus <- tm_map(comCorpus, removeNumbers)
comCorpus <-tm_map(comCorpus, removeWords, kor_stopwords) 


posTdm <- TermDocumentMatrix(posCorpus, control=list(wordLengths=c(4,36)))
negTdm <- TermDocumentMatrix(negCorpus, control=list(wordLengths=c(4,36)))
comTdm <- TermDocumentMatrix(comCorpus, control=list(wordLengths=c(4,36)))

pos_df <- as.data.frame(as.matrix(posTdm)) 
neg_df <- as.data.frame(as.matrix(negTdm)) 
com_df <- as.data.frame(as.matrix(comTdm)) 

pos_wordResult <- sort(rowSums(pos_df), decreasing=TRUE) 
neg_wordResult <- sort(rowSums(neg_df), decreasing=TRUE) 
com_wordResult <- sort(rowSums(com_df), decreasing=TRUE) 

poswords <- names(pos_wordResult)
negwords <- names(neg_wordResult)
comwords <- names(com_wordResult)

pos_words_df <- data.frame(word=poswords, freq=pos_wordResult)
neg_words_df <- data.frame(word=negwords, freq=neg_wordResult)
com_words_df <- data.frame(word=comwords, freq=com_wordResult)

wordcloud2(pos_words_df, color='skyblue', fontFamily="AppleGothic")
wordcloud2(neg_words_df, color='crimson', fontFamily="AppleGothic")
wordcloud2(com_words_df, color='purple', fontFamily="AppleGothic")




# 연관어 분석
# install.packages('rJava')
# install.packages('arules')
# install.packages('igraph')

# library(rJava)
# library(arules)
# library(igraph)

data <- read.csv('xx.csv')


tran <- Map(extractNoun, data$adv)
tran <- unique(tran)
tran <- sapply(tran,function(x){Filter(function(y) + {nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)})
tran <- Filter(function(x){length(x) >= 2}, tran)


names(tran) <- paste("Tr", 1:length(tran), sep="")
head(tran)
wordtran <- as(tran,"transactions")
wordtran
wordtab <- crossTable(wordtran)
wordtab

ares <- apriori(wordtran, parameter=list(supp=0.025, conf=0.05)) 
inspect(ares)
rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ", USE.NAMES=F)
rulemat <- do.call("rbind", rules)
rulemat

rulemat <- (gsub('[^[:alnum:]]', ' ', rulemat))

ruleg <- graph.edgelist(rulemat[-c(1:27,31,32,75:77),], directed=F)

ruleg %>%
  ggraph(layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name), colour=node1.name)) +
  geom_node_text(aes(label=name), size = 8)


###########################################

tran <- Map(extractNoun, data$disadv)
tran <- unique(tran)
tran <- sapply(tran,function(x){Filter(function(y) + {nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)})
tran <- Filter(function(x){length(x) >= 2}, tran)


names(tran) <- paste("Tr", 1:length(tran), sep="")
head(tran)
wordtran <- as(tran,"transactions")
wordtran
wordtab <- crossTable(wordtran)
wordtab

ares <- apriori(wordtran, parameter=list(supp=0.015, conf=0.05)) 
inspect(ares)
rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ", USE.NAMES=F)
rulemat <- do.call("rbind", rules)
rulemat

str(rulemat)
rulemat <- (gsub('[^[:alnum:]]', ' ', rulemat))

ruleg <- graph.edgelist(rulemat[-c(1:18),], directed=F)
plot.igraph(ruleg, vertex.label=V(ruleg)$name,vertex.label.cex=1.2,
            vertex.size=30,layout=layout.fruchterman.reingold.grid, vertex.label.family='AppleGothic')


ruleg %>%
  ggraph(layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name), colour=node1.name)) +
  geom_node_text(aes(label=name), size = 8)


###########################################

tran <- Map(extractNoun, data$comment)
tran <- unique(tran)
tran <- sapply(tran,function(x){Filter(function(y) + {nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)})
tran <- Filter(function(x){length(x) >= 2}, tran)


names(tran) <- paste("Tr", 1:length(tran), sep="")
head(tran)
wordtran <- as(tran,"transactions")
wordtran
wordtab <- crossTable(wordtran)
wordtab

ares <- apriori(wordtran, parameter=list(supp=0.015, conf=0.05)) 
inspect(ares)
rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ", USE.NAMES=F)
rulemat <- do.call("rbind", rules)
rulemat

rulemat <- (gsub('[^[:alnum:]]', ' ', rulemat))

ruleg <- graph.edgelist(rulemat[-c(1:12,21,22,25,26),], directed=F)
plot.igraph(ruleg, vertex.label=V(ruleg)$name,vertex.label.cex=1.2,
            vertex.size=30,layout=layout.fruchterman.reingold.grid, vertex.label.family='AppleGothic')


ruleg %>%
  ggraph(layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name), colour=(node1.name))) +
  geom_node_text(aes(label=name), size = 8)

