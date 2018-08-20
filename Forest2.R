setwd("C:/Users/jungmiso01/Desktop/산림청_R_Plot")  

library(ggplot2) 
library(plyr)
 
A <- read.csv("Work_456TEMP_0817.csv")
str(A)

############################1-Building Type Name Change############################

BT <- A$건물유형_G

BT1 <- revalue(BT, c('1-독립주거' = "단독주거", '2-집합주거' ="집합주거"))

A$건물유형_G <- BT1

unique(A$건물유형_G)

##################################2-safety rating##################################

B <- ggplot(A, aes(x=A$건물유형_G, y=A$안전등급)) + geom_dotplot(binaxis="y", binwidth=.03, stackdir="center")

B + scale_y_discrete(limits=c("A", "B", "C", "D", "E", "NULL")) + xlab("건물유형") + ylab("안전등급") 

##################################3-Detailed department############################

C <- ggplot(A, aes(x=A$건물유형_G, y=A$세부기관명)) + geom_dotplot(binaxis="y", binwidth=.15, stackdir="center")

C + scale_y_discrete(limits=c("국립산림과학원", "동부지방산림청", "국립산림품종관리센터", "북부지방산림청", "남부지방산림청"
                              ,"서부지방산림청", "산림항공본부", "중부지방산림청", "국립자연휴양림관리소", "산림청 관사")) + xlab("건물유형") + ylab("세부기관명")

##################################3-Building Structure#############################

D <- ggplot(A, aes(x=A$건물유형_G, y=A$건물구조_G)) + geom_dotplot(binaxis="y", binwidth=.07, stackdir="center")
D + scale_y_discrete(limits=c("블럭조", "벽돌조", "목구조", "RC조", "철골(경량+일반)", "모름")) + xlab("건물유형") + ylab("건물구조") 

##################################4-Count By City#############################

E <- ggplot(A, aes(x=A$건물유형_G, y=A$광역시도)) + geom_dotplot(binaxis="y", binwidth=.1, stackdir="center")
E + scale_y_discrete(limits=c("경기도", "강원도", "대전광역시", "경상북도", "경상남도", "충청북도", "충청남도", "전라북도", "전라남도")) +
  xlab("건물유형") + ylab("광역시·도") 

##################################5-Age of Building#############################

F <- ggplot(A, aes(x=A$건물유형_G, y=A$준공연도)) +
  geom_boxplot(aes(x=as.numeric(A$건물유형_G) + 0.2, group=A$건물유형_G), width=0.25) +
  geom_dotplot(aes(x=as.numeric(A$건물유형_G) - 0.2, group=A$건물유형_G), binaxis="y", binwidth=0.8, stackdir="center") 
F + scale_y_continuous(breaks=c(1967, seq(1965,2020, by=5), 2017)) + scale_x_continuous(breaks=c(1,2), labels=c("단독주거", "집합주거")) + 
  xlab("건물유형") + ylab("준공연도") 


##################################5-the number of people#############################

G <- ggplot(A, aes(x=A$건물유형_G, y=A$사용현원)) +
  geom_boxplot(aes(x=as.numeric(A$건물유형_G) + 0.2, group=A$건물유형_G), width=0.25) +
  geom_dotplot(aes(x=as.numeric(A$건물유형_G) - 0.2, group=A$건물유형_G), binaxis="y", binwidth=0.2, stackdir="center") 
G + scale_y_continuous(breaks=c(seq(0,25, by=3))) + scale_x_continuous(breaks=c(1,2), labels=c("단독주거", "집합주거")) + xlab("건물유형") + ylab("사용현원")

library(knitr)
