setwd("C:/Users/jungmiso01/Desktop/�긲û_R_Plot")  

library(ggplot2)
library(plyr)

A <- read.csv("Work_456TEMP_0817.csv")
str(A)

############################1-Building Type Name Change############################

BT <- A$�ǹ�����_G

BT1 <- revalue(BT, c('1-�����ְ�' = "�ܵ��ְ�", '2-�����ְ�' ="�����ְ�"))

A$�ǹ�����_G <- BT1

unique(A$�ǹ�����_G)

##################################2-safety rating##################################

B <- ggplot(A, aes(x=A$�ǹ�����_G, y=A$�������)) + geom_dotplot(binaxis="y", binwidth=.03, stackdir="center")

B + scale_y_discrete(limits=c("A", "B", "C", "D", "E", "NULL")) + xlab("�ǹ�����") + ylab("�������") 

##################################3-Detailed department############################

C <- ggplot(A, aes(x=A$�ǹ�����_G, y=A$���α����)) + geom_dotplot(binaxis="y", binwidth=.15, stackdir="center")

C + scale_y_discrete(limits=c("�����긲���п�", "��������긲û", "�����긲ǰ����������", "�Ϻ�����긲û", "��������긲û"
                              ,"��������긲û", "�긲�װ�����", "�ߺ�����긲û", "�����ڿ��޾縲������", "�긲û ����")) + xlab("�ǹ�����") + ylab("���α����")

##################################3-Building Structure#############################

D <- ggplot(A, aes(x=A$�ǹ�����_G, y=A$�ǹ�����_G)) + geom_dotplot(binaxis="y", binwidth=.07, stackdir="center")
D + scale_y_discrete(limits=c("������", "������", "����", "RC��", "ö��(�淮+�Ϲ�)", "��")) + xlab("�ǹ�����") + ylab("�ǹ�����") 

##################################4-Count By City#############################

E <- ggplot(A, aes(x=A$�ǹ�����_G, y=A$�����õ�)) + geom_dotplot(binaxis="y", binwidth=.1, stackdir="center")
E + scale_y_discrete(limits=c("��⵵", "������", "����������", "���ϵ�", "��󳲵�", "��û�ϵ�", "��û����", "����ϵ�", "���󳲵�")) +
  xlab("�ǹ�����") + ylab("�����á���") 

##################################5-Age of Building#############################

F <- ggplot(A, aes(x=A$�ǹ�����_G, y=A$�ذ�����)) +
  geom_boxplot(aes(x=as.numeric(A$�ǹ�����_G) + 0.2, group=A$�ǹ�����_G), width=0.25) +
  geom_dotplot(aes(x=as.numeric(A$�ǹ�����_G) - 0.2, group=A$�ǹ�����_G), binaxis="y", binwidth=0.8, stackdir="center") 
F + scale_y_continuous(breaks=c(1967, seq(1965,2020, by=5), 2017)) + scale_x_continuous(breaks=c(1,2), labels=c("�ܵ��ְ�", "�����ְ�")) + 
  xlab("�ǹ�����") + ylab("�ذ�����") 


##################################5-the number of people#############################

G <- ggplot(A, aes(x=A$�ǹ�����_G, y=A$�������)) +
  geom_boxplot(aes(x=as.numeric(A$�ǹ�����_G) + 0.2, group=A$�ǹ�����_G), width=0.25) +
  geom_dotplot(aes(x=as.numeric(A$�ǹ�����_G) - 0.2, group=A$�ǹ�����_G), binaxis="y", binwidth=0.2, stackdir="center") 
G + scale_y_continuous(breaks=c(seq(0,25, by=3))) + scale_x_continuous(breaks=c(1,2), labels=c("�ܵ��ְ�", "�����ְ�")) + xlab("�ǹ�����") + ylab("�������")

library(knitr)