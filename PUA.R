A <- read.csv("Eng_Original_Final_Final_DataBaseSet_CORRECTED.csv")

TOTAL <- read.csv("A1.csv")

library(ggplot2)
library(gridExtra)
library(RColorBrewer)

TOTAL1 <- ggplot(TOTAL, aes(x=BuildYear, fill=Class)) + geom_bar(position="dodge") + facet_grid(Class ~.) + scale_fill_brewer(palette = "Spectral")
TOTAL2 <- ggplot(TOTAL, aes(x=BuildYear, y=WinterGas, fill=Class)) + geom_bar(stat="identity") + facet_grid(Class ~.) + scale_fill_brewer(palette = "Spectral")
TOTAL3 <- ggplot(TOTAL, aes(x=BuildYear, y=WinterGas, fill=Class)) + geom_bar(stat="identity") + scale_fill_brewer(palette = "Spectral")
TOTAL4 <- ggplot(TOTAL, aes(x=BuildYear, fill=Class)) + geom_bar() + scale_fill_brewer(palette = "Spectral")

grid.arrange(TOTAL1,TOTAL2,TOTAL4,TOTAL3,  ncol=2)

#################################################A_District

A <- read.csv("PUA_A.csv")
str(A)
summary(A$Avg_Class)
A1 <- ggplot(A, aes(x=PUA_BuildYear, fill=Avg_Class)) + geom_bar(position="dodge") + facet_grid(Avg_Class ~.) + scale_fill_brewer(palette = "Spectral")
A2 <- ggplot(A, aes(x=PUA_BuildYear, y=PUA_Gas, fill=Avg_Class)) + geom_bar(stat="identity") + facet_grid(Avg_Class ~.) + scale_fill_brewer(palette = "Spectral")
A3 <- ggplot(A, aes(x=PUA_BuildYear, y=PUA_Gas, fill=Avg_Class)) + geom_bar(stat="identity") + scale_fill_brewer(palette = "Spectral")
A4 <- ggplot(A, aes(x=PUA_BuildYear, fill=Avg_Class)) + geom_bar() + scale_fill_brewer(palette = "Spectral")
A5 <- ggplot(A, aes(x=Avg_Class, y=PUA_Gas, fill=Avg_Class)) + geom_boxplot() + scale_fill_brewer(palette="Spectral")
grid.arrange(A1,A2,A3,A5 , ncol=2)
ggplot(A, aes(y=PUA_Elec, x=PUA_Gas, colour=Avg_Class)) + geom_point(na.rm=TRUE) + scale_colour_brewer(palette="Spectral")
str(A$Avg_Class)

#################################################################################

B <- read.csv("PUA_B.csv")
str(B)
summary(B)
summary(B$Avg_Class)
B1 <- ggplot(B, aes(x=PUA_BuildYear, fill=Avg_Class)) + geom_bar(position="dodge") + facet_grid(Avg_Class ~.) + scale_fill_brewer(palette = "Spectral")
B2 <- ggplot(B, aes(x=PUA_BuildYear, y=PUA_Gas, fill=Avg_Class)) + geom_bar(stat="identity") + facet_grid(Avg_Class ~.) + scale_fill_brewer(palette = "Spectral")
B3 <- ggplot(B, aes(x=PUA_BuildYear, y=PUA_Gas, fill=Avg_Class)) + geom_bar(stat="identity") + scale_fill_brewer(palette = "Spectral")
B4 <- ggplot(B, aes(x=PUA_BuildYear, fill=Avg_Class)) + geom_bar() + scale_fill_brewer(palette = "Spectral")
grid.arrange(B1,B2,B3,B5 , ncol=2)
B5 <- ggplot(B, aes(x=Avg_Class, y=PUA_Gas, fill=Avg_Class)) + geom_boxplot() + scale_fill_brewer(palette="Spectral")
ggplot(B, aes(y=PUA_Elec, x=PUA_Gas, colour=Avg_Class)) + geom_point(na.rm=TRUE) + scale_colour_brewer(palette="Spectral")

#################################################################################

C <- read.csv("PUA_C.csv")
str(C)
summary(C)
summary(C$Avg_Class)
C1 <- ggplot(C, aes(x=PUA_BuildYear, fill=Avg_Class)) + geom_bar(position="dodge") + facet_grid(Avg_Class ~.) + scale_fill_brewer(palette = "Spectral")
C2 <- ggplot(C, aes(x=PUA_BuildYear, y=PUA_Gas, fill=Avg_Class)) + geom_bar(stat="identity") + facet_grid(Avg_Class ~.) + scale_fill_brewer(palette = "Spectral")
C3 <- ggplot(C, aes(x=PUA_BuildYear, y=PUA_Gas, fill=Avg_Class)) + geom_bar(stat="identity") + scale_fill_brewer(palette = "Spectral")
C4 <- ggplot(C, aes(x=PUA_BuildYear, fill=Avg_Class)) + geom_bar() + scale_fill_brewer(palette = "Spectral")
grid.arrange(C1,C2,C3,C5 , ncol=2)
C5 <- ggplot(C, aes(x=Avg_Class, y=PUA_Gas, fill=Avg_Class)) + geom_boxplot() + scale_fill_brewer(palette="Spectral")
ggplot(C, aes(y=PUA_Elec, x=PUA_Gas, colour=Avg_Class)) + geom_point(na.rm=TRUE) + scale_colour_brewer(palette="Spectral")

#################################################################################

D <- read.csv("PUA_Total.csv")
str(D)
summary(D)
summary(D$PUA_Gas)
D1 <- ggplot(D, aes(x=PUA_BuildYear, fill=Avg_Class)) + geom_bar(position="dodge") + facet_grid(Avg_Class ~.) + scale_fill_brewer(palette = "Spectral")
D2 <- ggplot(D, aes(x=PUA_BuildYear, y=PUA_Gas, fill=Avg_Class)) + geom_bar(stat="identity") + facet_grid(Avg_Class ~.) + scale_fill_brewer(palette = "Spectral")
D3 <- ggplot(D, aes(x=PUA_BuildYear, y=PUA_Gas, fill=Avg_Class)) + geom_bar(stat="identity") + scale_fill_brewer(palette = "Spectral")
D4 <- ggplot(D, aes(x=PUA_BuildYear, fill=Avg_Class)) + geom_bar() + scale_fill_brewer(palette = "Spectral")
grid.arrange(D1,D2,D3,D5 , ncol=2)
D5 <- ggplot(D, aes(x=Avg_Class, y=PUA_Gas, fill=Avg_Class)) + geom_boxplot() + scale_fill_brewer(palette="Spectral")
ggplot(D, aes(y=PUA_Elec, x=PUA_Gas, colour=Avg_Class)) + geom_point(na.rm=TRUE) + scale_colour_brewer(palette="Spectral")

#################################################################################################

##0724 Rshiny
library(shiny)


#
library(knitr)
knit("RGitHub.Rmd")


