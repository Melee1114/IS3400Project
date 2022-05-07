#Devin Bagley
library(ggplot2) 
library(dplyr)
library(dslabs)
library(tidyverse)

#Temp until we can import from github.
#path <- "/Users/multi/Downloads/cricketer.csv"
#cricket <- read.csv(path)
#print(cricket)
#cricketers <- read_csv(url(urlfile))

#This is the import from github.
URL <- "https://raw.githubusercontent.com/Melee1114/IS3400Project/main/cricketer.csv"
cricket <- read_csv(URL)

#Plot 

#Column names are confusing. No point looking at Documentation.
colnames(cricket) <- c("ID","hand","birthYear","lifeSpan","dead","accident","KIA","inBed","cause")

#Histogram to show off causes of death at what lifespan. This is an old version that sorted out alive.
cricket_new <- filter(cricket, cricket$cause == "acd" | cricket$cause == "inbed")
G <- cricket_new %>% ggplot()
G + geom_histogram(aes(x=birthYear,fill = cause),color="black",bins=10,
                   show.legend = TRUE)

#Box plot that sorts out alive and shows concetration of lifespan per cause of death
cricket_dead <- filter(cricket, cricket$cause == "acd" | cricket$cause == "inbed")
G <- cricket_dead %>% ggplot()
G + geom_boxplot(aes(lifeSpan,fill= cause),show.legend = TRUE)



#Random

#Filter each data set to only count number of deaths
cricket_acd <- filter(cricket, cricket$cause == "acd")
cricket_inbed <- filter(cricket, cricket$cause == "inbed")

#Count the number of times these occur
dice <- c(replicate(nrow(cricket_acd),"acd"),replicate(nrow(cricket_inbed),"inbed"))

#Roll the dice 10000 times for probability.
Sample10000 <- sample(dice,10000,replace = TRUE)
Sample10000 <-table(Sample10000)
Sample10000

#Counted for expected
#nrow(cricket_inbed)
Expected <- c(555,9445)
Sample10000 <- rbind(Sample10000,Expected)
Sample10000

#Chi-square test.
chisq.test(Sample10000)
