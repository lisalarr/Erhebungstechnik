library("readxl")
data <- read_excel("C:/Users/Jacqu/Documents/Erhebungstechniken/Bericht/DatensatzFragebogenLJJY.xlsx")
data$id <- c(seq(1, length(data$`[id]`)))

## Anteile Bib-Nutzung und Ersatzbewertung ##

#barplot(table(data$`Ersatzbew. (10)`))
#data$`Ersatzbew. (10)` <- factor(data$`Ersatzbew. (10)`, levels = c(1,0), labels = c("Ja", "Nein"))
#barplot(table(data$`Ersatzbew. (10)`))

mosaicplot(~ factor(`UB(V)`, levels = c(1,0), labels = c("Ja", "Nein")) +
             factor(`Ersatzbew. (10)`, levels = c(1,0), labels = c("Ja", "Nein")),
           data = data, ylab = "Ersatz ausreichend", xlab= "Bibliothek benutzt",
           main = "Ersatzbewertung")

## Wofür werden die Lernorte genutzt? ##

barplot(colSums(na.omit(data[,21:26])))

## Am Meisten für Abgaben, am wenigsten für Abschlussarbeiten

## Bewertung der allgemeinen Lernsituation ##

barplot(table(na.omit(data$`Bewertung (9)`)))

## 0 im Datensatz?

index <- which(data$`Bewertung (9)` == 0)
data$`Bewertung (9)`[index] <- NA

barplot(table(na.omit(data$`Bewertung (9)`)), names.arg = c("Sehr schlecht", "Schlecht", "Gut", "Sehr gut"))
## Am meisten gut

mean(na.omit(data$`Bewertung (9)`))
## [1] 2.602941

## Geschlechter ##

data$Geschlecht <- factor(data$Geschlecht, labels=c("Weiblich", "Männlich", "Divers"))
barplot(table(data$Geschlecht), main = "Verteilung der Geschlechter",
        col = c("red", "orange", "yellow"), xlab = "Geschlecht", ylab="Anzahl")

## Häufigkeit Lernortnutzung Vorher/Jetzt ##

boxplot(as.numeric(na.omit(data$`Frage 2 (Vorher)`)), as.numeric(na.omit(data$`Frage 2 (Jetzt)`)), names = c("Group1", "Group2"), main = "Boxplots nebeneinander")


## Sankey Plot

# install.packages("networkD3")
# install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("virdis")
#install.packages("patchwork")
#install.packages("hrbrthemes")
#install.packages("circlize")

library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
library(dplyr)

sankeyNetwork(data[seq(5,20,2)], data[seq(6,)])

## Haeufigkeiten der Lernortnutzung (Vorher und Jetzt)
freq <- colSums(na.omit(data[,5:20]))

## Hauefigkeiten der Lernorte (Vorher)
sum_before <- colSums(na.omit(data[,seq(5,20, by=2)]))
# UB(V)     EFB(V)     CLS(V) Galerie(V)     Fak(V)     BCI(V)     Süd(V)    SRG (V) 
# 108   27          9         23         85          7          8         38 

## Hauefigkeiten der Lernorte (Jetzt)
sum_now <- colSums(na.omit(data[,seq(6,20, by=2)]))
# SB(J)     EFB(J)     CLS(J) Galerie(J)     Fak(J)     BCI(J)     Süd(J)     SRG(J) 
# 16         34         24         63         87          6          7         41 


## Spalten- und Zeilennamen definieren (alle möglichen Lernorte)
## Spalten = Jetzt, Zeilen = Vorher
spaltennamen <- c("SB", "EFB", "CLS", "Galerie", "Fak", "BCI", "Süd", "SRG")
zeilennamen <- c("UB", "EFB", "CLS", "Galerie", "Fak", "BCI", "Süd", "SRG")

## Test-Matrix = Änderung der Lernortnutzung
## Zählt also, wie viele Leute immer noch einen Lernort benutzten bzw. wie viele
## Leute von einem Lernort zu einem anderen gewechselt sind
test <- matrix(0, nrow = length(zeilennamen), ncol = length(spaltennamen))
rownames(test) <- spaltennamen
colnames(test) <- zeilennamen
test <- as.data.frame(test)

names <- names(freq)
orte_vorher <- names[seq(1,length(names), by = 2)]
orte_nachher <- names[seq(2,length(names), by = 2)]

## NAs werden mit 0 ersetzt
data[,5:20][is.na(data[,5:20])] <- 0

## Mithilfe der Schleifen wird gezählt, wie viele Leute einen Lernort vorher
## und jetzt angekreuzt haben bzw. wie viele Leute einen Lernort
## vorher angekreuzt haben und jetzt einen Anderen

for(i in 1:length(zeilennamen)){
  for(j in 1:length(spaltennamen)){
    for(k in 1:length(data$`Frage 1`)){
      if((i == j)|(i == 1)){
        if((data[k,3+2*i][[1]] == 1) & (data[k,4+2*j][[1]] == 1)){
          test[i,j] <- test[i,j] + 1
        }
      }
      else if((data[k,3+2*i][[1]] == 1) & (data[k,4+2*j][[1]] == 1) & (data[k,3+2*j][[1]] == 0)){
        test[i,j] <- test[i,j] + 1
      }
    }
  }
}


## Die Lernorte die generell sehr wenig genutzt werden, werden entfernt
## -> Übersichtlichkeit
test <- subset(test, select = -BCI)
test <- subset(test, select = -Süd)
test <- test[c(T,T,T,T,T,F,F,T),]

## Sankey Diagramm wird erstellt

library(networkD3)

## "Langes" Format wird erstellt
test_lang <- test %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)

colnames(test_lang) <- c("source", "target", "value")
test_lang$target <- paste(test_lang$target, " ", sep="")

## Nodes werden erstellt 
nodes <- data.frame(name=c(as.character(test_lang$source), as.character(test_lang$target)) %>% unique())

test_lang$IDsource <- match(test_lang$source, nodes$name)-1 
test_lang$IDtarget <- match(test_lang$target, nodes$name)-1

## Farbauswahl
farben_blau_gruen <- c("#08306b", "#2171b5", "#6baed6","#48b99b", "#369c75")

farben <- paste0('d3.scaleOrdinal().range(["', paste(farben_blau_gruen, collapse = '","'), '"])')

sankey_diagramm <- sankeyNetwork(Links = test_lang, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", nodePadding=17,
              sinksRight=FALSE, colourScale=farben, nodeWidth=45, fontSize=12.5)

## Sankey Diagramm wird geplotet
sankey_diagramm

## Barplots der Lernortnutzung (Vorher und Jetzt)
par(mfrow = c(1, 2))
barplot(sum_before, las = 2, ylim = c(0,100))
barplot(sum_now, las = 2, ylim = c(0,100))

dev.off()

