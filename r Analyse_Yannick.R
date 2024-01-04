library(readxl)
DatensatzFragebogenLJJY <- read_excel("C:/Users/yanni/Desktop/Studium/3. Semester/Erhebungstechniken/Fragebogen Projekt/DatensatzFragebogenLJJY.xlsx")
data <- DatensatzFragebogenLJJY


#table(data$Geschlecht)
#hist(table(data$Ruhe...33))
#hist(data$Ruhe...33)

#data$Ruhe...53 <-factor(data$Ruhe...53,levels = c("1","2","3","4","5","6"), 
       #labels = c("sehr gut","gut","befriedigend",
                  #"ausreichend", "mangelhaft","ungenügend"))
#data$Ruhe...43 <-factor(data$Ruhe...43,levels = c("1","2","3","4","5","6"), 
                        #labels = c("sehr gut","gut","befried.",
                                  # "ausr.", "mangel.","ungenüg."))
#par(mfrow=c(1,2))
#barplot(table(data$Ruhe...43)/length(data$Ruhe...43), main="Vorher",
#        xlab = "Aspekt Ruhe", ylim = c(0.0,0.4))
#barplot(table(data$Ruhe...53)/length(data$Ruhe...53), main="Nachher",
#        xlab = "Aspekt Ruhe", ylim = c(0,0.4))
#par(mfrow=c(1,1))



## Nas Raus
data[,28:57][is.na(data[,28:57])] <- 0

## Berechnung Score Vorher

ScoreVorher <- numeric(141)
gesum <- numeric(141)
for(j in 1:141) {
  for (i in 0:9) {
    ScoreVorher[j] <- ScoreVorher[j] + (as.numeric(data[j, 28 + i]) * as.numeric(data[j, 38 + i]))
    gesum[j] <- gesum[j] + as.numeric(data[j, 28 + i])
  }
  if(gesum[j] != 0){
    ScoreVorher[j] <- ScoreVorher[j]/gesum[j]
  }
  else{
    ScoreVorher[j] <- ScoreVorher[j]
  }
}

## Berechnung Score Nachher

ScoreNachher <- numeric(141)
for(j in 1:141) {
  for (i in 0:9) {
    ScoreNachher[j] <- ScoreNachher[j] + (as.numeric(data[j, 28 + i]) * as.numeric(data[j, 48 + i]))
  }
  if(gesum[j] != 0){
  ScoreNachher[j] <- ScoreNachher[j]/gesum[j]
  }
  else{
    ScoreNachher[j] <- ScoreNachher[j]
  }
}
ScoreNachher[which(ScoreNachher < 1)] <- NA
ScoreVorher[which(ScoreVorher < 1)] <- NA
meanV <- mean(ScoreVorher, na.rm = T)  #2.19936
meanN <- mean(ScoreNachher, na.rm = T) #2.727848
varV <- var(ScoreVorher, na.rm = T)  #0.3812979
varN <- var(ScoreNachher, na.rm = T) #0.5709283
ScoreNachher[which(ScoreNachher < 1)] <- NA
ScoreVorher[which(ScoreVorher < 1)] <- NA
ScoreVorher
Scorediff <- ScoreVorher - ScoreNachher
data$ScoreV <- ScoreVorher
data$ScoreN <- ScoreNachher
data$ScoreDiff <- Scorediff

## Plots
Bachelor <- subset(data,data$Studium==1)
Master <- subset(data,data$Studium==2)

par(mfrow=c(1,2))
plot(data$ScoreV, col=0, ylim = c(6,1), main="Score Vorher", ylab="Score Vorher")
points(1:19,Master$ScoreV, ylim = c(6,1), col="blue", pch=16)
points(20:137,Bachelor$ScoreV, ylim = c(6,1), col="orange",pch=16)
abline(h=meanV, col="red", lwd=2)
plot(data$ScoreN, col=0,ylim = c(6,1),main="Score Nachher", ylab="Score Nachher")
points(1:19,Master$ScoreN, ylim = c(6,1), col="blue", pch=16)
points(20:137,Bachelor$ScoreN, ylim = c(6,1), col="orange",pch=16)
abline(h=meanN, col="red", lwd=2)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plot(data$ScoreV, col=1, ylim = c(1,6), main="Score Vorher", ylab="Score Vorher")
abline(h=meanV, col="red", lwd=2)
plot(data$ScoreN, col=1,ylim = c(1,6),main="Score Nachher", ylab="Score Nachher")
abline(h=meanN, col="red", lwd=2)
par(mfrow=c(1,1))


#Vorher und Nacher
par(mfrow=c(1,1)) 
plot(ScoreVorher, col="orange", pch=19, cex=1, ylab="Score")
points(1:141,ScoreNachher, col="darkblue",pch=19 )
#abline(h=meanN, col="darkblue")
#abline(h=meanV, col="orange")



#Scorediff Allg
Scorediff <- ScoreVorher-ScoreNachher
plot(Scorediff, ylim = c(-3,3), xaxt="n", pch=1)
abline(h=0)


#Scorediff in Abhängigkeit von Fak.
plot(data$ScoreDiff ~ data$Fakultät, ylim = c(-3,3), xaxt="n", pch=1)
abline(h=0)
#Scorediff in Abhängigkeit von Fahrzeit
plot(data$ScoreDiff ~ data$Fahrzeit, ylim = c(-3,3), pch=1)
abline(h=0)
#Scorediff in Abhängigkeit von Lernzeit Jetzt
plot(data$ScoreDiff ~ data$`Frage 2 (Jetzt)`, ylim = c(-3,3), pch=1)
abline
#Scorediff in Abhängigkeit der Geschlechter
boxplot(data$ScoreDiff ~ data$Geschlecht, ylim = c(-3,3), pch=1)
abline(h=0)

#Scorediff nach Master/Bachelor
plot(data$ScoreDiff ~ data$Studium, ylim = c(-3,3), pch=1)
abline(h=0)
boxplot(data$ScoreDiff ~ data$Studium, ylim = c(-3,3), pch=1)


plot(ScoreVorher~ data$Fakultät, col="orange", pch=19, cex=1, ylab="Score")
points(data$Fakultät,ScoreNachher, col="darkblue",pch=19 )

mean(as.numeric(data$`Frage 2 (Vorher)`), na.rm = T)
mean(as.numeric(data$`Frage 2 (Jetzt)`), na.rm = T)



#Lernorte vorher
seq1 <- seq(from=5, to=20, by=2)
vorherOrt <- numeric(8)
w <- 0
for(i in seq1){
  w <- w+1
  vorherOrt[w] <- sum(data[i], na.rm=T)
}
vorherOrtdf <- rbind(vorherOrt,names=colnames(data[seq1]))
barplot(as.numeric(vorherOrtdf[1,])~vorherOrtdf[2,], 
        xlab="Lernorte", ylab = "absolute Häufigkeit")

#Lernorte jetzt
seq2 <- seq(from=6, to=20, by=2)
jetztOrt <- numeric(8)
w <- 0
for(i in seq2){
  w <- w+1
  jetztOrt[w] <- sum(data[i], na.rm=T)
}
jetztOrtdf <- rbind(jetztOrt,names=colnames(data[seq2]))
barplot(as.numeric(jetztOrtdf[1,])~jetztOrtdf[2,], xlab="Lernorte",
        ylab = "absolute Häufigkeit")

par(mfrow=c(1,2))
barplot(as.numeric(vorherOrtdf[1,])~vorherOrtdf[2,],
        xlab="Lernorte", ylab = "absolute Häufigkeit",
        main="Vorher", ylim=c(0,100))
barplot(as.numeric(jetztOrtdf[1,])~jetztOrtdf[2,],
        xlab="Lernorte", ylab = "absolute Häufigkeit",
        main="Jetzt", ylim=c(0,100))
par(mfrow=c(1,1))

#Differenz
indi <- c(2,3,4,5,6,7,8)
diffOrtc <- jetztOrt[indi]-vorherOrt[indi]
diffOrt <- rbind(diffOrtc,names=c("EFB","CLS","Galerie","Fak","BCI","Süd","SRG"))
col1 <- c("red",rep("green3",5),"red")
barplot(as.numeric(diffOrt[1,])~diffOrt[2,], xlab="Lernorte",
        ylab = "absolute Häufigkeit", ylim=c(-10,40), col=col1)
abline(h=0)

#Nutzungsgrund

seq3 <- seq(from=21, to=26)
w <- 0
grundc <- numeric(6)
for(i in seq3){
  w = w + 1
  grundc[w] <- sum(data[i], na.rm = T)
}
Grund <- rbind(grundc,names=colnames(data[seq3]))
Grund[2,1] <- "Zeitüberbrückung"
barplot(as.numeric(Grund[1,])~Grund[2,], xlab="Grund",
        ylab="Absolute Häufigkeiten")

#Boxplot Ersatzbewertung-Fahrzeit
boxplot(as.numeric(data$Fahrzeit)~as.numeric(data$`Ersatzbew. (10)`),  na.rm=T)

#Erhebungsorte
barplot(table(data$Erhebungsort))

#mittelwerte wichtigkeit
Mittelwerte <- numeric(10)
seq4 <- seq(from=28, to=37)
mean(data[seq4], na.rm=T)
