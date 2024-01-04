library(readxl)
library(ggplot2)

setwd("~/TU Dortmund/3. Semester/Erhebungstechniken/Gruppenarbeit")
data = read_excel("DatensatzFragebogenLJJY.xlsx")

# Vorbereitung Bericht 
# 1: Einleitung 
# Motivation: Neubau der UB, Analyse geschaffener Alternativen
# Forschungsfrage: Wie ist die aktuelle Lernort Situation der TU zu bewerten? 

# 2: Erhebungsinstrument 
# Vierseitiger Fragebogen 
# (i) Nutzung der Lernorte
# Allgemeine Infos ueber Nutzung (diese Woche, Durchschnitt pro Woche (v/n), welche, Zweck)
# Anforderungen an Lernort (Relevanz in Tabelle ankreuzen, Moeglichkeit "sonstiges" spaeter)
# Bewertung der Umsetzung (v/n) in Tabelle 
# Gesamtbewertung Situation an TU
# Einschaetzung, ob angemessener Ausgleich in Ueberbrueckungsphase
# (ii) Allgemeines
# Geschlecht
# Phase im Studium (Bachelor/Master), nochmals Filter: kein TU Student 
# Fakultaet
# Durchschnittliche Fahrtzeit 
# (iii)
# Feedback

# 3: Stichprobe und Datensatz
# Erhebung: Aushaendigen von Boegen, online Ausfuellen (PDF)
# Stichprobe: 143 eingereichte Frageboegen
# Variablen mit Skalenniveau 
# -------------

# 4: Ergebnisse 
# Gesamtzahl an Teilnehmern 
nrow(data) # 143 eingereicht
drop = c(43, 71) 
data = data[-drop, ]
rm(drop)
nrow(data) # 141 verwertbare Daten
# Nicht-TU-Studenten und Erstis ebenfalls loeschen
data = data[-which(data[ ,64] == 2), ]
nrow(data) # nur noch 122 verwertbare

# Auswertung der einzelnen Fragen
# 1: Diese Woche bereits Lernorte genutzt
# data[ ,2], numeric range 0-1 (0 nein, 1 ja)
length(which(data$`Frage 1` == 1)) # 103
103/122 # 84.43% 

# 2: Woechentlich durchschnittliche Nutzung  
# data[ ,3:4], character, Zeit in Stunden
data[49, 3:4] = NA
frage2 = as.data.frame(lapply(data[ ,3:4], as.numeric))
# Bereinigt zu numeric
mean(frage2[ ,1], na.rm = TRUE) # 12.13 Stunden
mean(frage2[ ,2], na.rm = TRUE) # 10.84 Stunden (weniger!)

# 3: Verteilung  
# data[ ,5:20], numeric range 0-1 (0 nein, 1 ja pro Standort)
optionen = c("UB", "Sebrath", "EF", "Co-Learning", "Galerie", "Fakultät", "CT BCI", "Süd", "SRG I")
wann = c("V", "N")
# stacked barplot mit ggplot2 
# x: optionen
# y: 0-122(?)
# fill: vorher nachher

# 4: Zweck der Nutzung
# data[ ,21:26], numeric range 0-1 (0 nein, 1 ja pro Zweck)
# data[ ,27], character 


# 5: Anforderungen an Lernumgebung 
# data[ ,28:37], numeric range 1-5 (sehr unwichtig - sehr wichtig)

# 6: Umsetzung VOR WiSe 2023/24
# data[ ,38:47], numeric range 1-6 (Schulnoten)

# 7: Umsetzung AKTUELL im WiSe 2023/24
# data[ ,48:57], numeric range 1-6 (Schulnoten)

# 8: Sonstige Aspekte 
# data[ ,58], character 
# data[ ,59:60], numeric range 1-6 (Schulnoten, vorher und jetzt)

# 9: Allgemeine Bwertung der Lernsituation
# data[ ,61], numeric range 1-4 (sehr schlecht - sehr gut)

# 10: Angemessener Ausgleich
# data[ ,62], numeric range 0-1 (0 nein, 1 ja)

# 11: Geschlecht
# data[ ,63], numeric range 1-3 (weiblich, maennlich, divers)
table(data[ ,63]) # sehr ausgeglichen 

# 12: Aktuelle Studienphase 
# data[ ,64], numeric range 0-2 (Bachelor, Master, Nein)
table(data[ ,64]) # am meisten Bachelor

# 13: Fakultaet
# data[ ,65], character
  # --- 
# Bereinigt zu numeric range 1-17 (Liste TU)

# 14: Durchschnittliche Fahrtzeit
# data[ ,66], character
  # ---
# Bereinigt zu numeric (Angabe in Minuten)

# (15:) Erhebungsort
# data[ ,67], character
# Ohne weitere Relevanz, daher keine Analyse

# (16:) Feedback
# data[ ,68], character

# 5: Diskussion
# Einordnung: 
# Lernsituation im Allgemeinen relativ gut 
# Aber mehrheitlich kein angemessener Ersatz der UB (Einzelaspekte!)
# Limitationen:
# Lernpensum anfangs des Semesters geringer (Erfassung in Klausurenphase interessanter)
# Gleichmaessigere Erfassung verschiedener Fakultaeten aussagekraeftiger 
# Je nach Fakultaet unterschiedlich gutes Angebot an eigenen Raeumlichkeiten
# Je nach Fakultaet individuelle Auslastung 
# Aussagekraft nur fuer Bachelor 

# 6: Reflexion der fragebogengestuetzten Erhebung 
# Erhebungsort bei diesem Stichprobenumfang und hoher Anzahl an Online-Einreichungen nicht sinnvoll
# Spaetere Erhebung
# Gleichmaessigere Verteilung (ggf. online spreaden / QR Code)