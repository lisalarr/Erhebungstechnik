library(readxl)
library(ggplot2)

setwd("~/TU Dortmund/3. Semester/Erhebungstechniken/Gruppenarbeit")
data = read_excel("DatensatzFragebogenLJJY.xlsx")

# Vorbereitung Bericht 
# 1: Einleitung 
# Motivation: Neubau der UB, Analyse geschaffener Alternativen
# + allgemeine Informationen wie Anz. Lernplaetze etc. (s. Johanna)
# Forschungsfrage: Wie ist die aktuelle Lernort Situation der TU zu bewerten? 
# Subfragen (s. Liste Johanna) 

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
# Erhebung: Aushaendigen von Boegen und online Ausfuellen 
# Ort: Campus, Lernorte der TU (Fokus: Galerie, Mathetower)
# Zeitraum: 13.11. bis 26.11.2023
# Stichprobe: 143 eingereichte Frageboegen, aber
  nrow(data) 
  drop = c(43, 71) 
  data = data[-drop, ]
  rm(drop)
  nrow(data) # 141 verwertbare Daten
  # davon Nicht-TU-Studenten und Erstis ebenfalls loeschen
  data = data[-which(data[ ,64] == 0), ]
  nrow(data) # also nur noch 138 verwertbare
# Variablen mit Skalenniveau: gleiche Typen gruppieren
  # --- 

# 4: Ergebnisse 

# Auswertung der einzelnen Fragen
# 1: Diese Woche bereits Lernorte genutzt
# data[ ,2], numeric range 0-1 (0 nein, 1 ja)
# Einstiegsfrage: gar nicht auswerten (ohne Erhebungsabsicht) 
# Motivation fuer Fragebogen, nicht repraesentativ

# 2: Woechentlich durchschnittliche Nutzung  
# data[ ,3:4], character, Zeit in Stunden
data[49, 3:4] = NA
frage2 = as.data.frame(lapply(data[ ,3:4], as.numeric))
# Bereinigt zu numeric
mean(frage2[ ,1], na.rm = TRUE) # 12.13 Stunden
mean(frage2[ ,2], na.rm = TRUE) # 10.84 Stunden (weniger!)
boxplot(na.omit(data[ ,3]), as.numeric(na.omit(data[ ,4]), names = c("Group1", "Group2"), main = "Boxplots nebeneinander")))

# 3: Verteilung  
# data[ ,5:20], numeric range 0-1 (0 nein, 1 ja pro Standort)
# (Plot mit Wanderung von Jacky + nebeneinander Barplot)
# Trend: viel UB vorher (Wanderung nach ...)
# Neue Lernorte werden genutzt 

# 4: Zweck der Nutzung
# data[ ,21:26], numeric range 0-1 (0 nein, 1 ja pro Zweck)
# data[ ,27], character 
barplot(colSums(na.omit(data[ ,21:26])))
# Divers, also wichtig

# 5: Anforderungen an Lernumgebung 
# data[ ,28:37], numeric range 1-5 (sehr unwichtig - sehr wichtig)
# (Plot mit Durchschnitten von Yannick)

# 6: Umsetzung VOR WiSe 2023/24
# data[ ,38:47], numeric range 1-6 (Schulnoten)
# (score)

# 7: Umsetzung AKTUELL im WiSe 2023/24
# data[ ,48:57], numeric range 1-6 (Schulnoten)
# (score)

# 8: Sonstige Aspekte 
# data[ ,58], character 
# data[ ,59:60], numeric range 1-6 (Schulnoten, vorher und jetzt)
# (einzeln im Text nennen) 

# 9: Allgemeine Bewertung der Lernsituation
# data[ ,61], numeric range 1-4 (sehr schlecht - sehr gut)
barplot(table(na.omit(data[ ,61])))

# 10: Angemessener Ausgleich
# data[ ,62], numeric range 0-1 (0 nein, 1 ja)
barplot(table(na.omit(data[ ,62])))
# (Mosaikplot Jacky)

# 11: Geschlecht
# data[ ,63], numeric range 1-3 (weiblich, maennlich, divers)
table(data[ ,63]) # sehr ausgeglichen 

# 12: Aktuelle Studienphase 
# data[ ,64], numeric range 0-2 (Bachelor, Master, Nein)
table(data[ ,64]) # am meisten Bachelor

# 13: Fakultaet
# data[ ,65], character
# uninteressant, lediglich in Reflexion 

# 14: Durchschnittliche Fahrtzeit
# data[ ,66], character
  # ---
# Bereinigt zu numeric (Angabe in Minuten)

# (15:) Erhebungsort
# data[ ,67], character
# Ohne weitere Relevanz, daher keine Analyse

# (16:) Feedback
# data[ ,68], character
# Positiv - wichtiges Thema, das die Studierenden beschaeftigt 

# 5: Diskussion
# Einordnung: 
# Lernsituation im Allgemeinen relativ gut 
# Aber mehrheitlich kein angemessener Ersatz der UB (Einzelaspekte!)
# Limitationen:
# Sebrath erst seit einem Monat geoeffnet 
# Lernpensum anfangs des Semesters geringer (Erfassung in Klausurenphase interessanter)
# Gleichmaessigere Erfassung verschiedener Fakultaeten aussagekraeftiger 
# Je nach Fakultaet unterschiedlich gutes Angebot an eigenen Raeumlichkeiten
# Je nach Fakultaet individuelle Auslastung 
# Aussagekraft primaer fuer Bachelor 

# 6: Reflexion der fragebogengestuetzten Erhebung 
# Erhebungsort bei diesem Stichprobenumfang und hoher Anzahl an Online-Einreichungen nicht sinnvoll
# Spaetere Erhebung
# Gleichmaessigere Verteilung (ggf. online spreaden / QR Code)
# Studierende vor Ort getroffen -> viele, die zuhause lernen nicht beruecksichtigt