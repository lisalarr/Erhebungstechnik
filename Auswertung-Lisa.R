rm(list = ls())
cat("\014")

library(readxl)
library(ggplot2)
library(circlize)
library(hrbrthemes)
library(networkD3)
library(tidyverse)
library(grDevices)

setwd("~/TU Dortmund/3. Semester/Erhebungstechniken/Gruppenarbeit")
data = read_excel("DatensatzFragebogenLJJY.xlsx")

# 1: Einleitung 
  # Motivation: Neubau der UB, Analyse geschaffener Alternativen
  # + allgemeine Informationen wie Anz. Lernplaetze etc. 
  # Forschungsfrage: Wie ist die aktuelle Lernort Situation der TU zu bewerten? 
  # Subfragen 

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
    # Feedback zur Reflexion

# 3: Stichprobe und Datensatz
  # Erhebung: Aushaendigen von Boegen und  
  # Ort: Campus, Lernorte der TU (Fokus: Galerie, Mathetower)
  # Zeitraum: 13.11. bis 26.11.2023
  # Stichprobe: 143 eingereichte Frageboegen, aber
    nrow(data) 
    drop = c(43, 71) 
    data = data[-drop, ]
    rm(drop)
    nrow(data) # 141 verwertbare Daten (zwei fast ausschliesslich NAs)
    # davon Nicht-TU-Studenten und Erstis ebenfalls loeschen (Kontrollfrage)
    data = data[-which(data[ ,64] == 0), ]
    nrow(data) # also nur noch 138 verwertbare
  # Variablen mit Skalenniveau: gleiche Typen gruppieren
    # 68 Variablen 
    # ID, Erhebungsort raus --> nur noch 66 
    # Nominal: Geschlecht (m/w/d), Studium (B/M), Fakultät (17 moegliche) 
    # Ordinal: fragebogentypisch das meiste 
      # Lernorte (v/n/s), Nutzungszweck (s)
      # Tabellen im Fokus: Kriterien und Anforderungen (allg, erfuellt v/n)
      # Gesamtbewertung, Ersatzbewertung (bei Nutzung UB), Feedback
    # Verhaeltnis: Dauer pro Woche (v/n), Fahrtzeit 
    
    # --> Auspraegungen nennen? 
    
# 4: Ergebnisse (Auswertung der einzelnen Fragen)

  # 1: Diese Woche bereits Lernorte genutzt
    # data[ ,2], numeric range 0-1 (0 nein, 1 ja)
    # Einstiegsfrage: gar nicht auswerten ("ohne Erhebungsabsicht") 
    # Lediglich Motivation fuer Fragebogen, nicht repraesentativ
  
  # 2: Woechentlich durchschnittliche Nutzung  
    # data[ ,3:4], character, Zeit in Stunden
    data[49, 3:4] = NA
    frage2 = as.data.frame(lapply(data[ ,3:4], as.numeric))
    mean(frage2[ ,1], na.rm = TRUE) # 11.44 Stunden
    mean(frage2[ ,2], na.rm = TRUE) # 10.10 Stunden
    # Weniger, aber Befragung anfangs des Semesters (keine Klausurenphase)
    var(frage2[ ,1], na.rm = TRUE) # 115.65 
    # Grosse Schwankungen in gegebenen Antworten
    
    # Fuer den Anhang: Boxplot um gesunkenen Trend zu verdeutlichen
    boxplot(na.omit(frage2[ ,1]), na.omit(frage2[ ,2]), 
            names = c("Vor WiSe 2023/24", "Während WiSe 2023/24"), 
            main = "Wöchentlich durchschnittliche Nutzung der Lernorte")
    rm(frage2)

  # 3: Verteilung  
    # data[ ,5:20], numeric range 0-1 (0 nein, 1 ja pro Standort)
    sankeyNetwork(data[seq(5, 20, 2)], data[seq(6, )])
    # Haeufigkeiten 
    freq = colSums(na.omit(data[ ,5:20])) # Nutzung
    sum_before = colSums(na.omit(data[ ,seq(5, 20, by = 2)])) # Orte vorher
    sum_now = colSums(na.omit(data[ ,seq(6, 20, by = 2)])) # Orte jetzt
    # Spalten (jetzt) und Zeilen (vorher) fuer alle Moeglichkeiten 
    spalten = c("SB", "EFB", "CLS", "Galerie", "Fakultät", "BCI", "Süd", "SRG")
    zeilen = c("UB", "EFB", "CLS", "Galerie", "Fakultät", "BCI", "Süd", "SRG")
    # Test-Matrix = Aenderung der Lernortnutzung (Anz. Leute, die wechselten)
    test = matrix(0, nrow = length(zeilen), ncol = length(spalten))
    rownames(test) = zeilen
    colnames(test) = spalten
    test = as.data.frame(test)
    names = names(freq)
    orte_vorher = names[seq(1, length(names), by = 2)]
    orte_nachher = names[seq(2, length(names), by = 2)]
    # NAs mit 0 ersetzen
    data[ ,5:20][is.na(data[ ,5:20])] = 0
    # Anzahl Leute, die ihre Lernorte gewechselt haben 
    for(i in 1:length(zeilen)){
      for(j in 1:length(spalten)){
        for(k in 1:length(data$`Frage 1`)){
          if((i == j)|(i == 1)){
            if((data[k, 3 + 2 * i][[1]] == 1) 
               & (data[k, 4 + 2 * j][[1]] == 1)){
              test[i, j] = test[i, j] + 1
            }
          }
          else if((data[k, 3 + 2 * i][[1]] == 1) 
                  & (data[k, 4 + 2 * j][[1]] == 1) 
                  & (data[k, 3 + 2 * j][[1]] == 0)){
            test[i, j] = test[i, j] + 1
          }
        }
      }
    }
    # Wenig genutzte Lernorte zur Uebersichtlichkeit entfernen 
    test = subset(test, select = -BCI)
    test = subset(test, select = -Süd)
    test = test[c(T, T, T, T, T, F, F, T), ]
      # SB EFB CLS Galerie Fak SRG
      # UB       9  25  14      48  56  26
      # EFB      1  14   4       5   1   3
      # CLS      0   0   3       2   0   1
      # Galerie  0   2   0      12   2   2
      # Fak      0   6  10      21  66   7
      # SRG      1   4   5      11   4  18

    test_lang = test %>% rownames_to_column %>% gather(key = 'key', value = 'value', -rowname) %>% filter(value > 0)
    colnames(test_lang) = c("source", "target", "value")
    test_lang$target = paste(test_lang$target, " ", sep="")
    nodes = data.frame(name = c(as.character(test_lang$source),
                                as.character(test_lang$target)) %>% unique())
    test_lang$IDsource = match(test_lang$source, nodes$name)-1 
    test_lang$IDtarget = match(test_lang$target, nodes$name)-1
    farben_blau_gruen = c(rgb(.5, .7, .4), rgb(.4, .6, .8), rgb(.8, .7, .4),
                          rgb(.8, .4, .4), rgb(.6, .4, .8) , rgb(.9, .6, .2) )
    farben = paste0('d3.scaleOrdinal().range(["', 
                    paste(farben_blau_gruen, collapse = '","'), '"])')
    sankey_diagramm = sankeyNetwork(Links = test_lang, Nodes = nodes,
                                    Source = "IDsource", Target = "IDtarget",
                                    Value = "value", NodeID = "name", nodePadding=15,
                                    sinksRight=FALSE, colourScale=farben, nodeWidth=45, fontSize=12.5)
    sankey_diagramm
    rm(farben, farben_blau_gruen, freq, i, j, k, names, orte_vorher, orte_nachher, 
       spalten, zeilen, nodes, sankey_diagramm, test, test_lang)
    # Wanderung erkennbar (beachte: SB zur Befragung noch nicht lang geoeffnet)
    # Vorher: UB und Fakultaeten am meisten genutzt 
    # Nachher: diversifiziertere Nutzung, weiterhin Fakultaeten am relevantesten,
    # aber Annahme der Galerie als neuer offizieller Lernort
    
    # UB und SB in Zahlen 
    sum(data[ ,5]) # 105/138 = 76.08% UB vorher
    sum(data[ ,6]) # 16/138 = 11.59% SB nachher 
    
    # Galerie in Zahlen
    sum(data[ ,11]) # 22/138 = 15.94% vorher
    sum(data[ ,12]) # 61/138 = 44.20% nachher 
    
    # Fakultaeten in Zahlen 
    sum(data[ ,13]) # 85/138 = 61.59% vorher
    sum(data [,14]) # 87/138 = 63.04% nachher
    
    # Fuer den Anhang: Barplots der Lernortnutzung 
    par(mfrow = c(1, 2))
    barplot(sum_before, las = 2, ylim = c(0, 100),
            ylab = "Anz. Studierende",
            names.arg = c("UB", "EFB", "CLS", "Galerie", "Fakultät", "BCI", "Süd", "SRG"),
            main = "Vorher")
    barplot(sum_now, las = 2, ylim = c(0, 100),
            ylab = "Anz. Studierende",
            names.arg = c("SB", "EFB", "CLS", "Galerie", "Fakultät", "BCI", "Süd", "SRG"),
            main = "Nachher")
    mtext("Nutzung der Lernorte", line = -1.5, outer = TRUE)
    dev.off()
    rm(sum_before, sum_now)

    # Lernorte vorher
    seq1 = seq(from = 5, to = 20, by = 2)
    vorherOrt = numeric(8)
    w = 0
    for(i in seq1) {
      w = w + 1 
      vorherOrt[w] = sum(data[i], na.rm = TRUE)
    }
    vorherOrtdf = rbind(vorherOrt, names = colnames(data[seq1]))
    # Lernorte nachher
    seq2 = seq(from = 6, to = 20, by = 2) 
    jetztOrt = numeric(8)
    w = 0
    for(i in seq2) {
      w = w + 1
      jetztOrt[w] = sum(data[i], na.rm = TRUE)
    }
    jetztOrtdf = rbind(jetztOrt, names = colnames(data[seq2]))
    # Fuer den Analyse Teil: GuV in absoluten HK
    indices = 2:8
    diffOrtc = jetztOrt[indices] - vorherOrt[indices]
    diffOrt = rbind(diffOrtc, names = c("EFB", "CLS", "Galerie", "Fakultät", "BCI", "Süd", "SRG"))
    barplot(as.numeric(diffOrt[1,]) ~ diffOrt[2,], 
            xlab = "Lernorte", ylab = "Absolute Häufigkeit", 
            ylim = c(-10, 40), col =  c("firebrick3", rep("palegreen3", 5), "firebrick3"),
            main = "Veränderung der Lernortnutzung")
    abline(h = 0, col = "black", lwd = 1.5)
    rm(diffOrt, jetztOrtdf, vorherOrtdf, diffOrtc, i, indices, jetztOrt,
       seq1, seq2,vorherOrt, w)
    # Vor allem Wanderung zur Galerie, aber auch Co-Learning-Space populaerer
    
  # 4: Zweck der Nutzung
    # data[ ,21:26], numeric range 0-1 (0 nein, 1 ja pro Zweck)
    # data[ ,27], character 
    # Fuer den Anhang, da Grafik ohne Mehrwert
    barplot(colSums(na.omit(data[ ,21:26])),
            ylab = "Anz. Studierende", main = "Zweck der Lernort-Nutzung")
    # Vielseitige Nutzung, also wichtige Thematik fuer Studienalltag
    length(which(data[ ,21] == 1)) # 60 Zeit rumkriegen 
    length(which(data[ ,22] == 1)) # 102 Abgaben (am wichtigsten)
    length(which(data[ ,23] == 1)) # 75 Vor-/Nachbereiten
    length(which(data[ ,24] == 1)) # 86 Gruppenarbeit
    length(which(data[ ,25] == 1)) # 77 Klausuren (wieder: falscher Zeitpunkt)
    length(which(data[ ,26] == 1)) # 22 Abschlussarbeiten (geringes Target)
    
  # 5: Anforderungen an Lernumgebung 
    # data[ ,28:37], numeric range 1-5 (sehr unwichtig - sehr wichtig)
    frage5 = data.frame(data[, 28:37])
    frage51 = lapply(frage5, as.numeric)
    frage512 = lapply(frage51, na.omit)
    sorted51 = frage51[order(-sapply(frage512, median))]
    means = lapply(frage5, na.omit)
    means1 = lapply(means, mean)
    sorted1 = sort(unlist(means1), decreasing = TRUE)

    # Plotten der durchschnittlichen Bewertung (weg)
    barplot(unlist(sorted1), 
         ylab = "", yaxt = "n", xlab = "", xaxt = "n",
         main = "Durchschnitte der Anforderungen an Lernorte")
    axis(1, at = 1:10, las = 1, labels = c("Platzgarantie", "Erreichbarkeit", "Stromversorgung", "Öffnungszeiten", "Ruhe", "Sicherheit", "Gruppenräume", "Barrierefreiheit", "Pausenbereiche", "Computer"))
    axis(2, at = 0:4, las = 1, labels = c("sehr unwichtig", "unwichtig", "neutral", "wichtig", "sehr wichtig"))
    
    # Plotten als Boxplots
    boxplot(x = sorted51, ylim = c(0, 6),
         ylab = "", yaxt = "n", xlab = "", xaxt = "n",
         main = "Anforderungen an Lernorte")
    axis(1, at = 1:10, las = 1, labels = c("Erreichbarkeit", "Öffnungszeiten", "Platzgarantie", "Stromversorgung", "Sicherheit", "Ruhe", "Gruppenräume", "Barrierefreiheit", "Pausenbereiche", "Computer"))
    axis(2, at = 1:5, las = 1, labels = c("sehr unwichtig", "unwichtig", "neutral", "wichtig", "sehr wichtig"))
    
  # PRE fuer 6-7: Berechnung des Scores mit Einbezug des Kriterienrankings
    data[ ,28:57][is.na(data[,28:57])] = 0
    ScoreVorher = numeric(138)
    ScoreNachher = numeric(138)
    gesum = numeric(138)
    
    for(j in 1:138) {
      for(i in 0:9) {
        ScoreVorher[j] = ScoreVorher[j] + (as.numeric(data[j, 28 + i]) * as.numeric(data[j, 38 + i]))
        gesum[j] = gesum[j] + as.numeric(data[j, 28 + i])
      }
      if(gesum[j] != 0){
        ScoreVorher[j] = ScoreVorher[j]/gesum[j]
      } else {
        ScoreVorher[j] = ScoreVorher[j]
      }
    }
    for(j in 1:138) {
      for(i in 0:9) {
        ScoreNachher[j] = ScoreNachher[j] + (as.numeric(data[j, 28 + i]) * as.numeric(data[j, 48 + i]))
      }
      if(gesum[j] != 0){
        ScoreNachher[j] = ScoreNachher[j]/gesum[j]
      } else {
        ScoreNachher[j] = ScoreNachher[j]
      }
    }
    rm(i, j)
    
    ScoreNachher[which(ScoreNachher < 1)] = NA
    ScoreVorher[which(ScoreVorher < 1)] = NA
    meanV = mean(ScoreVorher, na.rm = T)  # 2.40
    meanN = mean(ScoreNachher, na.rm = T) # 2.82
    varV = var(ScoreVorher, na.rm = T)  # 0.84
    varN = var(ScoreNachher, na.rm = T) # 0.67
    ScoreNachher[which(ScoreNachher < 1)] = NA
    ScoreVorher[which(ScoreVorher < 1)] = NA
    Scorediff = ScoreVorher - ScoreNachher
    data$ScoreV = ScoreVorher
    data$ScoreN = ScoreNachher
    data$ScoreDiff = Scorediff
    
  # 6: Umsetzung VOR WiSe 2023/24
    # data[ ,38:47], numeric range 1-6 (Schulnoten)
    # 4 sehr wichtig + 3 wichtig bewerten
    # "Erreichbarkeit", "Öffnungszeiten", "Platzgarantie" und 
    # "Stromversorgung", "Ruhe", "Gruppenräume"
    par(mfrow = c(3, 2))
    
    mosaicplot(~ factor(`Erreichbarkeit...38`, levels = 1:6,
                       labels = c("1", "2", "3", "4", "5", "6")) +
                 factor(`Erreichbarkeit...48`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")),
               data = data, ylab = "nachher", xlab = "vorher", 
               main = "Erreichbarkeit")
    mosaicplot(~ factor(`Öffnungszeiten...40`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")) +
                 factor(`Öffnungszeiten...50`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")),
               data = data, ylab = "nachher", xlab = "vorher", 
               main = "Öffnungszeiten")
    mosaicplot(~ factor(`Stromversorgung...44`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")) +
                 factor(`Stromversorgung...54`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")),
               data = data, ylab = "nachher", xlab = "vorher", 
               main = "Stromversorgung")
    mosaicplot(~ factor(`Ruhe...43`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")) +
                 factor(`Ruhe...53`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")),
               data = data, ylab = "nachher", xlab = "vorher", 
               main = "Ruhe")
    mosaicplot(~ factor(`Gruppenräume...45`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")) +
                 factor(`Gruppenräume...55`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")),
               data = data, ylab = "nachher", xlab = "vorher", 
               main = "Gruppenräume")
    mosaicplot(~ factor(`Platzgarantie...41`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")) +
                 factor(`Platzgarantie...51`, levels = 1:6,
                        labels = c("1", "2", "3", "4", "5", "6")),
               data = data, ylab = "nachher", xlab = "vorher", 
               main = "Platzgarantie", 
               color = ifelse(row(data$`Platzgarantie...41`) == column(data$`Platzgarantie...51`), "blue", "grey"))
                 
  # 7: Umsetzung AKTUELL im WiSe 2023/24
    # data[ ,48:57], numeric range 1-6 (Schulnoten)
    boxplot(x = data.frame(data$ScoreV, data$ScoreN), 
            ylim = c(6, 1), 
            ylab = "", yaxt = "n", xlab = "", xaxt = "n",
            main = "Gesamtbewertung der Lernorte an der TU anhand des Scores")
    axis(1, at = 1:2, las = 1, labels = c("vorher", "nachher"))
    axis(2, at = 1:6, las = 1, labels = c("sehr gut", "gut", "befriedigend", "ausreichend", "mangelhaft", "ungenügend"))
    # Insgesamt geringere Zufriedenheit (Median der Boxplots)
    # und: Score erklaeren

  # 8: Sonstige Aspekte 
    # data[ ,58], character 
    # data[ ,59:60], numeric range 1-6 (Schulnoten, vorher und jetzt)
    # Einzeln im Text nennen, keine grafische Auswertung 
    data[ ,58]
    # Automaten, garantierte Parkplaetze, Hoersaalnaehe, Tafeln, saubere Toiletten,
    # stabile Internetverbindung, Steckdosen, Wasserspender, Drucker, Gebets-
    # raeume, Atmosphaere (nicht ueberfuellt, Bsp. Mathetower)
    
  # 9: Allgemeine Bewertung der Lernsituation
    # data[ ,61], numeric range 1-4 (sehr schlecht - sehr gut)
    par(mfrow = c(1, 2))
    deleteZero = data[51,61]
    data[51,61] = NA
    # Fuer den Anhang
    barplot(table(na.omit(data[ ,61])),
            names.arg = c("Sehr schlecht", "Schlecht", "Gut", "Sehr Gut"),
            ylab = "Anz. Studierende", 
            main = "Allgemein",
            col = c("firebrick3", "firebrick1", "palegreen1", "palegreen3"))
    # Ueberwiegend "gut" bewertet 
    mean(na.omit(data$`Bewertung (9)`)) # 2.59, entspricht gut - sehr gut 
  
  # 10: Angemessener Ausgleich
    # data[ ,62], numeric range 0-1 (0 nein, 1 ja)
    barplot(table(na.omit(data[ ,62])),
            names.arg = c("nicht angemessen", "angemessen"),
            ylab = "Anz. Studierende",
            main = "Ersatz",
            col = c("firebrick3", "palegreen3"))    
    mtext("Bewertung der Lernortsituation", line = -1.5, outer = TRUE)
    # 83 nicht angemessen (= 60.14%) = Mehrheit
    # 30 angemessen (= 21.74%)
  
  # 11: Geschlecht
    # data[ ,63], numeric range 1-3 (weiblich, maennlich, divers)
    table(data[ ,63])
    # Sehr ausgeglichen (w/m identisch mit 68, 2 mal divers)
    # Also Aussagekraft Geschlechter-unabhaengig
    
  # 12: Aktuelle Studienphase 
    # data[ ,64], numeric range 0-2 (Bachelor, Master, Nein)
    table(data[ ,64]) 
    # 119 im Bachelor, 19 im Master
    # Aussagekraft fuer Master daher beschraenkt 
  
  # 13: Fakultaet
    # data[ ,65], character
    # Uninteressant, lediglich in Reflexion beruecksichtigen
  
  # 14: Durchschnittliche Fahrtzeit
    # data[ ,66], character
    frage14 = lapply(data[ ,66], as.numeric)
    mean(unlist(frage14), na.rm = TRUE) 
    rm(frage14)
    # 29.73 Minuten Weg zur Uni
    
  # (15:) Erhebungsort
    # data[ ,67], character
    # Ohne weitere Relevanz, daher keine Analyse
  
  # (16:) Feedback
    # data[ ,68], character
    # Einzelbetrachtung der Kommentare 
    # Insgesamt: positiv - wichtiges Thema, das die Studierenden beschaeftigt 

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