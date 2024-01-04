rm(list = ls())
cat("\014")

library(readxl)
library(ggplot2)
library(circlize)
library(hrbrthemes)
library(networkD3)
library(tidyverse)
library(grDevices)

# Vorbereitung fuer den Bericht 

setwd("~/TU Dortmund/3. Semester/Erhebungstechniken/Gruppenarbeit")
data = read_excel("DatensatzFragebogenLJJY.xlsx")

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
    # 68 Variablen 
    # --- 

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
    boxplot(na.omit(frage2[ ,1]), na.omit(frage2[ ,2]), 
            names = c("Vor WiSe 2023/24", "Während WiSe 2023/24"), 
            main = "Wöchentlich durchschnittliche Nutzung der Lernorte")
    rm(frage2)
    # Gesunkener Trend wie im mean bereits erkennbar 
  
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
    # Wanderung erkennbar: UB viel genutzt, Aufteilung auf verschiedene Lernorte
    # Also: diversifiziertere Nutzung aktuell
    # Beachte: Sebrath-Bib zum Zeitpunkt der Befragung noch nicht lang geoeffnet

    # Barplots der Lernortnutzung
    par(mfrow = c(1, 2))
    barplot(sum_before, las = 2, ylim = c(0, 100),
            ylab = "Anzahl an Personen",
            names.arg = c("UB", "EFB", "CLS", "Galerie", "Fakultät", "BCI", "Süd", "SRG"),
            main = "Vorher")
    barplot(sum_now, las = 2, ylim = c(0, 100),
            ylab = "Anzahl an Personen",
            names.arg = c("SB", "EFB", "CLS", "Galerie", "Fakultät", "BCI", "Süd", "SRG"),
            main = "Nachher")
    mtext("Nutzung der Lernorte", line = -1.5, outer = TRUE)
    dev.off()
    rm(sum_before, sum_now)
    # Gleicher Trend erkennbar 
  
  # 4: Zweck der Nutzung
    # data[ ,21:26], numeric range 0-1 (0 nein, 1 ja pro Zweck)
    # data[ ,27], character 
    barplot(colSums(na.omit(data[ ,21:26])))
    # Vielseitige Nutzung, also wichtige Thematik fuer Studienalltag
    # Am Meisten für Abgaben, am wenigsten für Abschlussarbeiten
  
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
    deleteZero = which(data[ ,61] == 0)
    data[, 61][deleteZero] = NA
    barplot(table(na.omit(data[ ,61])), 
            names.arg = c("Sehr schlecht", "Schlecht", "Gut", "Sehr Gut"))
    # Ueberwiegend "gut" bewertet 
    mean(na.omit(data[ ,61]))
    # 2.602941
  
  # 10: Angemessener Ausgleich
    # data[ ,62], numeric range 0-1 (0 nein, 1 ja)
    barplot(table(na.omit(data[ ,62])))
    # Mit Anteilen der Bib-Nutzung (Plot von Jacky) 
    mosaicplot(~ factor(`UB(V)`, levels = c(1,0), labels = c("Ja", "Nein")) +
                 factor(`Ersatzbew. (10)`, levels = c(1,0), labels = c("Ja", "Nein")),
               data = data, ylab = "Ersatz ausreichend", xlab= "Bibliothek benutzt",
               main = "Ersatzbewertung")
  
  # 11: Geschlecht
    # data[ ,63], numeric range 1-3 (weiblich, maennlich, divers)
    table(data[ ,63]) # sehr ausgeglichen 
    data[ ,63] = factor(data[ ,63], 
                        labels = c("Weiblich", "Männlich", "Divers"))
    barplot(table(data[ ,63]),
            main = "Geschlechterverteilung", 
            col = c("red", "blue", "yellow"),
            xlab = "Geschlecht", 
            ylab = "Anzahl an Personen")
    # 
  
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