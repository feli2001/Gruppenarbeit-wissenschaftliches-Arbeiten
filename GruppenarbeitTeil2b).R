#Gruppenarbeit wiss. Arbeiten
#Teil 2
#b)

#Deskriptive Statistiken fuer kategoriale Variablen:
#Man kann hier zwischen nominalen und ordinalen Merkmalen differenzieren:
#Nominal sind solche, die sich nicht sortieren lassen. Auf unseren Datensatz
#bezogen betrifft das vor allem die Variablen Studienfach und Mathe LK, falls man die Variable
#fuer die Berechnungen nicht noch binaer umkodiert auf 0 (kein Mathe LK) oder 1 (Mathe LK):
#(um evtl Korrelationen zu berechnen bietet sich das natuerlich an)
#(Fuer die Kontrolle der Tests muesste der Datensatz im Master Branch heruntergeladen und
#eingelesen werden):

library(dplyr)
def_par <- par(no.readonly = TRUE)

#Statistiken (recht wenig Moeglichkeiten bei nominalen Merkmalen)
NominalStat<- function(x) {
  #Absolute Haeufigkeiten:
  AbsoluteHaeufigkeiten<-table(x)
  #Prozentwerte:
  Prozentwerte<- prop.table(table(x))
  #Modus:
  Modus<- which.max(table(x))
  return(list(AbsHaeufigkeiten=AbsoluteHaeufigkeiten, Prozentangaben=Prozentwerte, Modus=Modus))
  }

#Grafiken fuer nominale Merkmale:
#Pareto Diagramm:
NominalGraf1<- function(x){
  library(dplyr)
  a<-as.data.frame(table(x), col.names=c("Var", "Freq"))
  #Zunaechst muessen wir die abs. Haeufigkeiten in Dataframe-Form bringen
  b <- arrange(a, desc(Freq)) %>%
  mutate(cumsum = cumsum(Freq),
      freq = round(Freq / sum(Freq), 3),
      cum_freq = cumsum(freq))
  #Jetzt nach der Groesse sortieren mit Hilfe von arrange in absteigender
  #Reihenfolge von unseren abs. Haeufigkeiten die wir mit 
  #Freq benannt haben.
  #Mit mutate kann man weitere Elemente zu dem Dataframe hinzufuegen und
  #alle bisherigen einfach behalten, wir berechnen also zusaetzlich
  #noch die kumulierten Summenhaeufigkeiten und in einem naechsten Schritt
  #die kumulierten realtiven Haeufigkeiten gerundet auf auf 2 Stellen nach dem Komma.
  #Randeinstellungen vergroessern (oben wurden sie ja schon gespeichert):
  par(mar=c(5,5,4,5))
  pc <-barplot(b$Freq,  
                width = 1, space = 0.2, border = NA, axes = F,
                ylim = c(0, 1.05 * max(b$cumsum, na.rm = T)), 
                ylab = "Summenhaeufigkeiten" , cex.names = 0.7, 
                names.arg = a$Var, main = "Pareto Diagramm")
  #Hier erstellen wir den Plot zunaechst ohne Achsen und Box, die x-Werte
  #fuer die Verbindungslinien speichern wir aber in pc, sodass wir sie
  #nachtraeglich hinzufuegen koennen:
  lines(pc, b$cumsum, type = "b", cex = 0.7, pch = 19, col="cyan3")
  box(col = "grey62")
  #Box, um Grafik zu umranden
  axis(side = 2, at = c(0, b$cumsum), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
  axis(side = 4, at = c(0, b$cumsum), labels = paste(c(0, round(b$cum_freq * 100)) ,"%",sep=""), 
       las = 1, col.axis = "cyan4", col = "cyan4", cex.axis = 0.8)
  #x und y-Achse, mit zusaetzlicher Angabe von Prozenten fuer die bessere
  #Uebersichtlichkeit
  par(def_par) 
}

#Test (mit Datensatz aus Teil 1):
NominalGraf1(Datensatz$Studienfach)

#Ansonsten liesse sich hier noch ein einfaches Balkendiagramm zeichnen mit
#dem Befehl barplot(table(x)).


#Ordinale Merkmale (auf unseren Datensatz bezogen unter anderem 
#der Spaß an Mathe oder Programmieren):

#Statistiken:
OrdinalStat<- function(x) {
  #Ueberblick ueber die Daten verschaffen analog zu nominalen Merkmalen:
  #Absolute Haeufigkeiten:
  AbsoluteHaeufigkeiten<-table(x)
  #Prozentwerte:
  Prozentwerte<- prop.table(table(x))
  #Modus:
  Modus<- which.max(table(x))
  #Hier lassen sich auch sinnvoll Median und Quantile zur Interpreatation berechnen:
  Minimum<- min(x)
  Maximum<- max(x)
  Spannweite<- range(x)
  Median<- median(x)
  Quantil1<-quantile(x, 0.25) #nach der Definition aus Stat1
  Quantil2<- quantile(x,0.75)
  return(list(AbsHaeufig=AbsoluteHaeufigkeiten, Prozentangaben=Prozentwerte, Modus=Modus,Minimum=Minimum, Maximum=Maximum,Spannweite=Spannweite, Median=Median, Quantile=c(Quantil1, Quantil2)))
}

#Test:
OrdinalStat(Datensatz$IntMath)

#Visualisierungen:
OrdinalGraf<- function(x) {
  par(mfrow=c(1,2))
  plot(ecdf(x), xlab="klassierte Werte", ylab="empirische Verteilung", main="Empirische Verteilungsfunktion")
  #Empirische Verteilungsfunktionen lassen sich zwar besser bei kardinalen Merkmalen
  #anwenden, sie sind jedoch auch fuer ordinale geeignet
  barplot(table(x), xlab="Kategorien", ylab="Absolute Haeufigkeiten", main="Balkendiagramm x", ylim=c(0,1.2* max(table(x))))
  par(mfrow=c(1,1))
}
#Test:
OrdinalGraf(Datensatz$IntMath) 







