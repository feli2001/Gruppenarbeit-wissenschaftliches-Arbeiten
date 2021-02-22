#Skript 1 Aufgabenteil 3

#a)
## Eine Funktion, die verschiedene geeignete deskriptive Statistiken fuer
## metrische Variablen berechnet und ausgibt:

# Deskriptive Statistiken fuer metrische Variablen
# - arithmetisches Mittel
# - Median
# - Standardabweichung bzw. Varianz
# - Minmum, Maximum
# - Quantile

## Vorarbeit: Funktionen, die in der Hauptfunktion angewendet werden:
#(siehe Skript 2 fuer Erklaerungen)
# Funktion fuer arithmetisches Mittel:
ar_mittel <- function(x){
  if(!(is.numeric(x))){stop("x muss ein numerischer Vektor sein!")}
  N <- length(x)
  a <- 1/N * sum(x)
  return(a)
}

# Funktion fuer das Maximum und das Minumum:
max_min <- function(x){
  if(!(is.numeric(x))){stop("x muss ein numerischer Vektor sein!")}
  s <- sort(x)
  N <- length(x)
  min <- s[1]
  max <- s[N]
  return(c(min,max))
}


## Hauptfunktion:
meth_func <- function(x){
  if(!(is.numeric(x))){stop("x muss ein numerischer Vektor sein!")}
  L <- list(Arithmetisches_Mittel = ar_mittel(x), Median = median(x),
            Quantile = quantile(x), Varianz = var(x),
            Standardabweichung = sd(x), Minimum_Maximum = max_min(x))
  return(L)
}

#b) 
## Eine Funktion, die verscheidene geeignete deskriptive Statistiken fuer
## kategoriale Variablen berechnet und ausgibt:

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


#c)
## Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
## den Zusammenhang zwischen zwei kategorialen Variablen berechnet und ausgibt:
# (auch hierfuer wird wieder das zweite Skript benoetigt, Funktion ist
#  jedoch ohne Erklaerung angefuegt):

kategorial_bivariat <- function(x,y)
{
  # Haeufigkeitstabelle
  a <- table(x, y)
  Gesamt <- margin.table(table(x, y),1)
  d <- margin.table(table(x, y),2)
  tabelle <- rbind(cbind(a,Gesamt),c(d,sum(d)))
  row.names(tabelle) <- c(row.names(a),"Gesamt")
  #######################################################################
  # Funktion aus dem Skript 2 ausfuehren
  eij <- Erwartungshaeufigkeiten(x,y)
  #######################################################################
  # Cramers Kontingenzkoeffizient
  kc <- sqrt(as.numeric(suppressWarnings(chisq.test(x,y)$statistic))/(tabelle[max(row(tabelle)),
                                                                              max(col(tabelle))]*(min(c(max(row(tabelle)),max(col(tabelle))))-1)))
  #######################################################################
  # Pearsons Kontingenzindex und korrigierter Pearson Index
  kp <- sqrt(as.numeric(suppressWarnings(chisq.test(x,y)$statistic))/(as.numeric(suppressWarnings(chisq.test(x,y)$statistic))
                                                                      +tabelle[max(row(tabelle)),max(col(tabelle))]))
  kpcorr <- sqrt(min(c(max(row(tabelle)),max(col(tabelle))))/(min(c(max(row(tabelle)),
                                                                    max(col(tabelle))))-1)*kp)
  #######################################################################
  # Korrelationskoeffizient und Kovarianz
  if(is.numeric(x) && is.numeric(y))
  {
    cor <- c(cor(x,y,method = "spearman"),cor(x,y,method = "kendall"))
    cov <- c(cov(x,y,method = "spearman"),cor(x,y,method = "kendall"))
  }
  else
  {
    cor <- "x oder y nicht numerisch: nicht berechenbar"
    cov <- "x oder y nicht numerisch: nicht berechenbar"
  }
  #######################################################################
  #######################################################################
  # Ausgabe
  list("Kontingenztafel" = tabelle,"Erwartungshäufigkeiten" = as.table(eij),"Cramers Kontingenzkoeffizient" = kc,"Pearsons Kontingenzindex" = kp,
       "korrigierter Pearson Index" = kpcorr,"Korrelationskoeffizient (nach Spearman/Kendall)" = cor,"Kovarianz (nach Spearman/Kendall)" = cov)
}

#Funktion aus Skript 2:
Erwartungshaeufigkeiten <- function(x,y)
{
  # Haeufigkeitstabelle
  a <- table(x, y)
  Gesamt <- margin.table(table(x, y),1)
  d <- margin.table(table(x, y),2)
  tabelle <- rbind(cbind(a,Gesamt),c(d,sum(d)))
  row.names(tabelle) <- c(row.names(a),"Gesamt")
  #####################################################################################
  # Erwartungshaeufigkeiten
  z <- NULL
  j <- 1
  while(j < max(col(tabelle)))
  {
    i <- 1
    while(i < max(row(tabelle)))
    {
      z <- c(z, tabelle[max(row(tabelle)),j]*tabelle[i,max(col(tabelle))]/tabelle[max(row(tabelle)),max(col(tabelle))])
      i <- i+1
    }
    j <- j+1
  }
  t1 <- matrix(z, ncol = max(col(tabelle))-1)
  t2 <- cbind(t1, Gesamt)
  eij <- rbind(t2, c(d,sum(d)))
  row.names(eij) <- dimnames(tabelle)[[1]]
  colnames(eij) <- dimnames(tabelle)[[2]]
  eij
}

#d)
## Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammenhang zwischen einer metrischen und einer dichotomen Variable
## berechnet und ausgibt:

# Die punktbiseriale Korrelation eignet sich, um einen linearen Zusammenhang
# zwischen einem metrischen Merkmal und einem dichotomen Merkmal zu messen.
# Dabei handelt es sich lediglich um einen Spezialfall des Korrelationskoeffi-
# zienten nach Pearson. 

## Eingabeparameter:
# Als Eingabeparameter haben wir unsere dichotomische Variable, unsere 
# metrische Variable sowie unser Dataframe "data", die unsere beiden Variablen
# beinhaltet. 
## einzelne Variablen in der Funktion:
# y0: Alle metrischen Merkmalsausprägungen, bei denen die dichotomische 
# Variable gleich 0 ist
# y1: Alle metrischen Merkmalsausprägungen, bei denen die dichotomische 
# Variable gleich 1 ist
## Anmerkung:
# Statt der Standardabweichung aus R wurde hier die uns bekannte Variante aus
# der Veranstaltung "Deskriptive Verfahren" gewählt mit 1/N als Vorfaktor
# statt 1/(N-1). Aus dem Grund wird sd(data$metric_variable) mit 
# sqrt((n-1)/n)) multipliziert. 


pbcor <- function(dicho, metric) {
  y0 <- which(dicho == "0")
  a0<- metric[y0]
  y1 <- which(dicho == "1")
  a1<- metric[y1]
  meana0 <- mean(a0)
  meana1 <- mean(a1)
  n0 <- length(a0)
  n1 <- length(a1)
  n <- n0 + n1
  return(((meana1 - meana0) / (sd(metric)) * sqrt((n-1)/n))
         * sqrt(n0*n1/(n^2)))
}

#e) 
## Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert
## kategorisiert ("niedrig", "mittel", "hoch"):

#Ueberpruefen, ob die eingegebene Variable quantilbasiert kategorisiert werden kann.
transformieren <- function(x)
{
  if (is.numeric(x)) return(x)
  else
  {
    if(is.factor(x)) 
    {
      x <- as.numeric(x)
      return(x)
    }
    else stop("Diese Variable kann nicht quantilbasiert kategorisiert werden")
  }
}

#Kategorisierung der Variable
kategorisieren_3 <- function(x)
{
  qtil <- quantile(x,  probs = c(1/3, 2/3))
  vec <- x
  for(i in c(1:length(vec))) 
  {
    if(vec[i] >= qtil[2]) vec[i] <- "Hoch"
    else
    {
      if(vec[i] >= qtil[1]) vec[i] <- "Mittel"
      else vec[i] <- "Niedrig"
    }
  }
  vec <- as.factor(vec)
  levels(vec) <- c("Niedrig", "Mittel", "Hoch")
  return(vec)
}

kategorisieren_4 <- function(x)
{
  qtil <- quantile(x,  probs = c(1/4, 2/4, 3/4))
  vec <- x
  for(i in c(1:length(vec))) 
  {
    if(vec[i] >= qtil[3]) vec[i] <- "Hoch"
    else
    {
      if(vec[i] >= qtil[2]) vec[i] <- "Eher hoch"
      else 
      {
        if(vec[i] >= qtil[1]) vec[i] <- "Eher niedrig"
        else vec[i] <- "Niedrig"
      }
    }
  }
  vec <- as.factor(vec)
  levels(vec) <- c("Niedrig", "Eher niedrig", "Eher hoch","Hoch")
  return(vec)
}

#Die main-Funktion benutzen: x ist der Variablenvektor und num ist die Anzahl der Kategorien
func_e <- function(x, num)
{
  if(num ==3) return(kategorisieren_3(transformieren(x)))
  else {
    if(num == 4) return(kategorisieren_4(transformieren(x)))
    else stop("Diese Kategorisierung ist nicht verfuegbar")
  }
}

#f)
## Eine Funktion, die eine geeignete Visualisierung von drei oder vier
## kategorialen Variablen erstellt:

#Ich habe unter die einzelnen Funktionen schonmal Beispiele für mögliche
#Anwendungen, bezogen auf unseren Datensatz, gegeben.

install.packages("vcd")
install.packages("MASS")
install.packages("colorspace")
install.packages("grid")

library("vcd")
library("MASS")
library("colorspace")
library("grid")

#Datensatz <- read.csv("DatenGruppenarbeit.csv")

#Funktion zur Visualisierung einer kategorieller Variablen.
VisKat1 <- function(Var,NameVariable = " ")
{
  return(barplot(table(Var), xlab = NameVariable, ylab = "absolute Häufigkeit", main = paste("Visualisierung der Variable",NameVariable)))
}

#Beispiel zur Verwendung:
#VisKat1(Datensatz$Studienfach,"Studienfach")
#oder:
#VisKat1(Datensatz$Alter,"Alter")


#Funktion zur Visualisierung zweier kategorieller Variablen.
VisKat2 <- function(Var1,Var2,NameVar1 = "Variable 1",NameVar2 = "Variable 2")
{
  Bla <- data.frame(Var1,Var2)
  Bla <- structable(Bla)
  mosaic(Bla, main = "Mosaikplot zweier Variablen", shade = TRUE, legend = TRUE,
         labeling_args = list(set_varnames = c(Var1 = NameVar1,Var2 = NameVar2)))
}

#Beispiel zur Verwendung:
#VisKat2(Datensatz$Studienfach,Datensatz$MatheLK,"Studienfach","Mathe LK")



#Funktion zur Visualisierung dreier oder vierer kategorieller Variablen.
VisKat34 <- function(Var1,Var2,Var3,Var4 = 0, NameVar1 = "Variable 1",NameVar2 = "Variable 2", NameVar3 = "Variable 3", NameVar4 = "Variable 4", Titel = "Visualisierung mehrerer kategorieller Variablen")
{
  if(Var4 == 0)
  {
    Bla <- data.frame(Var1,Var2,Var3)
    Bla <- structable(Bla)
    return(mosaic(Bla, shade = TRUE, legend = TRUE, main = Titel,
                  labeling_args = list(set_varnames = c(Var1 = NameVar1, 
                                                        Var2 = NameVar2, Var3 = NameVar3))))  
  } else{
    Bla <- data.frame(Var1,Var2,Var3,Var4)
    Bla <- structable(Bla)
    return(mosaic(Bla, shade = TRUE, legend = TRUE, main = Titel,
                  labeling_args = list(set_varnames = c(Var1 = NameVar1,Var2 = NameVar2,
                                                        Var3 = NameVar3,Var4 = NameVar4))))
  }
  
}

#Beispiel zur Verwendung:

#VisKat34(Datensatz$Studienfach,Datensatz$IntMath,Datensatz$IntProg,Datensatz$MatheLK,"Studienfach","IntMath","IntProg","MatheLK")

#oder verständlicher (für 4 Variablen):
#VisKat34(Var1 = Datensatz$Studienfach, Var2 = Datensatz$IntMath, Var3 = Datensatz$IntProg, 
#         Var4 = Datensatz$MatheLK, NameVar1 = "Studienfach", NameVar2 = "IntMath",
#         NameVar3 = "IntProg", NameVar4 = "MatheLK", Titel = "Darstellung mehrerer Variablen")


#oder verständlicher (für 3 Variablen): 
#VisKat34(Var1 = Datensatz$Studienfach, Var2 = Datensatz$IntMath, Var3 = Datensatz$IntProg, 
#         NameVar1 = "Studienfach", NameVar2 = "IntMath",
#         NameVar3 = "IntProg", Titel = "Darstellung mehrerer Variablen")

