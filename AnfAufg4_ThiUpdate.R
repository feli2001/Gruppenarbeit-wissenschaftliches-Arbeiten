##Hier ist ein Update zu dem originalen Skript von Lutz.
##Meine Ergänzung steht unter die Teile, die mit "##" markiert sind.

#Datensatz <- read.csv("DatenGruppenarbeit.csv")


#Aufgabe 4 - Auswertung der Daten 

#Datensatz <- read.csv("DatenGruppenarbeit.csv")

#Im Folgenden wird der Datensatz ausgewertet.
# Diese Version ist nicht die fertige, sondern nur der Anfang der Aufgabe 4. 

# Zunächst geht es darum, sich einen groben Überblick über die Daten zu verschaffen:

meth_func(Datensatz$Alter)
# Alle relevanten deskriptiven Statistiken zu der Variable Alter.

NominalStat(Datensatz$MatheLK)
# 68% der Personen besuchten den Mathe-LK.

NominalGraf1(Datensatz$MatheLK)
# Visuelle Darstellung der Variable Alter.

OrdinalStat(Datensatz$IntMath)
## Allgemeines Interesse an der Mathematik (von 1 := sehr gering bis 7 := sehr hoch)
OrdinalGraf(Datensatz$IntMath)
##Ich finde es sinnvoller, die Grafik nach der Kategorisierung von Variablen zu erstellen - lösche ich aber 
##das originale Code nicht, falls ihr andere Meinung dazu habt.
##Visuelle Darstellung nach der Kategorisierung von Interesse an Mathe
OrdinalGraf(func_e(Datensatz$IntMath,3))
##OrdinalGraf(func_e(Datensatz$IntMath,4))


OrdinalStat(Datensatz$IntProg)
# Allgemeines Interesse am Programmieren (von 1 := sehr gering bis 7 := sehr hoch)
OrdinalGraf(Datensatz$IntProg)
##Visuelle Darstellung nach der Kategorisierung von Interesse an Programmieren
OrdinalGraf(func_e(Datensatz$IntProg,3))
##OrdinalGraf(func_e(Datensatz$IntProg,4))

NominalStat(Datensatz$Studienfach)
# Aufteilung nach Studienfächern (deskriptiv).
NominalGraf1(Datensatz$Studienfach)
VisKat1(Datensatz$Studienfach,"Studienfach")
# Aufteilung nach Studienfächern (visuell).

#--------------------------------------------------------------------------

# Nun sollen Korrelationen/Zusammenhänge zwischen den einzelnen Variablen untersucht werden.

kategorial_bivariat(Datensatz$Studienfach, Datensatz$IntProg)
# Cramers Kontigenzkoeffizient beträgt 0.629, was für einen starken Zusammenhang 
# zwischen dem Studienfach und dem Interesse am Programmieren wiederspiegelt.

VisKat2(Datensatz$Studienfach, Datensatz$IntProg, "Studienfach", "Interesse am Programmieren")
# Visueller Zusammenhang Studienfach und Interesse am Programmieren.

kategorial_bivariat(Datensatz$Studienfach, Datensatz$IntMath)
# Der Zusammenhang zwischen dem Studienfach und dem Interesse an Mathe ist deutlich schwächer.

VisKat2(Datensatz$Studienfach, Datensatz$IntMath, "Studienfach", "Interesse an Mathematik")
# Visueller Zusammenhang Studienfach und Interesse an Mathe.



##Um pbcor an die Variable Mathe-LK anzuwenden, muss zuerst die originale Funktion leicht verändert werden.
##Sonst müssen wir eine weitere Hilfe-Funktion schreiben, um die Variable zu transformieren.
## pbcor <- function(dicho, metric) {
##   y0 <- which(dicho == "nein")
##   a0<- metric[y0]
##   y1 <- which(dicho == "ja")
##   a1<- metric[y1]
##   meana0 <- mean(a0)
##   meana1 <- mean(a1)
##   n0 <- length(a0)
##   n1 <- length(a1)
##   n <- n0 + n1
##   return(((meana1 - meana0) / (sd(metric)) * sqrt((n-1)/n))
##          * sqrt(n0*n1/(n^2)))
## }

pbcor(Datensatz$MatheLK,as.numeric(factor(Datensatz$Studienfach)))
## Ein schwacher Zusammenhang zwischen Mathe-LK und die Studienwahl ist zu beobachten.

pbcor(Datensatz$MatheLK,Datensatz$IntProg)
## Es ergibt sich eine negative Korrelation zwischen Mathe-LK und Interesse an Programmieren

##Zusammenhang zwischen Interesse an Mathe und Interesse an Programmieren
kategorial_bivariat(func_e(Datensatz$IntProg,3),func_e(Datensatz$IntMath,3))
##Schwacher Zusammenhang zwischen Interesse an Mathe und Programmieren ist zu beobachten.