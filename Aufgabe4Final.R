# Aufgabe 4 - Auswertung der Daten

#Datensatz <- read.csv("DatenGruppenarbeit.csv")

# Zunaechst geht es darum, sich einen groben Ueberblick ueber die Daten zu verschaffen:

meth_func(Datensatz$Alter)
# Alle relevanten deskriptiven Statistiken zu der Variable Alter.

VisKat1(Datensatz$Alter,"Alter")
# Visuelle Darstellung der Variable Alter.
# Es lässt sich vermuten, dass es sich um eine normalverteilte Variable handelt.

NominalStat(Datensatz$MatheLK)
# 68% der Personen besuchten den Mathe-LK.

NominalGraf1(Datensatz$MatheLK)
# Visuelle Darstellung der Variable Alter.

OrdinalStat(Datensatz$IntMath)
# Allgemeines Interesse an der Mathematik (von 1 := sehr gering bis 7 := sehr hoch)
OrdinalGraf(Datensatz$IntMath)
# Genaue graphische Darstellung.
OrdinalGraf(func_e(Datensatz$IntMath,3))
# Kategorisierte Darstellung für bessere Übersicht.

OrdinalStat(Datensatz$IntProg)
# Allgemeines Interesse am Programmieren (von 1 := sehr gering bis 7 := sehr hoch)
OrdinalGraf(Datensatz$IntProg)
# Genaue graphische Darstellung.
OrdinalGraf(func_e(Datensatz$IntProg,3))
# Kategorisierte Darstellung für bessere Übersicht.

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
# Zu erkennen ist hier, dass Informatiker großes Interesse am Programmieren
# mit in ihr Studium nehmen, wobei sich Statistiker und Mathematiker eher
# weniger für das Programmieren interessieren.

kategorial_bivariat(Datensatz$Studienfach, Datensatz$IntMath)
# Der Zusammenhang zwischen dem Studienfach und dem Interesse an Mathe ist deutlich schwächer.
# Siehe bspw. Cramers Kontigenzkoeffizienten, welcher ca. 0,257 beträgt.

VisKat2(Datensatz$Studienfach, Datensatz$IntMath, "Studienfach", "Interesse an Mathematik")
# Visueller Zusammenhang Studienfach und Interesse an Mathe.

kategorial_bivariat(Datensatz$Studienfach, Datensatz$MatheLK)
# Zusammenhang zwischen Studienfach und der Variable MatheLK.
# Nach Cramers Kontingenzkoeffizienten handelt es sich hier um einen eher schwachen Zusammenhang.
# Auch mit einer abgewandelten Form der Funktion pbcor schließen wir auf einen schwachen Zusammenhang:

 pbcor <- function(dicho, metric) {
   y0 <- which(dicho == "nein")
   a0<- metric[y0]
   y1 <- which(dicho == "ja")
   a1<- metric[y1]
   meana0 <- mean(a0)
   meana1 <- mean(a1)
   n0 <- length(a0)
   n1 <- length(a1)
   n <- n0 + n1
  return(((meana1 - meana0) / (sd(metric)) * sqrt((n-1)/n))
          * sqrt(n0*n1/(n^2)))
}

pbcor(Datensatz$MatheLK,as.numeric(factor(Datensatz$Studienfach)))
# 0.25987

VisKat2(Datensatz$Studienfach,Datensatz$MatheLK,"Studienfach","Mathe LK")
# Hier sehen wir allerdings, dass insbesondere die Mathematiker und die
# Statistiker den Mathe LK besuchten. Die Data Science Studierenden und vor allem
# die Informatiker haben seltener den Mathe LK gewählt. Dies könnte der Grund
# für den relativ geringen Kontingenzkoeffizienten sein.

kategorial_bivariat(func_e(Datensatz$IntProg,3),func_e(Datensatz$IntMath,3))
# Des Weiteren ist ein schwacher Zusammenhang zwischen den Variablen
# IntMath und IntProg zu beobachten.

# Zusammenhänge zwischen MatheLK und IntMath bzw. IntProg im Folgenden:

pbcor(Datensatz$MatheLK, Datensatz$IntMath) 
# 0.08894461 --> Kaum ein linearer Zusammenhang zwischen Mathe LK und dem
# Interesse für Mathe. Komisch, aber vielleicht besteht da eine andere Art von
# Zusammenhang?

VisKat2(Datensatz$IntMath, Datensatz$MatheLK, "Interesse an Mathematik", "Mathe LK" )
# Die Zahl der Studenten mit MatheLK ist dort am höchsten,
# wo IntMath durchschnittlich 3-5 ist. 
# Bei geringen ode rhohem Interesse an der Mathematik, ist die Zahl der Menschen,
# die den MatheLK besuchten deutlich geringer.

pbcor(Datensatz$MatheLK, Datensatz$IntProg)  
# -0.2804655 --> Ein leichter negativer linearer Zusammenhang zwischen Mathe LK
# und dem Interesse am Programmieren.

VisKat2(Datensatz$IntProg, Datensatz$MatheLK, "Interesse am Programmieren", "Mathe LK" )
# Sehr geringes Interesse an Programmieren: relativ viele, die Mathe LK hatten.
# Hohes Interesse an Programmieren (6): relativ viele, die kein Mathe LK hatten.
# Sehr hohes Interesse an Programmieren (7): viele, die Mathe LK hatten.
# --> Vermutlich leichter negativer linearer Zusammenhang dadurch zu erklären, 
# dass die Studierenden mit wenig Interesse am Programmieren zu einem sehr hohen
# Anteil kein Mathe LK hatten.
# Zudem  gibt es relativ wenige mit einem hohen Interesse, die Mathe LK gewählt
# hatten. 
# Dies wird allerdings durch einige Punkte wieder relativiert, wie z.B. der
# hohen Anzahl an denen, die Mathe LK gewählt hatten und dennoch ein sehr
# hohes Interesse an Programmieren hatten.
