#Aufgabe 4 - Auswertung der Daten
#Datensatz <- read.csv("DatenGruppenarbeit.csv")

#Im Folgenden wird der Datensatz ausgewertet.
# Diese Version ist nicht die fertige, sondern nur der Anfang der Aufgabe 4. 

# Zun?chst geht es darum, sich einen groben ?berblick ?ber die Daten zu verschaffen:

meth_func(Datensatz$Alter)
# Alle relevanten deskriptiven Statistiken zu der Variable Alter.

NominalStat(Datensatz$MatheLK)
# 68% der Personen besuchten den Mathe-LK.

NominalGraf1(Datensatz$MatheLK)
# Visuelle Darstellung der Variable Alter.

OrdinalStat(Datensatz$IntMath)
# Allgemeines Interesse an der Mathematik (von 1 := sehr gering bis 7 := sehr hoch)
OrdinalGraf(Datensatz$IntMath)

OrdinalStat(Datensatz$IntProg)
# Allgemeines Interesse am Programmieren (von 1 := sehr gering bis 7 := sehr hoch)
OrdinalGraf(Datensatz$IntProg)

NominalStat(Datensatz$Studienfach)
# Aufteilung nach Studienf?chern (deskriptiv).
NominalGraf1(Datensatz$Studienfach)
VisKat1(Datensatz$Studienfach,"Studienfach")
# Aufteilung nach Studienf?chern (visuell).
VisKat1(Datensatz$Alter,"Alter")
# Visuelle Aufteilung nach dem Alter. Hier sieht man auch schön die
# Normalverteilung.

#--------------------------------------------------------------------------

# Nun sollen Korrelationen/Zusammenh?nge zwischen den einzelnen Variablen untersucht werden.

### Zusammenhänge zwischen dem Studienfach und IntProg, IntMath, MatheLK: ### 
kategorial_bivariat(Datensatz$Studienfach, Datensatz$IntProg)
# Cramers Kontigenzkoeffizient betr?gt 0.629, was f?r einen starken Zusammenhang 
# zwischen dem Studienfach und dem Interesse am Programmieren wiederspiegelt.

VisKat2(Datensatz$Studienfach, Datensatz$IntProg, "Studienfach", "Interesse am Programmieren")
# Visueller Zusammenhang Studienfach und Interesse am Programmieren.

kategorial_bivariat(Datensatz$Studienfach, Datensatz$IntMath)
# Der Zusammenhang zwischen dem Studienfach und dem Interesse an Mathe ist deutlich schw?cher.

VisKat2(Datensatz$Studienfach, Datensatz$IntMath, "Studienfach", "Interesse an Mathematik")
# Visueller Zusammenhang Studienfach und Interesse an Mathe.

kategorial_bivariat(Datensatz$Studienfach, Datensatz$MatheLK)
# Cramers Kontingenzkoeffizient: 0.2546709 --> geringer Zusammenhang zw. dem 
# Studiengang und MatheLK
# Pearsons Kontingenzindex: 0.3388519
# korrigierter Pearson Index: 0.7129361

VisKat2(Datensatz$Studienfach,Datensatz$MatheLK,"Studienfach","Mathe LK")
# Hier sehen wir allerdings, dass insbesondere die Mathematiker sowie die
# Statistiker Mathe LK hatten. Die Data Science Studierenden und vor allem
# die Informatiker haben seltener Mathe LK gewählt. Dies könnte der Grund
# für den relativ geringen Kontingenzkoeffizient. 


### Zusammenhänge zwischen MatheLK und IntMath bzw. IntProg ###
pbcor(Datensatz$MatheLK, Datensatz$IntMath) 
# 0.08894461 --> Kaum ein linearer Zusammenhang zwischen Mathe LK und dem
# Interesse für Mathe. Komisch, aber vielleicht besteht da eine andere Art von
# Zusammenhang?

VisKat2(Datensatz$IntMath, Datensatz$MatheLK, "Interesse an Mathematik", "Mathe LK" )
# Studierende mit einem sehr hohen Interesse an Mathematik haben auch Mathe LK
# gewählt. Komischerweise auch die, die ein sehr geringes Interesse am Mathe LK
# hatten. Daher vermutlich auch die geringe Korrelation.

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



