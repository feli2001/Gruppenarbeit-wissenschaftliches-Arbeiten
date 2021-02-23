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
# Allgemeines Interesse an der Mathematik (von 1 := sehr gering bis 7 := sehr hoch)
OrdinalGraf(Datensatz$IntMath)

OrdinalStat(Datensatz$IntProg)
# Allgemeines Interesse am Programmieren (von 1 := sehr gering bis 7 := sehr hoch)
OrdinalGraf(Datensatz$IntProg)

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


