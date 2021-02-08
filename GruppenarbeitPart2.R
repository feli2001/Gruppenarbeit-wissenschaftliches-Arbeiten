#Gruppenarbeit wiss.Arbeiten
#Zweiter Teil der Aufgabe:

Alter<- rnorm(100, mean=25, sd=2)
#Mit Normalverteilung mit Erwartungswert 25 und Standardabweichung 2

#Hier waere es vllt sinnvoller die gerundeten Werte zu verwenden (niemand
#gibt sein Alter mit so vielen Nachkommastellen an):
Alter2<- round(Alter)

#Interesse Programmieren abhaengig von Studienfach:
InteresseProgrammieren<- numeric(100)
#leeren Vektor der Laenge 100 erstellen
InteresseProgrammieren[which(Studienfach=="Statistik")]<-sample(c(1:5), size= length(which(Studienfach=="Statistik")),replace=TRUE, prob=c(1/6, 1/5, 1/5, 1/12, 1/20))
#Statistiker haben etwas weniger Interesse an Programmieren -> nur Zahlen von 1 bis 5 werden verwendet
#Mit sample laesst sich jede Zahl mit bestimmter WSK ausgeben: prob=c(...)
#MIT Zuruecklegen weil wir Werte mehrfach belegen koennen
#Auch nicht gar kein Interesse: Zahlen 2 und 3 haben hoechste WSK

InteresseProgrammieren[which(Studienfach=="Data Science")]<-sample(c(3:7), size= length(which(Studienfach =="Data Science")), replace =TRUE, prob=c(1/6, 1/3, 1/3, 1/20, 1/40))
#Data Science Studierende muessen mit Java programmieren koennen, also ist das Interesse vermutlich etwas hoeher als bei den Statistikern

InteresseProgrammieren[which(Studienfach=="Informatik")]<- sample(c(6:7), size=length(which(Studienfach =="Informatik")), replace=TRUE, prob= c(1/2, 1/2))
#Informatiker haben vermutlich sehr viel Interesse an Programmieren

InteresseProgrammieren[which(Studienfach=="Mathe")]<- sample(c(1:3), size=length(which(Studienfach=="Mathe")), replace =TRUE, prob =c(1/3, 1/3, 1/3))
#Mathematiker haben eher weniger Interesse an Programmieren deshalb nur Zahlen von 1 bis 3 verwendet




