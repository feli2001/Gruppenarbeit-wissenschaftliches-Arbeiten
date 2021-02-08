#Gruppenarbeit Aufgabe 1
#zusammengefuehrtes Skript
setwd("/Users/sina.ruether/Documents/Uni")

ID <- 1:100
# Die ID der Studenten sind die Zahlen von 1 bis 100

Alter1<- rnorm(100, mean=25, sd=2)
#Mit Normalverteilung mit Erwartungswert 25 und Standardabweichung 2

#Hier waere es vllt sinnvoller die gerundeten Werte zu verwenden (niemand
#gibt sein Alter mit so vielen Nachkommastellen an):
Alter<- round(Alter1)
#Nach der Erstellung eines Issues sind wir zu dem Schluss gekommen,
#hier gerundete Werte zu verwenden.


Studienfach <- sample(c("Statistik","Data Science","Mathe","Informatik"), 100, replace = T, prob= c(0.35,0.35,0.1,0.2))
# Statistik und Data Science werden beide mit einer Wahrscheinlichkeit von 35% studiert. Mathe hat die geringste Wahrscheinlichkeit mit 10% und 
# Informatik liegt mit 20% dazwischen.
# Statistik und Data Science werden beide mit einer Wahrscheinlichkeit von 35 % studiert. Mathe hat die geringste Wahrscheinlichkeit mit 10 % und 
# Informatik liegt mit 20 % dazwischen.


# Variable: Interesse an Mathematik
# Es wird eine Funktion erstellt, die den Zusammenhang vom Studienfach zum Intersse fuer Mathematik 
# herstellt.
math_func <- function(x){
  math <- numeric(100)
  # Ein leerer Vektor der Laenge 100 wird erstellt.
  a <- 1
  for (i in x) {
    # Die Schleife soll spaeter den Vektor mit den Studienfaechern durchgehen.
    if(i == "Statistik"){
      math[a] <- sample(c(1,2,3,4,5,6,7), 1, 
                        prob = c(0.05,0.1,0.1,0.2,0.3,0.15,0.1))
      # Wenn die Leute das Studienfach "Statistik" haben, gibt der Vektor prob an, mit welcher 
      # W-keit, sie welche Zahl auf der Interssens-Skala waehlen.
      a <- a+1
    }
    if(i == "Data Science"){
      math[a] <- sample(c(1,2,3,4,5,6,7), 1, 
                        prob = c(0.05,0.1,0.2,0.3,0.15,0.15,0.05))
      # Wenn die Leute das Studienfach "Data Science" haben, gibt der Vektor prob an, mit welcher 
      # W-keit, sie welche Zahl auf der Interssens-Skala waehlen.
      a <- a+1
    }
    if(i == "Informatik"){
      math[a] <- sample(c(1,2,3,4,5,6,7), 1, 
                        prob = c(0.1,0.2,0.3,0.15,0.1,0.1,0.05))
      # Wenn die Leute das Studienfach "Informatik" haben, gibt der Vektor prob an, mit welcher 
      # W-keit, sie welche Zahl auf der Interssens-Skala waehlen.
      a <- a+1
    }
    if(i == "Mathe"){
      math[a] <- sample(c(1,2,3,4,5,6,7), 1, 
                        prob = c(0.05,0.1,0.1,0.1,0.15,0.3,0.2))
      # Wenn die Leute das Studienfach "Mathe" haben, gibt der Vektor prob an, mit welcher 
      # W-keit, sie welche Zahl auf der Interssens-Skala waehlen.
      a <- a+1
    }
    # Die Schleife fuellt so den Vektor math, der das Intersse an Mathematik angibt, nach und nach.
  }
  return(math)
  # Am Ende wird der Vektor zurueckgegeben.
}
InteresseMathe <- math_func(Studienfach)


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

#MatheLK Ja oder Nein:
MatheLK<- numeric(100)
MatheLK[which(Studienfach=="Statistik")]<-sample(c("ja","nein"), size= length(which(Studienfach=="Statistik")),replace=TRUE, prob=c(3/4, 1/4))
MatheLK[which(Studienfach=="Data Science")]<-sample(c("ja", "nein"), size= length(which(Studienfach =="Data Science")), replace =TRUE, prob=c(1/2, 1/2))
MatheLK[which(Studienfach=="Informatik")]<- sample(c("ja", "nein"), size=length(which(Studienfach =="Informatik")), replace=TRUE, prob= c(1/3, 2/3))
MatheLK[which(Studienfach=="Mathe")]<- sample(c("ja", "nein"), size=length(which(Studienfach=="Mathe")), replace =TRUE, prob =c(29/30, 1/30))
                                                                                          
# Statistik-Studenten hatten mit einer Wahrscheinlichkeit von 75% Mathe-LK, Data-Scientist-Studenten mit 65% Wahrscheinlichkeit.
# Mathematik-Studenten hatten mit einer Wahrscheinlichekti von 90% Mathe-LK und Informatik-Studenten nur mit einer Wahrscheinlichkeit von 50%. 
# Statistik-Studenten hatten mit einer Wahrscheinlichkeit von 75 % Mathe-LK, Data-Scientist-Studenten mit 65% Wahrscheinlichkeit.
# Mathematik-Studenten hatten mit einer Wahrscheinlichekti von 90 % Mathe-LK und Informatik-Studenten nur mit einer Wahrscheinlichkeit von 50 %.
                                                                                          
Datensatz<- data.frame(ID, Alter, Studienfach, IntMath=InteresseMathe, IntProg=InteresseProgrammieren, MatheLK)

write.csv(Datensatz, file="DatenGruppenarbeit.csv")

