#Aufgabenteil (F) 
#Funktionen, zur Visualisierung kategorieller Variablen.
#Speziell dreier oder vierer kategorieller Variablen.

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




