## Variable: ID
ID <- 1:100
# Die ID der Studenten sind die Zahlen von 1 bis 100

## Variable: Interesse an Mathematik
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
int_math <- math_func(Fach)
# Die Variable wird zugewiesen und gibt dann das Intersse an Mathematik an.


