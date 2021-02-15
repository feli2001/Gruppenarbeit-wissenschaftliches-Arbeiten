## (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken fuer
##     metrische Variablen berechnet und ausgibt:

# Deskriptive Statistiken fuer metrische Variablen
# - arithmetisches Mittel
# - Median
# - Standardabweichung bzw. Varianz
# - Minmum, Maximum
# - Quantile

## Vorarbeit: Funktionen, die in der Hauptfunktion angewendet werden:
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




