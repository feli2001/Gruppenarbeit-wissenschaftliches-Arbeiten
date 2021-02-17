# Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
# Zusammengang zwischen einer metrischen und einer dichotomen Variablen
# berechnet und ausgibt.

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
