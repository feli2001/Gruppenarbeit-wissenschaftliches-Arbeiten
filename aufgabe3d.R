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


pbcor = function(dichotomic_variable, metric_variable, data) {
  y0 = data[data$dichotomic_variable == "0", "metric_variable"]
  y1 = data[data$dichotomic_variable == "1", "metric_variable"]
  meany0 = mean(y0)
  meany1 = mean(y1)
  n0 = length(y0)
  n1 = length(y1)
  n = n0 + n1
  return(((meany1 - meany0) / (sd(data$metric_variable) * sqrt((n-1)/n)))
         * sqrt(n0*n1/n^2))
}

# Beim Anwenden der Funktion wird folgende Fehlermeldung ausgeworfen:
# Warnmeldungen:
# 1: In mean.default(y0) : argument is not numeric or logical: returning NA
# 2: In mean.default(y1) : argument is not numeric or logical: returning NA
# 3: In sqrt((n - 1)/n) : NaNs wurden erzeugt
