#Teil 3-e
# Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert kategorisiert

#Ueberpruefen, ob die eingegebene Variable quantilbasiert kategorisiert werden kann.
transformieren <- function(x)
{
  if (is.numeric(x)) return(x)
  else
  {
    if(is.factor(x)) 
    {
      x <- as.numeric(x)
      return(x)
    }
    else stop("Diese Variable kann nicht quantilbasiert kategorisiert werden")
  }
}

#Kategorisierung der Variable
kategorisieren_3 <- function(x)
{
  qtil <- quantile(x,  probs = c(1/3, 2/3))
  vec <- x
  for(i in c(1:length(vec))) 
  {
    if(vec[i] >= qtil[2]) vec[i] <- "Hoch"
    else
    {
      if(vec[i] >= qtil[1]) vec[i] <- "Mittel"
      else vec[i] <- "Niedrig"
    }
  }
  vec <- as.factor(vec)
  levels(vec) <- c("Niedrig", "Mittel", "Hoch")
  return(vec)
}

kategorisieren_4 <- function(x)
{
  qtil <- quantile(x,  probs = c(1/4, 2/4, 3/4))
  vec <- x
  for(i in c(1:length(vec))) 
  {
    if(vec[i] >= qtil[3]) vec[i] <- "Hoch"
    else
    {
      if(vec[i] >= qtil[2]) vec[i] <- "Eher hoch"
      else 
      {
        if(vec[i] >= qtil[1]) vec[i] <- "Eher niedrig"
        else vec[i] <- "Niedrig"
      }
    }
  }
  vec <- as.factor(vec)
  levels(vec) <- c("Niedrig", "Eher niedrig", "Eher hoch","Hoch")
  return(vec)
}

#Die main-Funktion zu benutzen, x ist die Variablenvektor und num ist die Anzahl der Kategorien
func_e <- function(x, num)
{
  if(num ==3) return(kategorisieren_3(transformieren(x)))
  else {
    if(num == 4) return(kategorisieren_4(transformieren(x)))
    else stop("Diese Kategorisierung ist nicht verfuegbar")
  }
}


#Kategorisierung von numetrichem Vektor
x <- sample(c(1:10), 100, replace=TRUE)

func_e(x,3)
func_e(x,4)
#Kategorisierung von Rangvektor
y <- factor(sample(c(1:15), 100, replace=TRUE))

func_e(y,3)
func_e(y,4)

#Kategorisierung von ordinal-skaliert Vektor
z <- factor(sample(c("sehr gering","gering","eher gering","normal","eher hoch","hoch","sehr hoch"), 100, replace=TRUE),
            levels = c("sehr gering","gering","eher gering","normal","eher hoch","hoch","sehr hoch"))

func_e(z,3)
func_e(z,4)