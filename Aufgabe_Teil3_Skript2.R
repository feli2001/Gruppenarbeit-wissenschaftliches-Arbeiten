## Funktion fuer arithmetisches Mittel (wird in Teil a verwendet):
ar_mittel <- function(x){
  if(!(is.numeric(x))){stop("x muss ein numerischer Vektor sein!")}
  # Zunaechst wird getestet, ob der eingegebene Parameter x wirklich ein 
  # numerischer Vektor ist. Sonst koennte man das arithmetische Mittel nicht 
  # berechnen.
  N <- length(x)
  # Als N wird die Anzahl der Objekte in x, also die Anzahl Beobachtungen 
  # definiert.
  a <- 1/N * sum(x)
  # Nun wird das arithmetische Mittel (hier mit a bezeichnet) mit der bekannten
  # Formel (bekannt aus "deskriptive Statistik") berechnet.
  return(a)
  # Schliesslich gibt die Funktion das arithmetische Mittel aus.
}


## Funktion fuer das Maximum und das Minumum (wird in Teil a verwendet):
max_min <- function(x){
  if(!(is.numeric(x))){stop("x muss ein numerischer Vektor sein!")}
  # Zunaechst wird getestet, ob der eingegebene Parameter x wirklich ein 
  # numerischer Vektor ist. Sonst koennte man das Minumum und Maximum nicht 
  # berechnen.
  s <- sort(x)
  # Der Vektor s wird definiert, also sortierter Vektor x. Hierbei wird 
  # automatisch von der kleinsten zur groessten Zahl sortiert.
  N <- length(x)
  # Als N wird die Anzahl der Objekte in x, also die Anzahl Beobachtungen 
  # definiert.
  min <- s[1]
  max <- s[N]
  # Da s ja der sortierte Vekor x ist, ist nun das erste Objekt das kleinste, 
  # also das Minumun, und das letzte Objekt das groesste, als das Maximum.
  # Das letze Objekt steht an der Stelle N, das der Vektor N objekte hat.
  return(c(min,max))
  # Zum Schluss werden Minimum und Maximum, als Vektor verbunden, zurueckgegeben
}


## Funktion fuer die Erwartungshaeufigkeiten (wird in Teil c verwendet):
Erwartungshaeufigkeiten <- function(x,y)
{
  # Haeufigkeitstabelle
  a <- table(x, y)
  Gesamt <- margin.table(table(x, y),1)
  d <- margin.table(table(x, y),2)
  tabelle <- rbind(cbind(a,Gesamt),c(d,sum(d)))
  row.names(tabelle) <- c(row.names(a),"Gesamt")
  #####################################################################################
  # Erwartungshaeufigkeiten
  z <- NULL
  j <- 1
  while(j < max(col(tabelle)))
  {
    i <- 1
    while(i < max(row(tabelle)))
    {
      z <- c(z, tabelle[max(row(tabelle)),j]*tabelle[i,max(col(tabelle))]/tabelle[max(row(tabelle)),max(col(tabelle))])
      i <- i+1
    }
    j <- j+1
  }
  t1 <- matrix(z, ncol = max(col(tabelle))-1)
  t2 <- cbind(t1, Gesamt)
  eij <- rbind(t2, c(d,sum(d)))
  row.names(eij) <- dimnames(tabelle)[[1]]
  colnames(eij) <- dimnames(tabelle)[[2]]
  eij
}











