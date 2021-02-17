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




