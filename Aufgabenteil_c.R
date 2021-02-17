kategorial_bivariat <- function(x,y)
{
  # Haeufigkeitstabelle
  a <- table(x, y)
  Gesamt <- margin.table(table(x, y),1)
  d <- margin.table(table(x, y),2)
  tabelle <- rbind(cbind(a,Gesamt),c(d,sum(d)))
  row.names(tabelle) <- c(row.names(a),"Gesamt")
  #######################################################################
  # Funktion aus dem Skript 2 ausfuehren
  eij <- Erwartungshaeufigkeiten(x,y)
  #######################################################################
  # Cramers Kontingenzkoeffizient
  kc <- sqrt(as.numeric(suppressWarnings(chisq.test(x,y)$statistic))/(tabelle[max(row(tabelle)),
                                                                              max(col(tabelle))]*(min(c(max(row(tabelle)),max(col(tabelle))))-1)))
  #######################################################################
  # Pearsons Kontingenzindex und korrigierter Pearson Index
  kp <- sqrt(as.numeric(suppressWarnings(chisq.test(x,y)$statistic))/(as.numeric(suppressWarnings(chisq.test(x,y)$statistic))
                                                                      +tabelle[max(row(tabelle)),max(col(tabelle))]))
  kpcorr <- sqrt(min(c(max(row(tabelle)),max(col(tabelle))))/(min(c(max(row(tabelle)),
                                                                    max(col(tabelle))))-1)*kp)
  #######################################################################
  # Korrelationskoeffizient und Kovarianz
  if(is.numeric(x) && is.numeric(y))
  {
    cor <- c(cor(x,y,method = "spearman"),cor(x,y,method = "kendall"))
    cov <- c(cov(x,y,method = "spearman"),cor(x,y,method = "kendall"))
  }
  else
  {
    cor <- "x oder y nicht numerisch: nicht berechenbar"
    cov <- "x oder y nicht numerisch: nicht berechenbar"
  }
  #######################################################################
  #######################################################################
  # Ausgabe
  list("Kontingenztafel" = tabelle,"Erwartungshäufigkeiten" = as.table(eij),"Cramers Kontingenzkoeffizient" = kc,"Pearsons Kontingenzindex" = kp,
       "korrigierter Pearson Index" = kpcorr,"Korrelationskoeffizient (nach Spearman/Kendall)" = cor,"Kovarianz (nach Spearman/Kendall)" = cov)
}


