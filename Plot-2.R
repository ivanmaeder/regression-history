getCoefficient = function(weightTable) {
  cor(x   = weightTable$day,
      y   = weightTable$weight,
      use = "complete.obs")
}

printPlotWithRegression = function(table) {
  relation = lm(table$weight ~ table$day)
  
  plot(x    = table$day,
       y    = table$weight,
       xlim = c(0, 70),
       ylim = c(65, 70))
  
  abline(relation, col="blue", lwd=2)
}

addPreviousRegression = function(table, rows) {
  r = rows
  i = 5
  
  while (r > 2 && i > 0) {
    subset = head(table, r - 1)
    relation = lm(subset$weight ~ subset$day)
    abline(relation, col=rgb(0, 0, 0, i * 0.1))
    
    r = r - 1
    i = i - 1
  }
}

weightTable = read.table('Data.csv', header=T, sep=',')

for (rows in c(2:70)) {
  subset = head(weightTable, rows)

  printPlotWithRegression(subset)
  addPreviousRegression(subset, rows)

  Sys.sleep(0.2)
}
