weightTable = read.table('Data.csv', header=T, sep=',')

coefficient = cor(x   = weightTable$day,
                  y   = weightTable$weight,
                  use = "complete.obs")
print(coefficient)

for (rows in c(2:70)) {
  subset = head(weightTable, rows)

  subsetRelation = lm(subset$weight ~ subset$day)

  plot(x    = subset$day,
       y    = subset$weight,
       xlim = c(0, 70),
       ylim = c(65, 70))

  abline(subsetRelation)

  Sys.sleep(0.2)
}