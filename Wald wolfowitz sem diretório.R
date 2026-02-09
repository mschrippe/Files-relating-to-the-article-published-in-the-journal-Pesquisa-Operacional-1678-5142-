if (!require("randtests")) install.packages("randtests")
library(randtests)

HW <- read.csv(file.choose(), sep = ",", skip = 1, header = TRUE)

ts_data <- ts(HW[,2], start = c(2015, 1), frequency = 12)

wald_wolfowitz_test <- runs.test(ts_data)

print(wald_wolfowitz_test)

cat("Estatística do teste =", wald_wolfowitz_test$statistic, "\n")
cat("p-value =", wald_wolfowitz_test$p.value, "\n")
cat("Hipótese nula: Os dados são independentes\n")

if (wald_wolfowitz_test$p.value < 0.05) {
  cat("Conclusão: Rejeitamos a hipótese nula. Os dados não são independentes ao nível de significância de 5%\n")
} else {
  cat("Conclusão: Não rejeitamos a hipótese nula. Não há evidência suficiente para concluir que os dados não são independentes ao nível de significância de 5%\n")
}