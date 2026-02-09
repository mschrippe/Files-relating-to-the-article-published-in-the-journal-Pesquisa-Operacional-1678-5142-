if (!require("tseries")) install.packages("tseries")
library(tseries)

# --- 2. IMPORTAR DADOS ---
HW <- read.csv(file.choose(), sep = ",", skip = 1, header = TRUE)

# --- 3. TRANSFORMAR EM SÉRIE TEMPORAL ---
# Ajuste conforme o início dos seus dados (Ex: 2015, mês 1)
ts_data <- ts(HW[,2], start = c(2015, 1), frequency = 12)

# --- 4. TESTE KPSS ---
kpss_test <- kpss.test(ts_data)

# --- 5. EXIBIÇÃO DOS RESULTADOS ---
cat("\n==============================================\n")
cat("      RESULTADOS DO TESTE KPSS (ESTACIONARIEDADE)\n")
cat("==============================================\n")
print(kpss_test)

cat("\n--- Interpretação Detalhada ---\n")
cat("KPSS Level =", kpss_test$statistic, "\n")
cat("p-value =", kpss_test$p.value, "\n")
cat("\nValores Críticos de Referência:\n")
cat("10%: 0.347 | 5%: 0.463 | 2.5%: 0.574 | 1%: 0.739\n\n")

# Conclusão baseada nos níveis de significância
if (kpss_test$p.value < 0.01) {
  cat("Conclusão: A série NÃO é estacionária ao nível de significância de 1%\n")
} else if (kpss_test$p.value < 0.025) {
  cat("Conclusão: A série NÃO é estacionária ao nível de significância de 2.5%\n")
} else if (kpss_test$p.value < 0.05) {
  cat("Conclusão: A série NÃO é estacionária ao nível de significância de 5%\n")
} else if (kpss_test$p.value < 0.1) {
  cat("Conclusão: A série NÃO é estacionária ao nível de significância de 10%\n")
} else {
  cat("Conclusão: A série é ESTACIONÁRIA (p-value >= 0.1)\n")
}
cat("==============================================\n")