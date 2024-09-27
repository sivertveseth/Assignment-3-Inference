# Laster inn nødvendige pakker
library(tidyverse)
library(gridExtra)


set.seed(1) # Sikrer at alle dei tilfeldige tallene som generes er dei sammen hver gang koden kjøres
population <- rnorm(1000000, mean = 1.5, sd = 3) # Generer 1 million tilfeldige verdier som er normalfordelt

# Sample 1
samp1 <- data.frame(y = sample(population, 8, replace = FALSE))

# Sample 2
samp2 <- data.frame(y = sample(population, 40, replace = FALSE))

# Lager lineære modeller for hver sample med kun en konstant
m1 <- lm(y ~ 1, data = samp1)
m2 <- lm(y ~ 1, data = samp2)

# Oppsummering av modellene 
summary(m1)
summary(m2)

------------------------------------------------
  
# Laster inn nødvendige pakker
library(ggplot2)
library(gridExtra)

# Definer t-verdier for begge utvalg
t_value1 <- 1.47  # t-verdi for samp1
t_value2 <- 3.953  # t-verdi for samp2
df1 <- 7   # Antall frihetsgrader for samp1 (n-1, her 8-1=7)
df2 <- 39  # Antall frihetsgrader for samp2 (n-1, her 40-1=39)

# Lag dataramme for x-verdier
x_vals <- seq(-5, 5, length.out = 1000)

# Lag plot for samp1
plot1 <- ggplot(data.frame(x = x_vals), aes(x)) +
  # Fyll venstre hale for samp1
  stat_function(fun = dt, args = list(df = df1), xlim = c(-5, -t_value1), geom = "area", fill = "gray", alpha = 0.5) +
  # Fyll høyre hale for samp1
  stat_function(fun = dt, args = list(df = df1), xlim = c(t_value1, 5), geom = "area", fill = "gray", alpha = 0.5) +
  # Tegn hele t-fordelingskurven for samp1
  stat_function(fun = dt, args = list(df = df1)) +
  # Legg til den observerte t-verdien som en vertikal linje
  geom_vline(xintercept = t_value1, color = "red", linetype = "dashed") +
  # Annoter t-verdien for samp1
  annotate("text", x = t_value1 + 0.3, y = 0.2, label = "Observed t (samp1)", angle = -90, size = 2.8) +
  labs(title = "t-distribution for samp1", x = "t", y = "Density f(t)") +
  theme_classic() +
  xlim(-5, 5)

# Print plot1
print(plot1)

-----------------------------------------------------------

  # Lager data frames for å lagre modell estimater
  results_8 <- data.frame(estimate = rep(NA, 1000), 
                          se = rep(NA, 1000), 
                          pval = rep(NA, 1000), 
                          n = 8)  

results_40 <- data.frame(estimate = rep(NA, 1000), 
                         se = rep(NA, 1000), 
                         pval = rep(NA, 1000), 
                         n = 40)

# En løkke som for å hente ut 1000 prøver(samples), hver iterasjon (i) vil 
# hente ut en ny prøve fra populasjonen

for(i in 1:1000) {
  
  # Henter ut en prøve 
  samp1 <- data.frame(y = sample(population, 8, replace = FALSE))
  samp2 <- data.frame(y = sample(population, 40, replace = FALSE))
  
  # Lager en lineær modell
  m1 <- lm(y ~ 1, data = samp1)
  m2 <- lm(y ~ 1, data = samp2)
  
  # Henter ut verdier fra modellen
  results_8[i, 1] <- coef(summary(m1))[1, 1]
  results_8[i, 2] <- coef(summary(m1))[1, 2]
  results_8[i, 3] <- coef(summary(m1))[1, 4]
  
  results_40[i, 1] <- coef(summary(m2))[1, 1]
  results_40[i, 2] <- coef(summary(m2))[1, 2]
  results_40[i, 3] <- coef(summary(m2))[1, 4]
  
  
}

# Lagrer resultatene i en data frame

results <- bind_rows(results_8, results_40)

------------------------------------------------

  library(knitr)

# Filtrer ut resultatene for n = 8 og n = 40

results_n8 <- results %>% filter(n == 8) 
results_n40 <- results %>% filter(n == 40)

# Beregn standardavvik for estimate og gjennomsnitt for SE

summary_table <- data.frame( 
  "Sample Size" = c(8, 40), 
  "Standard Deviation of Estimate" = c(sd(results_n8$estimate), sd(results_n40$estimate)), 
  "Average SE" = c(mean(results_n8$se), mean(results_n40$se)))

# Print tabellen med kable

kable(summary_table, caption = "Summary Statistics for Estimate and SE by Sample Size")

----------------------------------------------

  results %>%
  ggplot(aes(pval)) + 
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "white") +  # Juster binwidth hvis nødvendig
  facet_wrap(~ n) +  # Lager ett histogram for n = 8 og ett for n = 40
  labs(title = "Histogram of p-values for Sample Sizes 8 and 40",
       x = "P-values",
       y = "Frequency") +
  theme_minimal()

-----------------------------------------------

  library(kableExtra)
library(dplyr)

# Beregn andelen signifikante resultater
sig_table <- results %>%
  filter(pval < 0.05) %>%
  group_by(n) %>%
  summarise(`α` = n()/1000)

# Lag en tabell med kableExtra
kable(sig_table, digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

---------------------------------------------------

  # Laster inn pwr og knitr pakken
  library(pwr)
library(knitr)

# Beregn statistisk styrke for n = 8 og n = 40
power_n8 <- pwr.t.test(n = 8, sig.level = 0.05, d = 1.5/3, type = "one.sample")
power_n40 <- pwr.t.test(n = 40, sig.level = 0.05, d = 1.5/3, type = "one.sample")

# Lag en tabell med resultatene
power_results <- data.frame(
  "Sample Size" = c(8, 40),
  "Power" = c(power_n8$power, power_n40$power)
)

# Print tabellen pent med kable
kable(power_results)
--------------------------------------------------
  
  population <- rnorm(1000000, mean = 0, sd = 3)


# Create data frames to store the model estimates
results_8 <- data.frame(estimate = rep(NA, 1000), 
                        se = rep(NA, 1000), 
                        pval = rep(NA, 1000), 
                        n = 8)  

results_40 <- data.frame(estimate = rep(NA, 1000), 
                         se = rep(NA, 1000), 
                         pval = rep(NA, 1000), 
                         n = 40)

# A for loop used to sample 1000 studies, each iteration (i) will draw a new sample
# from the population. 

for(i in 1:1000) {
  
  # Draw a sample 
  samp1 <- data.frame(y = sample(population, 8, replace = FALSE))
  samp2 <- data.frame(y = sample(population, 40, replace = FALSE))
  
  # Model the data
  m1 <- lm(y ~ 1, data = samp1)
  m2 <- lm(y ~ 1, data = samp2)
  
  # Extract values from the models
  results_8[i, 1] <- coef(summary(m1))[1, 1]
  results_8[i, 2] <- coef(summary(m1))[1, 2]
  results_8[i, 3] <- coef(summary(m1))[1, 4]
  
  results_40[i, 1] <- coef(summary(m2))[1, 1]
  results_40[i, 2] <- coef(summary(m2))[1, 2]
  results_40[i, 3] <- coef(summary(m2))[1, 4]
  
  
}


# Save the results in a combined data frame

results_null <- bind_rows(results_8, results_40)

-------------------------------------------------
  
  
  library(ggplot2)

# Lag histogrammer av p-verdiene med fasetter for hver utvalgsstørrelse
results_null %>%
  ggplot(aes(pval)) + 
  geom_histogram(binwidth = 0.05, fill = "blue", color = "white") + 
  facet_wrap(~ n, scales = "free_y") +
  labs(title = "Histogram of P-values for Population with Mean Effect of Zero",
       x = "P-values",
       y = "Frequency") +
  theme_minimal()

--------------------------------------------------
  
  library(knitr)

# Beregn totalt antall tester og falske positive
total_tests <- 1000  # Antall tester
false_positives <- results_null %>%
  filter(pval < 0.05) %>%
  group_by(n) %>%
  summarise("Antall falske positive" = n(), 
            "Falske positive (%)" = (n() / total_tests) * 100)

# Legg til totalt antall tester for å beregne ikke-signifikante resultater
false_positives <- false_positives %>%
  mutate("Antall ikke-signifikante" = total_tests - `Antall falske positive`)

# Lag en tabell med kable og norske kolonnenavn
kable(false_positives, caption = "Resultater av falske positive og ikke-signifikante tester per utvalgsstørrelse", digits = 2)