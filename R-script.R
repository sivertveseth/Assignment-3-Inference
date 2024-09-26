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

# Lag plot for samp2
plot2 <- ggplot(data.frame(x = x_vals), aes(x)) +
  # Fyll venstre hale for samp2
  stat_function(fun = dt, args = list(df = df2), xlim = c(-5, -t_value2), geom = "area", fill = "lightblue", alpha = 0.5) +
  # Fyll høyre hale for samp2
  stat_function(fun = dt, args = list(df = df2), xlim = c(t_value2, 5), geom = "area", fill = "lightblue", alpha = 0.5) +
  # Tegn hele t-fordelingskurven for samp2
  stat_function(fun = dt, args = list(df = df2)) +
  # Legg til den observerte t-verdien som en vertikal linje
  geom_vline(xintercept = t_value2, color = "red", linetype = "dashed") +
  # Annoter t-verdien for samp2
  annotate("text", x = t_value2 + 0.3, y = 0.2, label = "Observed t (samp2)", angle = -90, size = 2.8) +
  labs(title = "t-distribution for samp2", x = "t", y = "Density f(t)") +
  theme_classic() +
  xlim(-6, 6)

# Print plot2
print(plot2)

# Kombiner plottene ved siden av hverandre
combined_plot <- grid.arrange(plot1, plot2, ncol = 2)

# Vis det kombinerte plottet
print(combined_plot)

-----------------------------------------------------------

# Laster inn nødvendige pakker
library(tidyverse)


set.seed(1) # Sikrer at alle dei tilfeldige tallene som generes er dei sammen hver gang koden kjøres
population <- rnorm(1000000, mean = 1.5, sd = 3) # Generer 1 million tilfeldige verdier som er normalfordelt

# Sample 1
samp1 <- data.frame(y = sample(population, 8, replace = FALSE))

# Sample 2
samp2 <- data.frame(y = sample(population, 40, replace = FALSE))

# Lager lineære modeller for hver sample med kun en konstant
m1 <- lm(y ~ 1, data = samp1)
m2 <- lm(y ~ 1, data = samp2)

# Oppsummering av modellen m1
summary(m1)

# Definer t-verdi 
t_value <- 1.47
df <- 7  # Antall frihetsgrader (n-1, her 8-1=7)

# Lag dataramme for x-verdier
x_vals <- seq(-4, 4, length.out = 1000)

# Bruk ggplot for å lage en t-fordeling med skyggelagte områder
ggplot(data.frame(x = x_vals), aes(x = x)) +
  stat_function(fun = dt, args = list(df = df), color = "black", size = 1.2) +  # T-fordelingskurve
  geom_area(data = subset(data.frame(x = x_vals), x < -t_value), aes(x = x, y = dt(x, df)), fill = "gray", alpha = 0.5) +  # Venstre hale
  geom_area(data = subset(data.frame(x = x_vals), x > t_value), aes(x = x, y = dt(x, df)), fill = "gray", alpha = 0.5) +   # Høyre hale
  labs(title = "Observed t-value plotted in the null-hypothesis t-distribution", 
       x = "t", y = "Density f(t)") +
  theme_minimal()

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
