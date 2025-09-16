# =========================================================
# BOOTSTRAP EN R: ESTADÍSTICOS COMPLETOS
# =========================================================
# Incluye: media, mediana, varianza y desviación estándar
# con verificación de normalidad, intervalos de confianza
# y visualización gráfica.
# =========================================================

# ----------------------------------
# 1. Instalación y carga de librerías
# ----------------------------------
# Descomentar si no tienes instaladas:
# install.packages("boot")
# install.packages("ggplot2")
# install.packages("gridExtra")

library(boot)       # Para bootstrap
library(ggplot2)    # Para gráficos
library(gridExtra)  # Para combinar gráficos

# ----------------------------------
# 2. Generamos datos normales
# ----------------------------------
set.seed(123)
n <- 100
muestra <- rnorm(n, mean = 100, sd = 15)  # Normal(100, 15^2)

# ----------------------------------
# 3. Comprobamos normalidad
# ----------------------------------
cat("===== Test de Normalidad =====\n")
print(shapiro.test(muestra))   # Test de Shapiro-Wilk
qqnorm(muestra, main = "QQ-Plot para verificar normalidad")
qqline(muestra, col = "red")

# ----------------------------------
# 4. Definimos funciones para estadísticos
# ----------------------------------
f_media <- function(data, i) mean(data[i])
f_mediana <- function(data, i) median(data[i])
f_var <- function(data, i) var(data[i])
f_sd <- function(data, i) sd(data[i])

# ----------------------------------
# 5. Bootstrap para cada estadístico
# ----------------------------------
R <- 2000  # número de réplicas bootstrap

boot_media   <- boot(muestra, f_media, R)
boot_mediana <- boot(muestra, f_mediana, R)
boot_var     <- boot(muestra, f_var, R)
boot_sd      <- boot(muestra, f_sd, R)

# ----------------------------------
# 6. Intervalos de confianza
# ----------------------------------
cat("===== Intervalos de confianza (95%) =====\n")
cat("Media:\n");   print(boot.ci(boot_media, type = c("perc", "bca")))
cat("Mediana:\n"); print(boot.ci(boot_mediana, type = c("perc", "bca")))
cat("Varianza:\n");print(boot.ci(boot_var, type = c("perc", "bca")))
cat("Desv. Estándar:\n"); print(boot.ci(boot_sd, type = c("perc", "bca")))

# ----------------------------------
# 7. Visualización
# ----------------------------------
# Creamos dataframes para graficar
df_media   <- data.frame(valor = boot_media$t,   estadistico = "Media")
df_mediana <- data.frame(valor = boot_mediana$t, estadistico = "Mediana")
df_var     <- data.frame(valor = boot_var$t,     estadistico = "Varianza")
df_sd      <- data.frame(valor = boot_sd$t,      estadistico = "Desv. Estándar")

# Histograma + densidad para cada estadístico
plot_boot <- function(df, original, titulo) {
  ic <- boot.ci(boot(data = muestra, statistic = function(d,i) df$valor, R=2000), type="perc")$percent[4:5]
  
  ggplot(df, aes(x = valor)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = original, color = "red", linetype = "dashed", size = 1) +
    labs(title = titulo, x = "Valores bootstrap", y = "Frecuencia") +
    theme_minimal()
}

# Gráficos individuales
p1 <- ggplot(df_media, aes(x = valor)) +
  geom_density(fill="lightblue", alpha=0.6) +
  geom_vline(xintercept = mean(muestra), color="red", linetype="dashed") +
  labs(title="Bootstrap Media", x="Media", y="Densidad") +
  theme_minimal()

p2 <- ggplot(df_mediana, aes(x = valor)) +
  geom_density(fill="lightgreen", alpha=0.6) +
  geom_vline(xintercept = median(muestra), color="red", linetype="dashed") +
  labs(title="Bootstrap Mediana", x="Mediana", y="Densidad") +
  theme_minimal()

p3 <- ggplot(df_var, aes(x = valor)) +
  geom_density(fill="orange", alpha=0.6) +
  geom_vline(xintercept = var(muestra), color="red", linetype="dashed") +
  labs(title="Bootstrap Varianza", x="Varianza", y="Densidad") +
  theme_minimal()

p4 <- ggplot(df_sd, aes(x = valor)) +
  geom_density(fill="purple", alpha=0.6) +
  geom_vline(xintercept = sd(muestra), color="red", linetype="dashed") +
  labs(title="Bootstrap Desv. Estándar", x="SD", y="Densidad") +
  theme_minimal()

# Mostrar en una cuadrícula
grid.arrange(p1, p2, p3, p4, ncol=2)



