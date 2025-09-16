# Instalar librerías si no las tienes
install.packages("boot")      # para bootstrap
install.packages("ggplot2")   # para gráficos
install.packages("dplyr")     # para manipulación de datos

# Cargar librerías
library(boot)
library(ggplot2)
library(dplyr)

# Dataset de ejemplo
data(mtcars)

# ================================
# 1. Función estadística: media de mpg
# ================================
media_mpg <- function(data, indices) {
  muestra <- data[indices, ]       # remuestreo con reemplazo
  return(mean(muestra$mpg))        # calcular la media de mpg
}

# ================================
# 2. Aplicar bootstrap
# ================================
set.seed(123)   # Semilla para reproducibilidad
resultado <- boot(data = mtcars, statistic = media_mpg, R = 2000)

# Mostrar resumen
print(resultado)

# Intervalo de confianza bootstrap al 95%
ic <- boot.ci(resultado, type = c("perc", "bca"))
print(ic)

# ================================
# 3. Gráfico de la distribución bootstrap
# ================================
# Guardar los valores bootstrap
bootstrap_vals <- resultado$t

# Crear un data frame para graficar
df_boot <- data.frame(media_bootstrap = bootstrap_vals)

# Gráfico con ggplot2
ggplot(df_boot, aes(x = media_bootstrap)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(mtcars$mpg)),
             color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = ic$perc[4]),
             color = "darkgreen", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = ic$perc[5]),
             color = "darkgreen", linetype = "dashed", size = 1) +
  labs(title = "Distribución Bootstrap de la Media de mpg",
       subtitle = "Líneas rojas = media original | Líneas verdes = IC al 95%",
       x = "Media bootstrap de mpg",
       y = "Frecuencia") +
  theme_minimal(base_size = 14)

