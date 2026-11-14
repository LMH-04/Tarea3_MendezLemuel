# Librerías necesarias para las funciones de este archivo
library(dplyr)


# ------------------------------------------------------------
# alerta.movil
# Recibe:
#   df        data frame de transacciones con la columna fecha
#             (una fila por transacción).
#   ventana   número de días anteriores que forman lo "normal" (v).
#   k.alerta  número de desviaciones estándar sobre la media (k).
# Devuelve:
#   data frame con una fila por día, ordenado por fecha, con
#   fecha, n.tx (transacciones del día), media y desv (media y
#   desviación estándar muestral de los v días anteriores) y
#   alerta (TRUE si n.tx > media + k.alerta * desv).
#   Los primeros v días no se evalúan: media, desv y alerta = NA.
# ------------------------------------------------------------
alerta.movil <- function(df, ventana, k.alerta) {
  conteo <- table(as.Date(df$fecha))
  diario <- data.frame(fecha = as.Date(names(conteo)),
                       n.tx  = as.integer(conteo))

  n.dias <- nrow(diario)
  media  <- rep(NA_real_, n.dias)
  desv   <- rep(NA_real_, n.dias)

  for (t in seq_len(n.dias)) {
    if (t > ventana) {
      previos  <- diario$n.tx[(t - ventana):(t - 1)]   # sin incluir el día t
      media[t] <- mean(previos)
      desv[t]  <- sd(previos)
    }
  }

  diario$media  <- media
  diario$desv   <- desv
  diario$alerta <- diario$n.tx > media + k.alerta * desv
  diario
}


# ------------------------------------------------------------
# benford.mad
# Recibe:
#   x   vector numérico de montos (positivos).
# Devuelve:
#   un número: la desviación absoluta media (MAD) entre la
#   proporción observada del primer dígito (p_d) y la de
#   Benford (B_d = log10(1 + 1/d)), para d = 1, ..., 9.
# ------------------------------------------------------------
benford.mad <- function(x) {
  x <- x[!is.na(x) & x > 0]
  digito  <- x %/% 10^floor(log10(x))           # primer dígito, de 1 a 9
  p.obs   <- tabulate(digito, nbins = 9) / length(x)
  benford <- log10(1 + 1 / (1:9))
  mean(abs(p.obs - benford))                    # = (1/9) * suma |p_d - B_d|
}


# ------------------------------------------------------------
# benford.canton
# Recibe:
#   df        data frame de transacciones con cod_canton, canal
#             y monto_crc.
#   canal     canal a auditar (ej. "Web").
#   cantones  catálogo con cod_canton y canton.
#   limites   cortes de MAD de Nigrini para la clase:
#             cercana, aceptable y marginal.
# Devuelve:
#   data frame con una fila por cantón: cod_canton, canton,
#   n (transacciones del canal), mad y clase, ordenado de
#   mayor a menor MAD.
# ------------------------------------------------------------
benford.canton <- function(df, canal, cantones,
                           limites = c(0.006, 0.012, 0.015)) {
  df |>
    filter(.data$canal == .env$canal) |>          # columna canal == argumento canal
    summarise(n   = n(),
              mad = benford.mad(monto_crc),
              .by = cod_canton) |>
    left_join(select(cantones, cod_canton, canton), by = "cod_canton") |>
    mutate(clase = as.character(cut(
      mad,
      breaks = c(-Inf, limites, Inf),
      labels = c("conformidad cercana", "conformidad aceptable",
                 "conformidad marginal", "no conformidad")
    ))) |>
    arrange(desc(mad)) |>
    select(cod_canton, canton, n, mad, clase)
}


# ------------------------------------------------------------
# valor.acumulado
# Recibe:
#   df          data frame de transacciones con fecha, canal y
#               monto_crc.
#   tasa.anual  tasa efectiva anual i, en decimal (0.06 = 6 %).
#   canal       canal cuyos montos se depositan cada mes.
# Devuelve:
#   lista con
#     tabla         data frame con mes, monto (S_m, total del mes)
#                   y factor ((1 + i_m)^(12 - m));
#     tasa.mensual  tasa efectiva mensual equivalente i_m;
#     va            valor acumulado al 31-dic: suma de S_m * factor;
#     interes       intereses ganados: va - suma de S_m.
# ------------------------------------------------------------
valor.acumulado <- function(df, tasa.anual, canal) {
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1

  tabla <- df |>
    filter(.data$canal == .env$canal) |>
    mutate(mes = as.integer(format(as.Date(fecha), "%m"))) |>
    summarise(monto = sum(as.numeric(monto_crc)), .by = mes) |>   # as.numeric: evita desbordar enteros
    arrange(mes) |>
    mutate(factor = (1 + tasa.mensual)^(12 - mes))

  va <- sum(tabla$monto * tabla$factor)

  list(tabla        = tabla,
       tasa.mensual = tasa.mensual,
       va           = va,
       interes      = va - sum(tabla$monto))
}
