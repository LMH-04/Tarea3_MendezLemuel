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
