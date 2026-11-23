# Publicación operacional ----

PublicarOperacionalRDS <- function(raiz, tablas, contratos, procedencia) {
  # La transformación se completa antes de publicar y documenta sus exportaciones de origen.
  campos <- c("fuentes", "corte")
  if (!is.list(procedencia) || !all(campos %in% names(procedencia)) ||
      !length(procedencia$fuentes) || !length(procedencia$corte)) {
    stop("Indique las fuentes y el corte de las exportaciones operacionales.", call. = FALSE)
  }
  source(file.path(raiz, "APP", "misc", "persistencia", "rds.R"), local = TRUE)
  almacen <- AbrirPersistenciaRDS(raiz, list(operacional = contratos))
  ActualizarCorteRDS(almacen, "operacional", function(anterior) tablas, procedencia = procedencia)
}
