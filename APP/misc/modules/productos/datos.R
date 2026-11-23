# Análisis de productos ----

.prod_dimension <- function(datos, dimension) {
  datos$codigo <- datos[[dimension]]
  datos$descripcion <- datos[[paste0(dimension, "_nombre")]]
  datos$item <- .prod_id(datos$CiaCod, datos$codigo)
  datos
}

.prod_catalogo <- function(datos, corte) {
  identificados <- datos[!is.na(datos$item), , drop = FALSE]
  grupos <- split(identificados, identificados$item)
  filas <- lapply(grupos, function(x) {
    nombres <- sort(unique(x$descripcion[!is.na(x$descripcion)]))
    data.frame(item = x$item[1L], CiaCod = x$CiaCod[1L], codigo = x$codigo[1L],
               nombre = if (length(nombres) == 1L) nombres else NA_character_,
               descripciones = paste(nombres, collapse = " | "), conflicto = length(nombres) > 1L,
               linea_negocio = "20000", fuente = paste(sort(unique(x$fuente)), collapse = ", "),
               corte = rep(as.character(corte)[1L], 1L))
  })
  if (!length(filas)) return(data.frame(item = character(), codigo = character(), nombre = character()))
  do.call(rbind, filas)
}

.prod_contar <- function(x) length(unique(x[!is.na(x) & nzchar(x)]))

.prod_resumen <- function(datos, claves) {
  agrupado <- dplyr::group_by(datos, dplyr::across(dplyr::all_of(claves)))
  dplyr::summarise(agrupado, Sacos = .dp_suma(sacos), Embarcado = if (all(is.na(embarcado))) NA_real_ else
                     sum(embarcado, na.rm = TRUE),
                   Clientes = .prod_contar(PerCod), Pedidos = .prod_contar(pedido_id),
                   Lotes = if (all(is.na(lote))) NA_integer_ else .prod_contar(lote),
                   Frecuencia = if (Clientes > 0L) Pedidos / Clientes else NA_real_,
                   PesoFCC = sum(peso_fcc[is.finite(fcc)], na.rm = TRUE),
                   MargenCOP = if (PesoFCC > 0) sum(peso_fcc[is.finite(fcc)] * fcc[is.finite(fcc)]) else NA_real_,
                   MNFCC = if (PesoFCC > 0) MargenCOP / PesoFCC else NA_real_, .groups = "drop")
}

.prod_filtrar <- function(datos, filtros, periodo, fecha = "fecha", comparable = FALSE) {
  if (!is.null(periodo) && length(periodo) == 2L && !anyNA(periodo)) {
    fechas <- datos[[fecha]]
    datos <- datos[which(fechas >= periodo[1L] & fechas <= periodo[2L]), , drop = FALSE]
  }
  if (length(filtros$destino)) datos <- datos[datos$pais %in% filtros$destino, , drop = FALSE]
  if (length(filtros$estado)) datos <- datos[datos$estado %in% filtros$estado, , drop = FALSE]
  if (!comparable) {
    if (length(filtros$cliente)) datos <- datos[datos$PerCod %in% filtros$cliente, , drop = FALSE]
    if (nzchar(filtros$percod)) datos <- datos[datos$PerCod %in% filtros$percod, , drop = FALSE]
  }
  datos
}
