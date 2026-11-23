# Identidad y atribución operacional ----

.prod_campos <- list(marca = c("MrcCod", "CLMrcCod", "MrcNom", "CLMrcNom"),
                     nombre = c("MCCod", "CLMCCod", "MCNom", "CLMCNom"))

.prod_texto <- function(datos, columna) {
  x <- as.character(.dp_col(datos, columna))
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}

.prod_coalesce <- function(x, y) {
  x[is.na(x)] <- y[is.na(x)]
  x
}

.prod_id <- function(compania, codigo) {
  id <- .dp_id_pedido(compania, codigo)
  id[is.na(codigo) | is.na(compania)] <- NA_character_
  id
}

.prod_hechos <- function(publicacion) {
  tablas <- publicacion$tablas
  lineas <- tablas$lineas_pedido
  pedidos <- tablas$pedidos
  if (is.null(lineas) || is.null(pedidos)) return(NULL)
  .dp_clave(lineas, c("pedido_id", "PdcLin"), "líneas de productos")
  .dp_clave(pedidos, "pedido_id", "pedidos de productos")
  if (anyNA(lineas$LinNegCod) || any(lineas$LinNegCod != "20000")) stop("Línea ajena a productos.")
  cab <- pedidos[match(lineas$pedido_id, pedidos$pedido_id), , drop = FALSE]
  if (anyNA(cab$pedido_id)) stop("Hay líneas sin cabecera en productos.")
  fuentes_linea <- tablas$raw_lineas
  originales <- which(fuentes_linea$fuente == "lineas.csv")
  raw <- if (length(originales)) fuentes_linea$datos[[originales[1L]]] else NULL
  if (is.null(raw)) raw <- data.frame()
  if (nrow(raw)) {
    claves <- c("CiaCod", "PdcCod", "PdcLin")
    .dp_clave(raw, claves, "detalle original de productos")
    raw$pedido_id <- .dp_id_pedido(raw$CiaCod, raw$PdcCod)
    raw <- raw[match(.prod_id(lineas$pedido_id, lineas$PdcLin), .prod_id(raw$pedido_id, raw$PdcLin)), , drop = FALSE]
  } else raw <- lineas
  base <- data.frame(pedido_id = lineas$pedido_id, PdcLin = lineas$PdcLin, CiaCod = cab$CiaCod,
                     PerCod = cab$PerCod, fecha = cab$fecha_creacion, pais = lineas$PaiCodDtn, sacos = lineas$sacos,
                     estado = cab$estado_original, comercial = .prod_texto(cab, "comercial"))
  raw_pedidos <- tablas$raw_pedidos$datos[[1L]]
  if (!is.null(raw_pedidos) && "Comercial" %in% names(raw_pedidos)) {
    .dp_clave(raw_pedidos, c("CiaCod", "PdcCod"), "cabecera original de productos")
    ids <- .dp_id_pedido(raw_pedidos$CiaCod, raw_pedidos$PdcCod)
    base$comercial <- .prod_texto(raw_pedidos, "Comercial")[match(base$pedido_id, ids)]
  }
  for (dimension in names(.prod_campos)) {
    campos <- .prod_campos[[dimension]]
    base[[dimension]] <- .prod_coalesce(.prod_texto(lineas, campos[1L]), .prod_texto(raw, campos[2L]))
    base[[paste0(dimension, "_nombre")]] <- .prod_coalesce(.prod_texto(raw, campos[3L]), .prod_texto(raw, campos[4L]))
  }
  base$lote <- rep(NA_character_, nrow(base))
  base$embarcado <- base$peso_fcc <- base$fcc <- rep(NA_real_, nrow(base))
  base$fecha_embarque <- rep(as.Date(NA_character_), nrow(base))

  # Solo se acepta un puente certificado por el productor del corte, con cantidades propias o asignadas.
  lotes <- tablas$lotes_productos
  indice_lotes <- which(fuentes_linea$fuente == "lotes_productos.csv")
  if (is.null(lotes) && length(indice_lotes)) {
    lotes <- .prod_normalizar_lotes(fuentes_linea$datos[[indice_lotes[1L]]])
  }
  base$fuente <- rep("línea", nrow(base))
  base$conflicto <- rep(FALSE, nrow(base))
  if (is.null(lotes) || !nrow(lotes)) return(base)
  .prod_lotes(base, lotes)
}

.prod_lotes <- function(base, lotes) {
  claves <- c("pedido_id", "PdcLin", "lote")
  requeridas <- c(claves, "LinNegCod", "marca", "nombre", "marca_nombre", "nombre_nombre", "sacos",
                  "embarcado", "fecha_embarque", "peso_fcc", "fcc")
  if (!all(requeridas %in% names(lotes)) || anyNA(lotes$LinNegCod) || any(lotes$LinNegCod != "20000") ||
      !inherits(lotes$fecha_embarque, "Date") ||
      !all(vapply(lotes[c("sacos", "embarcado", "peso_fcc", "fcc")], is.numeric, logical(1)))) {
    stop("El puente lote–producto tiene un esquema incompatible.")
  }
  .dp_clave(lotes, claves, "puente lote–producto")
  repetidos <- duplicated(lotes$lote) | duplicated(lotes$lote, fromLast = TRUE)
  evidencia <- .prod_texto(lotes, "evidencia")
  if (any(repetidos) && (anyNA(evidencia[repetidos]) || anyDuplicated(evidencia[repetidos]))) {
    stop("Cada asignación de un lote compartido requiere evidencia distinta.")
  }
  if (any(!is.finite(lotes$sacos) | lotes$sacos < 0) ||
      any(lotes$embarcado < 0 | !is.finite(lotes$embarcado), na.rm = TRUE) ||
      any(!is.na(lotes$embarcado) & is.na(lotes$fecha_embarque)) ||
      any(!is.na(lotes$fcc) & (!is.finite(lotes$fcc) | is.na(lotes$peso_fcc) |
                              !is.finite(lotes$peso_fcc) | lotes$peso_fcc <= 0))) {
    stop("Las cantidades o el peso FCC del puente requieren conciliación.")
  }
  llave <- .prod_id(base$pedido_id, base$PdcLin)
  indice <- match(.prod_id(lotes$pedido_id, lotes$PdcLin), llave)
  if (anyNA(indice)) stop("Hay lotes de productos sin línea elegible.")
  filas <- base[indice, , drop = FALSE]
  columnas <- setdiff(requeridas, c("pedido_id", "PdcLin", "LinNegCod"))
  conflicto <- function(dimension) {
    original <- filas[[dimension]]
    nuevo <- .prod_texto(lotes, dimension)
    !is.na(original) & !is.na(nuevo) & original != nuevo
  }
  filas$conflicto <- conflicto("marca") | conflicto("nombre")
  filas[columnas] <- lotes[columnas]
  for (dimension in c("marca", "nombre")) filas[[dimension]] <- .prod_texto(lotes, dimension)
  if ("pais" %in% names(lotes)) filas$pais <- .prod_coalesce(.prod_texto(lotes, "pais"), filas$pais)
  filas$fuente <- "lote"
  asignados <- .dp_agregar(as.character(indice), lotes$sacos, as.character(seq_len(nrow(base))))
  saldo <- base$sacos - asignados
  if (any(saldo < -1e-7, na.rm = TRUE)) stop("El volumen de lotes supera el volumen de la línea.")
  sin_lote <- base[is.na(asignados), , drop = FALSE]
  residual <- base[!is.na(asignados) & (is.na(saldo) | saldo > 1e-7), , drop = FALSE]
  residual$sacos <- saldo[!is.na(asignados) & (is.na(saldo) | saldo > 1e-7)]
  sin_atribuir <- c("marca", "nombre", "marca_nombre", "nombre_nombre")
  residual[sin_atribuir] <- lapply(residual[sin_atribuir], function(x) rep(NA_character_, length(x)))
  residual$fuente <- rep("saldo sin atribución", nrow(residual))
  rbind(sin_lote, filas, residual)
}

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


# Adaptación de la exportación certificada de lotes ----

.prod_normalizar_lotes <- function(raw) {
  claves <- c("CiaCod", "CLPdcCod", "CLPdcLin", "CLLotCod")
  if (!all(c(claves, "CLLinNegCo", "SacosAsignados") %in% names(raw)) ||
      anyNA(raw$CLLinNegCo) || any(!grepl("^[0-9]+$", raw$CLLinNegCo))) {
    stop("La exportación de lotes no contiene llaves, línea original y asignación verificable.")
  }
  raw <- raw[raw$CLLinNegCo == "20000", , drop = FALSE]
  .dp_clave(raw, claves, "lotes comerciales")
  campos <- c("SacosAsignados", "SacosEmbarcados", "PesoFCCKg", "MNFCCCOPKg")
  numeros <- lapply(campos, function(campo) {
    texto <- .dp_col(raw, campo)
    valor <- .dp_numero(texto)
    if (any(!is.na(texto) & is.na(valor))) stop("Cantidad inválida de lotes: ", campo)
    valor
  })
  data.frame(pedido_id = .dp_id_pedido(raw$CiaCod, raw$CLPdcCod), PdcLin = raw$CLPdcLin,
             lote = .prod_id(raw$CiaCod, raw$CLLotCod), LinNegCod = raw$CLLinNegCo,
             evidencia = .prod_texto(raw, "ReferenciaAsignacion"),
             marca = .prod_texto(raw, "CLMrcCod"), nombre = .prod_texto(raw, "CLMCCod"),
             marca_nombre = .prod_texto(raw, "CLMrcNom"), nombre_nombre = .prod_texto(raw, "CLMCNom"),
             pais = .prod_texto(raw, "CLPaiCodDt"), sacos = numeros[[1L]], embarcado = numeros[[2L]],
             fecha_embarque = .dp_fecha(.dp_col(raw, "FechaEmbarqueEfectivo")),
             peso_fcc = numeros[[3L]], fcc = numeros[[4L]])
}


.prod_crecimiento <- function(datos, periodo) {
  mitad <- periodo[1L] + floor(as.numeric(diff(periodo)) / 2)
  d <- datos[!is.na(datos$item) & !is.na(datos$fecha), , drop = FALSE]
  dplyr::summarise(dplyr::group_by(d, item, CiaCod, codigo),
                   SacosInicial = .dp_suma(sacos[fecha <= mitad]), SacosFinal = .dp_suma(sacos[fecha > mitad]),
                   Diferencia = SacosFinal - SacosInicial,
                   Variacion = if (is.finite(SacosInicial) && SacosInicial > 0) Diferencia / SacosInicial else NA_real_,
                   .groups = "drop")
}
