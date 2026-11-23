# Cestas y recomendaciones ----

.prod_cestas <- function(datos, tipo) {
  pedido_valido <- is.na(datos$sacos) | datos$sacos > 0
  datos <- datos[pedido_valido & !is.na(datos$fecha) & !is.na(datos$item) & !is.na(datos$PerCod), , drop = FALSE]
  llave <- switch(tipo, pedido = datos$pedido_id, mes = .prod_id(datos$PerCod, format(datos$fecha, "%Y-%m")),
                   periodo = datos$PerCod, stop("Unidad de cesta inválida."))
  mapa <- sort(unique(datos$item))
  ids <- sort(unique(llave))
  membresia <- unique(data.frame(Cesta = match(llave, ids), Item = match(datos$item, mapa)))
  fecha <- if (tipo == "mes") as.Date(format(datos$fecha, "%Y-%m-01")) else datos$fecha
  fechas <- tapply(as.numeric(fecha), match(llave, ids), max)
  list(membresia = membresia, ids = seq_along(ids), fecha = as.Date(as.numeric(fechas), origin = "1970-01-01"),
       mapa = mapa, pedidos = .prod_contar(datos$pedido_id), clientes = .prod_contar(datos$PerCod))
}

.prod_motor <- function() {
  if (!requireNamespace("racafeForecast", quietly = TRUE)) {
    stop("Instale racafeForecast para calcular Market Basket.", call. = FALSE)
  }
  list(conjuntos = racafeForecast::ConjuntosFrecuentes, reglas = racafeForecast::ReglasAsociacion,
       validar = racafeForecast::ValidarReglasAsociacion)
}

.prod_analizar_cestas <- function(cestas, soporte, minimo, longitud, confianza, lift, temporal, estadistica,
                                 motor = .prod_motor()) {
  ids <- cestas$ids
  seg <- integer()
  corte <- as.Date(NA_character_)
  if (temporal) {
    fechas <- sort(unique(cestas$fecha))
    if (length(fechas) < 2L) stop("La validación requiere al menos dos fechas de cesta diferentes.")
    corte <- fechas[min(length(fechas) - 1L, max(1L, floor(length(fechas) * .7)))]
    seg <- ids[cestas$fecha > corte]
    ids <- ids[cestas$fecha <= corte]
  }
  if (length(ids) < minimo) stop("Cestas de entrenamiento insuficientes para el mínimo elegido.")
  modelo <- motor$conjuntos(cestas$membresia, ids, soporte, minimo, longitud)
  reglas <- motor$reglas(modelo, confianza, lift, estadistica)
  if (temporal) reglas <- motor$validar(reglas, cestas$membresia, seg)
  decodificar <- function(x) cestas$mapa[as.integer(strsplit(x, ",", fixed = TRUE)[[1L]])]
  conjuntos <- lapply(modelo$conjuntos, function(x) {
    data.frame(Conjunto = paste(cestas$mapa[x$items], collapse = " + "), Longitud = length(x$items),
               Cestas = x$cuenta, Soporte = x$cuenta / modelo$n)
  })
  conjuntos <- dplyr::bind_rows(conjuntos)
  reglas$Compra <- vapply(reglas$Antecedente, function(x) paste(decodificar(x), collapse = " + "), character(1))
  reglas$Asociado <- vapply(reglas$Consecuente, function(x) paste(decodificar(x), collapse = " + "), character(1))
  reglas$CestasModelo <- rep(length(ids), nrow(reglas))
  evidencia <- cestas$membresia[cestas$membresia$Cesta %in% ids, , drop = FALSE]
  frecuencia <- table(evidencia$Item)
  items <- data.frame(Item = cestas$mapa[as.integer(names(frecuencia))], Cestas = as.integer(frecuencia),
                      Soporte = as.integer(frecuencia) / length(ids))
  tamanos <- table(table(cestas$membresia$Cesta))
  principales <- head(as.integer(names(sort(frecuencia, decreasing = TRUE))), 12L)
  matriz <- matrix(0L, nrow = length(principales), ncol = length(principales),
                   dimnames = rep(list(cestas$mapa[principales]), 2L))
  if (length(principales)) {
    pares <- evidencia[evidencia$Item %in% principales, , drop = FALSE]
    presencia <- table(pares$Cesta, factor(pares$Item, levels = principales))
    matriz[,] <- crossprod(presencia)
  }
  list(reglas = reglas, conjuntos = conjuntos, items = items,
       pares = if (nrow(conjuntos)) conjuntos[conjuntos$Longitud == 2L, , drop = FALSE] else conjuntos,
       tamanos = data.frame(Items = as.integer(names(tamanos)), Cestas = as.integer(tamanos)),
       matriz = matriz, corte = corte, cestas_modelo = length(ids), cestas_validacion = length(seg), mapa = cestas$mapa,
       temporal = temporal, minimo = minimo)
}

.prod_recomendar <- function(analisis, historial, percod, dimension, periodo) {
  compras <- unique(historial$item[historial$PerCod %in% percod])
  filas <- lapply(seq_len(nrow(analisis$reglas)), function(i) {
    regla <- analisis$reglas[i, , drop = FALSE]
    a <- analisis$mapa[as.integer(strsplit(regla$Antecedente, ",", fixed = TRUE)[[1L]])]
    b <- analisis$mapa[as.integer(strsplit(regla$Consecuente, ",", fixed = TRUE)[[1L]])]
    if (!all(a %in% compras) || !length(setdiff(b, compras))) return(NULL)
    if (analisis$temporal && (regla$AmbosValidacion < analisis$minimo ||
        !is.finite(regla$LiftValidacion) || regla$LiftValidacion < 1)) return(NULL)
    sugeridos <- setdiff(b, compras)
    data.frame(PerCod = percod, dimension, compra = regla$Compra, sugerido = sugeridos,
               soporte = regla$Soporte, confianza = regla$Confianza, lift = regla$Lift,
               evidencia = regla$CestasAmbos, cestas_modelo = regla$CestasModelo,
               desde = periodo[1L], hasta = periodo[2L], calculado = Sys.time(),
               lift_validacion = if (analisis$temporal) regla$LiftValidacion else NA_real_,
               q_valor = if ("Qvalor" %in% names(regla)) regla$Qvalor else NA_real_)
  })
  salida <- dplyr::bind_rows(filas)
  if (!nrow(salida)) return(salida)
  salida[!duplicated(salida$sugerido), , drop = FALSE]
}

# Aceptación explícita y auditada ----

.prod_contrato_oportunidad <- function() {
  prototipo <- data.frame(oportunidad_id = character(), PerCod = character(), tipo = character(),
                         dimension = character(), item = character(), antecedente = character(),
                         soporte = numeric(), confianza = numeric(), lift = numeric(), evidencia = integer(),
                         cestas_modelo = integer(), desde = as.Date(character()), hasta = as.Date(character()),
                         calculado = as.POSIXct(character()), lift_validacion = numeric(), q_valor = numeric(),
                         usuario = character(), aceptada_en = as.POSIXct(character()), responsable = character(),
                         estado = character())
  list(version = "v1", clave = "oportunidad_id", prototipo = prototipo)
}

.prod_autorizado <- function(usuario, permitidos = Sys.getenv("CRM_CROSS_SELLING_USUARIOS")) {
  length(usuario) == 1L && !is.na(usuario) && nzchar(usuario) &&
    usuario %in% trimws(strsplit(permitidos, ",", fixed = TRUE)[[1L]])
}

.prod_aceptar <- function(raiz, fila, usuario, motivo) {
  if (!.prod_autorizado(usuario)) stop("Su usuario no tiene permiso para aceptar recomendaciones.")
  contrato <- .prod_contrato_oportunidad()
  almacen <- AbrirPersistenciaRDS(raiz, list(crm = list(oportunidades = contrato)))
  id <- .prod_id(.prod_id(fila$PerCod, fila$dimension), fila$sugerido)
  nueva <- data.frame(oportunidad_id = id, PerCod = fila$PerCod, tipo = "CROSS_SELLING",
                      dimension = fila$dimension, item = fila$sugerido, antecedente = fila$compra,
                      soporte = fila$soporte, confianza = fila$confianza, lift = fila$lift,
                      evidencia = as.integer(fila$evidencia), cestas_modelo = as.integer(fila$cestas_modelo),
                      desde = fila$desde, hasta = fila$hasta, calculado = fila$calculado,
                      lift_validacion = fila$lift_validacion, q_valor = fila$q_valor,
                      usuario, aceptada_en = Sys.time(), responsable = usuario, estado = "PENDIENTE")
  ActualizarRDS(almacen, "oportunidades", "crm", function(tabla) {
    if (id %in% tabla$oportunidad_id) stop("Ya existe una oportunidad para este cliente e ítem.")
    rbind(tabla, nueva)
  }, usuario = usuario, motivo = motivo, referencia = id)
}
