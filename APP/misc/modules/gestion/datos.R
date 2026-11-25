# Contratos y operaciones comerciales ----

.gestion_estados <- list(oportunidad = c("ABIERTA", "CONCRETADA", "APLAZADA", "NO_PROSPERO"),
                         oferta = c("ABIERTA", "CONCRETADA", "APLAZADA", "NO_PROSPERO"),
                         muestra = c("SOLICITADA", "EN_PREPARACION", "ENVIADA", "PENDIENTE_RESPUESTA",
                                     "APROBADA", "RECHAZADA"),
                         tarea = c("PENDIENTE", "REALIZADA", "CANCELADA"))

.gestion_contratos <- function() {
  ref <- function(tabla, columna) list(tabla = tabla, columnas = columna)
  contrato <- function(id, clases, referencias = list(), requeridos = "PerCod") {
    list(version = "v1", clave = id, requeridos = requeridos, prototipo = clases, referencias = referencias)
  }
  ch <- character()
  fecha <- as.Date(ch)
  instante <- as.POSIXct(ch)
  list(
    oportunidades = .prod_contrato_oportunidad(),
    oportunidad_detalle = contrato("oportunidad_id",
      data.frame(oportunidad_id = ch, PerCod = ch, asunto = ch, producto = ch, sacos = numeric(),
                 embarque_estimado = fecha, pedido_id = ch, motivo = ch),
      list(ref("oportunidades", "oportunidad_id"))),
    ofertas = contrato("oferta_id", data.frame(oferta_id = ch, oportunidad_id = ch, PerCod = ch,
      producto = ch, sacos = numeric(), valor = numeric(), unidad = ch, condiciones = ch, embarque_estimado = fecha,
      estado = ch, motivo = ch, fecha = fecha, responsable = ch), list(ref("oportunidades", "oportunidad_id"))),
    muestras = contrato("muestra_id", data.frame(muestra_id = ch, PerCod = ch, producto = ch, calidad = ch,
      fecha = fecha, estado = ch, pedido_id = ch, resultado = ch, responsable = ch, actualizada = instante)),
    tareas = contrato("tarea_id", data.frame(tarea_id = ch, PerCod = ch, tipo = ch, asunto = ch, resultado = ch,
      fecha = fecha, seguimiento = fecha, estado = ch, oportunidad_id = ch, muestra_id = ch, responsable = ch,
      actualizada = instante), list(ref("oportunidades", "oportunidad_id"), ref("muestras", "muestra_id")),
      requeridos = "tipo"),
    tarea_prioridad = contrato("tarea_id", data.frame(tarea_id = ch, PerCod = ch, prioridad = ch),
      list(ref("tareas", "tarea_id")), requeridos = "prioridad"),
    tarea_etiquetas = contrato(c("tarea_id", "etiqueta"),
      data.frame(tarea_id = ch, PerCod = ch, etiqueta = ch), list(ref("tareas", "tarea_id")),
      requeridos = character()),
    tarea_actividades = contrato(c("tarea_id", "orden"),
      data.frame(tarea_id = ch, PerCod = ch, orden = integer(), actividad = ch, cumplida = logical()),
      list(ref("tareas", "tarea_id")), requeridos = character()),
    tarea_archivo = contrato("tarea_id", data.frame(tarea_id = ch, estado = ch),
      list(ref("tareas", "tarea_id")), requeridos = "estado"),
    contactos = contrato("contacto_id", data.frame(contacto_id = ch, PerCod = ch, nombre = ch,
      correo = ch, telefono = ch, preferencia_calidad = ch))
  )
}

.gestion_actualizar <- function(raiz, funcion, usuario, motivo, referencia) {
  almacen <- AbrirPersistenciaRDS(raiz, list(crm = .gestion_contratos()))
  ActualizarCorteRDS(almacen, "crm", funcion, usuario = usuario, motivo = motivo,
                     referencia = referencia, ampliar = TRUE)
}

.gestion_filas <- function(tablas, nombre) {
  tabla <- tablas[[nombre]]
  if (is.null(tabla)) .gestion_contratos()[[nombre]]$prototipo else tabla
}

.gestion_oportunidades <- function(tablas) {
  oportunidades <- .gestion_filas(tablas, "oportunidades")
  campos <- c("oportunidad_id", "PerCod", "tipo", "item", "estado", "responsable")
  oportunidades <- oportunidades[campos]
  detalle <- .gestion_filas(tablas, "oportunidad_detalle")
  detalle$PerCod <- NULL
  salida <- merge(oportunidades, detalle, by = "oportunidad_id", all.x = TRUE, sort = FALSE)
  salida$asunto <- ifelse(is.na(salida$asunto), salida$item, salida$asunto)
  salida$estado[salida$estado == "PENDIENTE"] <- "ABIERTA"
  salida$item <- NULL
  salida
}

.gestion_texto <- function(valor) {
  if (length(valor) != 1L || is.na(valor) || !nzchar(trimws(valor))) return(NA_character_)
  trimws(valor)
}

.gestion_campo <- function(valor, etiqueta) {
  texto <- .gestion_texto(valor)
  if (is.na(texto)) stop("Complete ", etiqueta, ".", call. = FALSE)
  texto
}

.gestion_fecha <- function(valor, etiqueta, requerida = FALSE) {
  if (length(valor) != 1L || is.na(valor) || !nzchar(trimws(as.character(valor)))) {
    if (requerida) stop("Complete ", etiqueta, ".", call. = FALSE)
    return(as.Date(NA_character_))
  }
  fecha <- .dp_fecha(as.character(valor))
  if (is.na(fecha)) stop("Fecha inválida para ", etiqueta, ".", call. = FALSE)
  fecha
}

.gestion_numero <- function(valor, etiqueta, positivo = FALSE) {
  if (!length(valor) || is.na(valor)) return(NA_real_)
  if (!is.finite(valor) || (positivo && valor <= 0)) stop("Valor inválido para ", etiqueta, ".", call. = FALSE)
  as.numeric(valor)
}

.gestion_dominio <- function(valor, opciones) {
  if (length(valor) != 1L || !valor %in% opciones) stop("Seleccione un valor permitido.", call. = FALSE)
  valor
}

.gestion_vinculo <- function(tabla, clave, id, cliente) {
  if (is.na(id)) return(invisible(NULL))
  filas <- which(tabla[[clave]] == id & tabla$PerCod == cliente)
  if (length(filas) != 1L) stop("La referencia no pertenece a este cliente.", call. = FALSE)
  invisible(filas)
}

.gestion_upsert <- function(tabla, fila, clave) {
  indice <- match(fila[[clave]], tabla[[clave]])
  if (is.na(indice)) return(rbind(tabla, fila))
  tabla[indice, ] <- fila
  tabla
}

.crm_es_admin <- function(usuario, grupos = character()) {
  identidad_valida <- length(usuario) == 1L && !is.na(usuario) && nzchar(trimws(usuario))
  identidad_valida && (tolower(trimws(usuario)) == "lmgranados" ||
    any(toupper(trimws(grupos)) %in% c("ANALITICA", "ANALÍTICA")))
}

.gestion_autorizada <- function(usuario, grupos = character()) {
  .crm_es_admin(usuario, grupos) || .prod_autorizado(usuario, Sys.getenv("CRM_GESTION_USUARIOS"))
}

.gestion_guardar <- function(raiz, tipo, campos, origen, usuario, clientes, pedidos, esperado = NULL, grupos = character()) {
  if (!.gestion_autorizada(usuario, grupos)) {
    stop("La cuenta no tiene permiso para registrar gestión.", call. = FALSE)
  }
  nota <- tipo == "tarea" && identical(campos$tipo, "NOTA")
  cliente <- .gestion_texto(campos$PerCod)
  sin_cliente <- tipo == "tarea" && (nota || identical(campos$tipo, "SEGUIMIENTO"))
  if ((!sin_cliente || !is.na(cliente)) && !cliente %in% clientes$PerCod) {
    stop("Elija un código de cliente del corte operacional.", call. = FALSE)
  }
  id <- if (is.null(origen)) basename(tempfile(paste0(tipo, "-"))) else origen
  estado <- if (tipo != "contacto") .gestion_dominio(campos$estado, .gestion_estados[[tipo]])
  pedido <- .gestion_texto(campos$pedido_id)
  if (!is.na(pedido)) .gestion_vinculo(pedidos, "pedido_id", pedido, cliente)
  if (tipo == "oportunidad" && estado == "CONCRETADA" && is.na(pedido)) {
    stop("Vincule el pedido para marcar la oportunidad como concretada.", call. = FALSE)
  }
  motivo <- .gestion_campo(campos$motivo, "motivo del registro")
  .gestion_actualizar(raiz, function(t) {
    ahora <- Sys.time()
    tabla <- switch(tipo, oportunidad = "oportunidades", oferta = "ofertas", muestra = "muestras",
                    tarea = "tareas", contacto = "contactos")
    llave <- .gestion_contratos()[[tabla]]$clave
    if (!is.null(origen)) {
      actual <- t[[tabla]][which(t[[tabla]][[llave]] == id), , drop = FALSE]
      if (nrow(actual) != 1L || !identical(actual$PerCod, cliente)) {
        stop("La referencia no pertenece a este cliente.", call. = FALSE)
      }
      detalle <- t$oportunidad_detalle[t$oportunidad_detalle$oportunidad_id == id, , drop = FALSE]
      if (!identical(actual, esperado$fila) || (tipo == "oportunidad" && !identical(detalle, esperado$detalle))) {
        stop("El registro cambió desde que abrió el formulario. Vuelva a consultarlo.", call. = FALSE)
      }
    }
    opp <- .gestion_texto(campos$oportunidad_id)
    muestra <- .gestion_texto(campos$muestra_id)
    .gestion_vinculo(t$oportunidades, "oportunidad_id", opp, cliente)
    .gestion_vinculo(t$muestras, "muestra_id", muestra, cliente)
    if (tipo == "oportunidad") {
      fila <- if (is.null(origen)) t[[tabla]][NA_integer_, , drop = FALSE] else actual
      fila$oportunidad_id <- id
      fila$PerCod <- cliente
      fila$tipo <- .gestion_dominio(campos$tipo, c("ADQUISICION", "CROSS_SELLING", "UP_SELLING"))
      fila$estado <- estado
      if (is.null(origen)) {
        fila$usuario <- usuario
        fila$responsable <- usuario
      }
      nueva <- data.frame(oportunidad_id = id, PerCod = cliente, asunto = .gestion_campo(campos$asunto, "asunto"),
        producto = .gestion_texto(campos$producto), sacos = .gestion_numero(campos$sacos, "sacos", TRUE),
        embarque_estimado = .gestion_fecha(campos$embarque, "embarque"), pedido_id = pedido, motivo = motivo)
      t$oportunidad_detalle <- .gestion_upsert(t$oportunidad_detalle, nueva, "oportunidad_id")
    } else if (tipo == "oferta") {
      opp <- .gestion_campo(opp, "oportunidad asociada")
      fila <- data.frame(oferta_id = id, oportunidad_id = opp, PerCod = cliente,
        producto = .gestion_campo(campos$producto, "producto"), sacos = .gestion_numero(campos$sacos, "sacos", TRUE),
        valor = .gestion_numero(campos$valor, "precio o diferencial"), unidad = .gestion_campo(campos$unidad, "unidad"),
        condiciones = .gestion_texto(campos$condiciones), embarque_estimado = .gestion_fecha(campos$embarque, "embarque"),
        estado = estado, motivo = motivo, fecha = .gestion_fecha(campos$fecha, "fecha de oferta", TRUE),
        responsable = if (is.null(origen)) usuario else actual$responsable)
    } else if (tipo == "muestra") {
      fila <- data.frame(muestra_id = id, PerCod = cliente, producto = .gestion_campo(campos$producto, "producto"),
        calidad = .gestion_texto(campos$calidad), fecha = .gestion_fecha(campos$fecha, "fecha de solicitud", TRUE),
        estado = estado, pedido_id = pedido, resultado = .gestion_texto(campos$resultado),
        responsable = if (is.null(origen)) usuario else actual$responsable, actualizada = ahora)
    } else if (tipo == "tarea") {
      fila <- data.frame(tarea_id = id, PerCod = cliente,
        tipo = .gestion_dominio(campos$tipo, c("LLAMADA", "REUNION", "COMPROMISO", "SEGUIMIENTO", "NOTA")),
        asunto = .gestion_campo(campos$asunto, "asunto"), resultado = .gestion_texto(campos$resultado),
        fecha = .gestion_fecha(campos$fecha, "fecha de actividad", TRUE),
        seguimiento = .gestion_fecha(campos$seguimiento, "fecha de seguimiento",
                                      requerida = estado == "PENDIENTE"),
        estado = estado, oportunidad_id = opp, muestra_id = muestra,
        responsable = if (is.null(origen)) usuario else actual$responsable, actualizada = ahora)
    } else {
      fila <- data.frame(contacto_id = id, PerCod = cliente, nombre = .gestion_campo(campos$asunto, "nombre"),
        correo = .gestion_texto(campos$correo), telefono = .gestion_texto(campos$telefono),
        preferencia_calidad = .gestion_texto(campos$calidad))
    }
    if (tipo == "tarea") {
      prioridad <- .gestion_texto(campos$prioridad)
      if (is.na(prioridad)) prioridad <- "MEDIA"
      if (prioridad == "ALTA") prioridad <- "IMPORTANTE"
      prioridad <- .gestion_dominio(prioridad, names(.colores_prioridad_crm))
      t$tarea_prioridad <- .gestion_upsert(t$tarea_prioridad,
                                          data.frame(tarea_id = id, PerCod = cliente, prioridad = prioridad), "tarea_id")
      etiquetas <- unique(trimws(as.character(campos$etiquetas)))
      etiquetas <- etiquetas[!is.na(etiquetas) & nzchar(etiquetas)]
      if (length(etiquetas)) {
        t$tarea_etiquetas <- rbind(t$tarea_etiquetas,
                                  data.frame(tarea_id = id, PerCod = cliente, etiqueta = etiquetas))
      }
      actividades <- unique(trimws(as.character(campos$actividades)))
      actividades <- actividades[!is.na(actividades) & nzchar(actividades)]
      if (length(actividades)) {
        t$tarea_actividades <- rbind(t$tarea_actividades,
          data.frame(tarea_id = id, PerCod = cliente, orden = seq_along(actividades), actividad = actividades,
                     cumplida = actividades %in% campos$actividades_cumplidas))
      }
    }
    t[[tabla]] <- .gestion_upsert(t[[tabla]], fila, llave)
    t
  }, usuario, motivo, id)
}


# Comparación descriptiva de períodos consecutivos ----

.gestion_caidas <- function(pedidos, corte, cobertura = FALSE) {
  if (is.null(pedidos) || length(corte) != 1L) return(NULL)
  if (!nrow(pedidos)) return(data.frame())
  corte <- as.Date(corte)
  reciente <- corte - 89
  anterior <- corte - 179
  if (!cobertura && !any(pedidos$fecha_creacion <= anterior, na.rm = TRUE)) return(NULL)
  base <- pedidos[which(pedidos$fecha_creacion >= anterior & pedidos$fecha_creacion <= corte), , drop = FALSE]
  grupos <- split(base, base$PerCod)
  filas <- lapply(grupos, function(x) {
    previo <- .dp_suma(x$sacos[x$fecha_creacion < reciente])
    actual <- .dp_suma(x$sacos[x$fecha_creacion >= reciente])
    if (!is.finite(previo) || !is.finite(actual) || previo <= 0 || actual >= previo) return(NULL)
    data.frame(PerCod = x$PerCod[1L], SacosPrevios = previo, SacosRecientes = actual,
               Diferencia = actual - previo, VariacionPct = 100 * (actual - previo) / previo)
  })
  salida <- do.call(rbind, filas)
  if (is.null(salida)) return(data.frame())
  salida[order(salida$Diferencia), , drop = FALSE]
}
