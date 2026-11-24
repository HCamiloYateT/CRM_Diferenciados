# Contratos y rutas ----

.rds_nombre <- function(nombre) {
  if (!is.character(nombre) || length(nombre) != 1L || is.na(nombre) ||
      !grepl("^[a-z][a-z0-9_]*$", nombre)) {
    stop("Nombre de RDS inválido.", call. = FALSE)
  }
  nombre
}

.rds_directorio <- function(ruta, raiz) {
  enlace <- Sys.readlink(ruta)
  if (!is.na(enlace) && nzchar(enlace)) {
    stop("La persistencia requiere directorios internos sin enlaces simbólicos.", call. = FALSE)
  }
  if (!dir.exists(ruta)) dir.create(ruta, showWarnings = FALSE, mode = "0700")
  normalizada <- normalizePath(ruta, winslash = "/", mustWork = TRUE)
  if (!startsWith(normalizada, paste0(raiz, "/"))) stop("Ruta de datos fuera del proyecto.", call. = FALSE)
  normalizada
}

AbrirPersistenciaRDS <- function(raiz, contratos, ruta = Sys.getenv("CRM_DATOS_DIR", "data")) {
  raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  partes <- strsplit(ruta, "/", fixed = TRUE)[[1L]]
  if (!length(partes) || grepl("^/|/$", ruta) || any(!grepl("^[A-Za-z0-9_-]+$", partes))) {
    stop("CRM_DATOS_DIR debe ser una ruta relativa interna al proyecto.", call. = FALSE)
  }
  if (!is.list(contratos) || !length(contratos) || is.null(names(contratos)) ||
      anyDuplicated(names(contratos)) || !all(names(contratos) %in% c("crm", "operacional"))) {
    stop("Declare los contratos por dominio crm u operacional.", call. = FALSE)
  }
  for (dominio in contratos) {
    if (!is.list(dominio) || !length(dominio) || is.null(names(dominio)) || anyDuplicated(names(dominio))) {
      stop("Cada dominio requiere contratos con nombres únicos.", call. = FALSE)
    }
    for (nombre in names(dominio)) {
      .rds_nombre(nombre)
      if (identical(nombre, "manifiesto")) stop("Nombre de tabla reservado.", call. = FALSE)
      contrato <- dominio[[nombre]]
      .rds_nombre(contrato$version)
      if (!is.data.frame(contrato$prototipo) || nrow(contrato$prototipo) ||
          !length(contrato$clave) || anyDuplicated(names(contrato$prototipo)) ||
          !all(c(contrato$clave, contrato$requeridos) %in% names(contrato$prototipo)) ||
          any(names(contrato$prototipo) %in% c(".fila_anterior", ".fila_posterior"))) {
        stop("Contrato inválido para ", nombre, ".", call. = FALSE)
      }
    }
  }
  datos <- raiz
  for (parte in partes) datos <- .rds_directorio(file.path(datos, parte), raiz)
  list(raiz = raiz, datos = datos, contratos = contratos)
}

ValidarEsquemaRDS <- function(datos, contrato) {
  prototipo <- contrato$prototipo
  if (!is.data.frame(datos) || !identical(names(datos), names(prototipo)) ||
      !identical(lapply(datos, class), lapply(prototipo, class)) ||
      !identical(vapply(datos, typeof, ""), vapply(prototipo, typeof, ""))) {
    stop("Columnas o tipos incompatibles con el contrato RDS.", call. = FALSE)
  }
  requeridos <- unique(c(contrato$clave, contrato$requeridos))
  if (anyNA(datos[requeridos]) || anyDuplicated(datos[contrato$clave])) {
    stop("El RDS contiene una clave duplicada o un valor requerido ausente.", call. = FALSE)
  }
  if (!is.null(contrato$validar)) contrato$validar(datos)
  invisible(TRUE)
}

.rds_validar_corte <- function(tablas, contratos) {
  if (anyDuplicated(names(tablas)) || !setequal(names(tablas), names(contratos))) {
    stop("El corte debe contener todas las tablas contratadas.", call. = FALSE)
  }
  for (nombre in names(contratos)) {
    contrato <- contratos[[nombre]]
    datos <- tablas[[nombre]]
    ValidarEsquemaRDS(datos, contrato)
    for (ref in contrato$referencias) {
      destino <- tablas[[ref$tabla]]
      clave <- contratos[[ref$tabla]]$clave
      if (is.null(destino) || !length(clave) || length(clave) != length(ref$columnas)) {
        stop("Referencia RDS inválida en ", nombre, ".", call. = FALSE)
      }
      local <- datos[ref$columnas]
      presente <- !is.na(local)
      completo <- rowSums(presente) == length(clave)
      if (any(rowSums(presente) > 0L & !completo)) {
        stop("Referencia compuesta parcial en ", nombre, ".", call. = FALSE)
      }
      local <- local[completo, , drop = FALSE]
      names(local) <- clave
      coincidencias <- merge(local, destino[clave], by = clave)
      if (nrow(coincidencias) != nrow(local)) stop("Referencia sin destino en ", nombre, ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Generaciones y lectura ----

.rds_dominio <- function(almacen, dominio) {
  .rds_nombre(dominio)
  if (!dominio %in% names(almacen$contratos)) stop("Dominio RDS no contratado.", call. = FALSE)
  .rds_directorio(file.path(almacen$datos, dominio), almacen$raiz)
}

.rds_id_generacion <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^gen-[A-Za-z0-9]+$", id)) {
    stop("Identificador de generación inválido.", call. = FALSE)
  }
  id
}

.rds_verificar_archivos <- function(carpeta, id) {
  manifiesto <- readRDS(file.path(carpeta, "manifiesto.rds"))
  if (!identical(manifiesto$formato, 1L) || !identical(manifiesto$id, id)) {
    stop(structure(list(message = "Formato de publicación incompatible."),
                   class = c("esquema_publicacion", "error", "condition")))
  }
  nombres <- names(manifiesto$versiones)
  if (!length(nombres) || anyDuplicated(nombres) || !all(vapply(nombres, .rds_nombre, "") == nombres)) {
    stop("Nombres de publicación inválidos.", call. = FALSE)
  }
  archivos <- c(paste0(nombres, ".rds"), "_auditoria.rds")
  firmas <- unname(tools::md5sum(file.path(carpeta, archivos)))
  if (!identical(names(manifiesto$firmas), archivos) || anyNA(firmas) ||
      !identical(firmas, unname(manifiesto$firmas))) {
    stop("Generación RDS incompleta o alterada.", call. = FALSE)
  }
  manifiesto
}

.rds_cargar <- function(ruta, contratos, id) {
  id <- .rds_id_generacion(id)
  carpeta <- file.path(ruta, id)
  manifiesto <- .rds_verificar_archivos(carpeta, id)
  versiones <- vapply(contratos, function(contrato) contrato$version, "")
  if (!identical(manifiesto$versiones, versiones)) stop("Manifiesto RDS incompatible.", call. = FALSE)
  archivos <- names(manifiesto$firmas)
  tablas <- setNames(lapply(file.path(carpeta, archivos[-length(archivos)]), readRDS), names(contratos))
  .rds_validar_corte(tablas, contratos)
  auditoria <- readRDS(file.path(carpeta, "_auditoria.rds"))
  if (!is.list(auditoria)) stop("Auditoría RDS inválida.", call. = FALSE)
  list(manifiesto = manifiesto, tablas = tablas, auditoria = auditoria)
}

.rds_vigente <- function(ruta, contratos) {
  puntero <- file.path(ruta, "vigente.rds")
  if (file.exists(puntero)) return(.rds_cargar(ruta, contratos, readRDS(puntero)))
  if (length(list.files(ruta, pattern = "^gen-"))) {
    stop("Falta el puntero vigente. Revise las generaciones para recuperar el RDS.", call. = FALSE)
  }
  list(manifiesto = NULL, tablas = lapply(contratos, function(x) x$prototipo), auditoria = list())
}

LeerCorteRDS <- function(almacen, dominio) {
  ruta <- .rds_dominio(almacen, dominio)
  .rds_vigente(ruta, almacen$contratos[[dominio]])
}

LeerRDS <- function(almacen, nombre, dominio) {
  .rds_nombre(nombre)
  corte <- LeerCorteRDS(almacen, dominio)
  if (!nombre %in% names(corte$tablas)) stop("RDS no contratado.", call. = FALSE)
  corte$tablas[[nombre]]
}

# Escrituras y auditoría ----

.rds_bloquear <- function(ruta) {
  bloqueo <- file.path(ruta, ".escritura-activa")
  # La creación del directorio es exclusiva. Si un proceso termina abruptamente, revisar el bloqueo antes de retirarlo.
  if (!dir.create(bloqueo, mode = "0700", showWarnings = FALSE)) {
    stop("No se pudo adquirir el bloqueo de escritura en ", bloqueo, ".", call. = FALSE)
  }
  bloqueo
}

.rds_metadatos <- function(usuario, motivo, referencia) {
  metadatos <- list(usuario = usuario, motivo = motivo, referencia = referencia)
  campos <- c("usuario", "motivo", "referencia")
  invalidos <- vapply(metadatos[campos], function(x) {
    !is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))
  }, logical(1))
  if (any(invalidos)) stop("Indique usuario, motivo y referencia de auditoría.", call. = FALSE)
  metadatos[campos]
}

.rds_auditar <- function(antes, despues, contratos, metadatos) {
  eventos <- list()
  fecha <- Sys.time()
  for (nombre in names(contratos)) {
    previo <- antes[[nombre]]
    nuevo <- despues[[nombre]]
    if (identical(previo, nuevo)) next
    clave <- contratos[[nombre]]$clave
    anterior <- previo[clave]
    posterior <- nuevo[clave]
    anterior[[".fila_anterior"]] <- seq_len(nrow(previo))
    posterior[[".fila_posterior"]] <- seq_len(nrow(nuevo))
    cruces <- merge(anterior, posterior, by = clave, all = TRUE, sort = FALSE)
    historial <- isTRUE(contratos[[nombre]]$solo_anexar)
    for (i in seq_len(nrow(cruces))) {
      indice_anterior <- cruces$.fila_anterior[i]
      indice_posterior <- cruces$.fila_posterior[i]
      viejo <- if (!is.na(indice_anterior)) as.list(previo[indice_anterior, , drop = FALSE])
      actual <- if (!is.na(indice_posterior)) as.list(nuevo[indice_posterior, , drop = FALSE])
      if (identical(viejo, actual)) next
      if (historial && !is.null(viejo)) stop("El historial solo admite anexar eventos.", call. = FALSE)
      operacion <- if (is.null(viejo)) "alta" else if (is.null(actual)) "baja" else "modificacion"
      eventos[[length(eventos) + 1L]] <- c(metadatos, list(
        fecha = fecha, tabla = nombre, clave = as.list(cruces[i, clave, drop = FALSE]),
        operacion = operacion, anterior = viejo, nuevo = actual
      ))
    }
  }
  eventos
}

.rds_publicar <- function(ruta, contratos, tablas, auditoria, anterior, procedencia) {
  if (.Platform$OS.type != "unix") stop("La publicación RDS requiere reemplazo atómico en Unix.", call. = FALSE)
  carpeta <- tempfile("gen-", tmpdir = ruta)
  id <- basename(carpeta)
  if (!dir.create(carpeta, mode = "0700")) stop("No se pudo crear una generación RDS.", call. = FALSE)
  puntero_tmp <- tempfile("puntero-", tmpdir = ruta)
  publicado <- FALSE
  on.exit({
    unlink(puntero_tmp)
    if (!publicado) unlink(carpeta, recursive = TRUE)
  })
  archivos <- c(paste0(names(contratos), ".rds"), "_auditoria.rds")
  destinos <- file.path(carpeta, archivos)
  for (nombre in names(contratos)) saveRDS(tablas[[nombre]], file.path(carpeta, paste0(nombre, ".rds")))
  saveRDS(auditoria, tail(destinos, 1L))
  versiones <- vapply(contratos, function(contrato) contrato$version, "")
  manifiesto <- list(formato = 1L, id = id, anterior = anterior, creado_en = Sys.time(),
                     versiones = versiones, procedencia = procedencia,
                     firmas = setNames(unname(tools::md5sum(destinos)), archivos))
  saveRDS(manifiesto, file.path(carpeta, "manifiesto.rds"))
  verificado <- .rds_cargar(ruta, contratos, id)
  if (!identical(verificado$tablas, tablas[names(contratos)]) || !identical(verificado$auditoria, auditoria)) {
    stop("La relectura de la generación RDS no coincide.", call. = FALSE)
  }
  saveRDS(id, puntero_tmp)
  suspendInterrupts({
    if (!identical(readRDS(puntero_tmp), id) || !file.rename(puntero_tmp, file.path(ruta, "vigente.rds"))) {
      stop("No se pudo publicar la generación RDS.", call. = FALSE)
    }
    publicado <- TRUE
  })
  invisible(manifiesto)
}

ActualizarCorteRDS <- function(almacen, dominio, funcion, usuario = NULL, motivo = NULL,
                               referencia = NULL, procedencia = NULL, ampliar = FALSE) {
  ruta <- .rds_dominio(almacen, dominio)
  bloqueo <- .rds_bloquear(ruta)
  on.exit(unlink(bloqueo, recursive = TRUE))
  contratos <- almacen$contratos[[dominio]]
  disponibles <- contratos
  puntero <- file.path(ruta, "vigente.rds")
  if (ampliar && file.exists(puntero)) {
    id <- .rds_id_generacion(readRDS(puntero))
    versiones <- .rds_verificar_archivos(file.path(ruta, id), id)$versiones
    if (!all(names(versiones) %in% names(contratos))) stop("El corte CRM tiene tablas sin contrato.")
    disponibles <- contratos[names(versiones)]
  }
  vigente <- .rds_vigente(ruta, disponibles)
  if (ampliar) {
    nuevos <- setdiff(names(contratos), names(vigente$tablas))
    vigente$tablas[nuevos] <- lapply(contratos[nuevos], function(x) x$prototipo)
  }
  tablas <- funcion(vigente$tablas)
  .rds_validar_corte(tablas, contratos)
  auditoria <- vigente$auditoria
  if (identical(dominio, "crm")) {
    metadatos <- .rds_metadatos(usuario, motivo, referencia)
    auditoria <- c(auditoria, .rds_auditar(vigente$tablas, tablas, contratos, metadatos))
  }
  .rds_publicar(ruta, contratos, tablas, auditoria, vigente$manifiesto$id, procedencia)
}

ActualizarRDS <- function(almacen, nombre, dominio, funcion, ...) {
  .rds_nombre(nombre)
  ActualizarCorteRDS(almacen, dominio, function(tablas) {
    if (!nombre %in% names(tablas)) stop("RDS no contratado.", call. = FALSE)
    tablas[[nombre]] <- funcion(tablas[[nombre]])
    tablas
  }, ...)
}

GuardarRDS <- function(almacen, datos, nombre, dominio, ...) {
  ActualizarRDS(almacen, nombre, dominio, function(anterior) datos, ...)
}

LeerBackupRDS <- function(almacen, dominio, generacion) {
  ruta <- .rds_dominio(almacen, dominio)
  .rds_cargar(ruta, almacen$contratos[[dominio]], generacion)
}

# Recuperación explícita desde una generación verificada ----

RecuperarCorteRDS <- function(almacen, dominio, generacion, usuario, motivo, referencia) {
  ruta <- .rds_dominio(almacen, dominio)
  bloqueo <- .rds_bloquear(ruta)
  on.exit(unlink(bloqueo, recursive = TRUE))
  contratos <- almacen$contratos[[dominio]]
  vigente <- tryCatch(.rds_vigente(ruta, contratos), error = identity)
  if (!inherits(vigente, "error")) stop("El corte vigente es legible. Use una actualización auditada.", call. = FALSE)
  respaldo <- .rds_cargar(ruta, contratos, generacion)
  metadatos <- .rds_metadatos(usuario, motivo, referencia)
  evento <- c(metadatos, list(fecha = Sys.time(), operacion = "recuperacion", generacion = generacion))
  .rds_publicar(ruta, contratos, respaldo$tablas, c(respaldo$auditoria, list(evento)), generacion,
                respaldo$manifiesto$procedencia)
}

# Lectura de una publicación para consumidores con contratos propios ----

LeerPublicacionRDS <- function(raiz, dominio, ruta = Sys.getenv("CRM_DATOS_DIR", "data")) {
  if (!dominio %in% c("operacional", "crm")) stop("Dominio de publicación inválido.", call. = FALSE)
  partes <- strsplit(ruta, "/", fixed = TRUE)[[1L]]
  if (!length(partes) || grepl("^/|/$", ruta) || any(!grepl("^[A-Za-z0-9_-]+$", partes))) {
    stop("CRM_DATOS_DIR debe ser una ruta relativa interna al proyecto.", call. = FALSE)
  }
  carpeta <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  for (parte in c(partes, dominio)) {
    carpeta <- file.path(carpeta, parte)
    enlace <- Sys.readlink(carpeta)
    if (!is.na(enlace) && nzchar(enlace)) stop("Directorio de publicación inválido.", call. = FALSE)
  }
  puntero <- file.path(carpeta, "vigente.rds")
  if (!file.exists(puntero)) {
    if (length(list.files(carpeta, pattern = "^gen-"))) stop("Falta el puntero vigente.", call. = FALSE)
    return(NULL)
  }
  id <- .rds_id_generacion(readRDS(puntero))
  generacion <- file.path(carpeta, id)
  manifiesto <- .rds_verificar_archivos(generacion, id)
  nombres <- names(manifiesto$versiones)
  tablas <- setNames(lapply(file.path(generacion, paste0(nombres, ".rds")), readRDS), nombres)
  list(tablas = tablas, manifiesto = manifiesto)
}

# Adaptadores y validaciones ----

.dp_col <- function(datos, nombre) {
  if (nombre %in% names(datos)) return(datos[[nombre]])
  rep(NA_character_, nrow(datos))
}

.dp_clave <- function(datos, columnas, fuente) {
  clave <- datos[columnas]
  invalida <- Reduce(`|`, lapply(clave, function(x) is.na(x) | x == "" | trimws(x) != x))
  if (any(invalida)) stop("Identificadores vacíos o con espacios externos en ", fuente, ".", call. = FALSE)
  if (anyDuplicated(clave)) stop("Clave duplicada en ", fuente, ".", call. = FALSE)
}

.dp_id_pedido <- function(compania, pedido) {
  if (!length(compania)) return(character())
  paste0(nchar(compania, type = "bytes"), ":", compania, pedido)
}

.dp_numero <- function(valor) {
  valido <- !is.na(valor) & grepl("^[-+]?[0-9]+([.][0-9]+)?$", valor)
  convertido <- rep(NA_real_, length(valor))
  convertido[valido] <- suppressWarnings(as.numeric(valor[valido]))
  convertido[!is.finite(convertido)] <- NA_real_
  convertido
}

.dp_fecha <- function(valor) {
  # Se conserva el día fuente y por separado el valor original, incluida la hora si existe.
  patron <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}([ T]([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]([.][0-9]+)?)?$"
  dia <- ifelse(!is.na(valor) & grepl(patron, valor), substr(valor, 1L, 10L), NA_character_)
  fecha <- as.Date(as.character(dia), format = "%Y-%m-%d")
  invalida <- is.na(fecha) | format(fecha, "%Y-%m-%d") != dia | dia == "1753-01-01"
  fecha[invalida] <- NA
  fecha
}

.dp_periodo <- function(anio, mes) {
  valido <- !is.na(anio) & grepl("^[1-9][0-9]{3}$", anio) & !is.na(mes) & grepl("^(0?[1-9]|1[0-2])$", mes)
  periodo <- rep(NA_character_, length(anio))
  periodo[valido] <- sprintf("%s-%02d", anio[valido], as.integer(mes[valido]))
  periodo
}

.dp_suma <- function(x) {
  if (requireNamespace("racafeCore", quietly = TRUE) && "SumaEstricta" %in% getNamespaceExports("racafeCore")) {
    return(racafeCore::SumaEstricta(x))
  }
  if (anyNA(x)) NA_real_ else sum(x)
}

.dp_agregar <- function(clave, cantidad, universo) {
  grupos <- split(cantidad, clave)
  totales <- vapply(grupos, .dp_suma, numeric(1))
  unname(totales[match(universo, names(totales))])
}

.dp_incidencia <- function(clave, condicion, codigo) {
  ids <- clave[which(condicion)]
  data.frame(clave = ids, codigo = rep(codigo, length(ids)))
}

# Normalización de hechos ----

.dp_pedidos <- function(raw_pedidos, raw_lineas, raw_clientes, raw_unidades, corte) {
  .dp_clave(raw_pedidos, c("CiaCod", "PdcCod"), "pedidos")
  .dp_clave(raw_lineas, c("CiaCod", "PdcCod", "PdcLin"), "lineas")
  .dp_clave(raw_clientes, "PerCod", "clientes")
  if (anyNA(raw_lineas$LinNegCod) || any(!grepl("^[0-9]+$", raw_lineas$LinNegCod))) {
    stop("El código original de línea falta o es inválido en lineas.csv.", call. = FALSE)
  }
  cabecera_id <- .dp_id_pedido(raw_pedidos$CiaCod, raw_pedidos$PdcCod)
  lineas <- raw_lineas[raw_lineas$LinNegCod == "20000", , drop = FALSE]
  linea_id <- .dp_id_pedido(lineas$CiaCod, lineas$PdcCod)
  if (any(!linea_id %in% cabecera_id)) stop("Hay líneas 20000 sin cabecera.", call. = FALSE)
  cab <- raw_pedidos[cabecera_id %in% linea_id, , drop = FALSE]
  pedido_id <- cabecera_id[cabecera_id %in% linea_id]
  if (anyNA(cab$CliNit) || any(!cab$CliNit %in% raw_clientes$PerCod)) {
    stop("Hay pedidos 20000 sin PerCod verificable en clientes.csv.", call. = FALSE)
  }
  fecha <- .dp_fecha(cab$PdcFecCre)
  if (any(fecha > corte, na.rm = TRUE)) stop("Hay pedidos 20000 creados después del corte.", call. = FALSE)
  periodo <- .dp_periodo(.dp_col(cab, "PdcAnoEmb"), .dp_col(cab, "PdcMesEmb"))
  fact_pedidos <- data.frame(pedido_id, CiaCod = cab$CiaCod, PdcCod = cab$PdcCod, PerCod = cab$CliNit,
                            fecha_creacion = fecha, fecha_creacion_original = cab$PdcFecCre,
                            estado_original = .dp_col(cab, "PdcEst"), periodo_cabecera = periodo)
  unidad <- .dp_col(lineas, "UMeCod")
  falta_unidad <- is.na(unidad)
  unidad[falta_unidad] <- .dp_col(cab, "UMeCod")[match(linea_id[falta_unidad], pedido_id)]
  origen_unidad <- ifelse(is.na(unidad), NA_character_, ifelse(falta_unidad, "cabecera", "linea"))
  cantidad <- .dp_numero(lineas$PdcCan)
  factor_kg <- factor_sacos <- rep(NA_real_, length(unidad))
  if (is.null(raw_unidades)) raw_unidades <- data.frame(UMeCod = character())
  .dp_clave(raw_unidades, "UMeCod", "unidades")
  dim_unidades <- data.frame(UMeCod = raw_unidades$UMeCod,
                             factor_kg = .dp_numero(.dp_col(raw_unidades, "KgPorUnidad")),
                             factor_sacos = .dp_numero(.dp_col(raw_unidades, "SacosPorUnidad")))
  if (nrow(dim_unidades)) {
    indice <- match(unidad, raw_unidades$UMeCod)
    factor_kg <- dim_unidades$factor_kg[indice]
    factor_sacos <- dim_unidades$factor_sacos[indice]
    if (any(c(factor_kg, factor_sacos) <= 0, na.rm = TRUE)) stop("Factor de unidad no positivo.", call. = FALSE)
  }
  kg <- cantidad * factor_kg
  sacos <- cantidad * factor_sacos
  fecha_embarque <- .dp_col(lineas, "PdcFecEmb")
  valor <- .dp_numero(.dp_col(lineas, "ValorLinea"))
  moneda <- .dp_col(lineas, "MonedaLinea")
  valor[is.na(moneda) | !grepl("^[A-Z]{3}$", moneda)] <- NA_real_
  moneda[is.na(valor)] <- NA_character_
  fact_lineas <- data.frame(pedido_id = linea_id, PdcLin = lineas$PdcLin, LinNegCod = lineas$LinNegCod,
                           cantidad_original = lineas$PdcCan, cantidad, UMeCod = unidad,
                           origen_unidad = as.character(origen_unidad), factor_kg, factor_sacos, kg, sacos,
                           valor, moneda,
                           fecha_embarque_original = fecha_embarque, fecha_embarque_linea = .dp_fecha(fecha_embarque),
                           LinProCod = .dp_col(lineas, "LinProCod"), MCCod = .dp_col(lineas, "MCCod"),
                           MrcCod = .dp_col(lineas, "MrcCod"), PaiCodDtn = .dp_col(lineas, "PaiCodDtn"),
                           PaiCodOri = .dp_col(lineas, "PaiCodOri"), PueCodDtn = .dp_col(lineas, "PueCodDtn"),
                           PueCodOri = .dp_col(lineas, "PueCodOri"),
                           categoria_homologada = rep(NA_character_, nrow(lineas)),
                           producto_homologado = rep(NA_character_, nrow(lineas)))
  for (campo in c("ForPagCod", "ForPagNom")) fact_pedidos[[campo]] <- .dp_col(cab, campo)
  etiquetas <- c("LinNegNom", "LinProNom", "MrcNom", "MCNom", "PaisDtn", "ISO2Dtn", "PuertoDtn",
                 "PaisOri", "ISO2Ori", "PuertoOri")
  for (campo in etiquetas) fact_lineas[[campo]] <- .dp_col(lineas, campo)
  fact_pedidos$kg <- .dp_agregar(linea_id, kg, pedido_id)
  fact_pedidos$sacos <- .dp_agregar(linea_id, sacos, pedido_id)
  monedas <- split(moneda, linea_id)
  moneda_pedido <- vapply(monedas, function(x) {
    if (anyNA(x) || length(unique(x)) != 1L) NA_character_ else x[[1L]]
  }, character(1))
  fact_pedidos$moneda <- unname(moneda_pedido[match(pedido_id, names(moneda_pedido))])
  fact_pedidos$valor <- .dp_agregar(linea_id, valor, pedido_id)
  fact_pedidos$valor[is.na(fact_pedidos$moneda)] <- NA_real_
  dim_clientes <- data.frame(PerCod = raw_clientes$PerCod, PerRazSoc = .dp_col(raw_clientes, "PerRazSoc"),
                            codigo_pais = .dp_col(raw_clientes, "PaiCod"), pais = .dp_col(raw_clientes, "Pais"),
                            iso2 = .dp_col(raw_clientes, "ISO2"))
  calidad <- rbind(
    .dp_incidencia(pedido_id, is.na(fecha), "creacion_sin_fecha_valida"),
    .dp_incidencia(pedido_id, substr(cab$PdcFecCre, 1L, 10L) == "1753-01-01", "creacion_centinela"),
    .dp_incidencia(pedido_id, is.na(periodo) & (!is.na(.dp_col(cab, "PdcAnoEmb")) |
                                               !is.na(.dp_col(cab, "PdcMesEmb"))), "periodo_cabecera_invalido"),
    .dp_incidencia(paste(linea_id, lineas$PdcLin, sep = "/"), is.na(cantidad), "cantidad_sin_numero_valido"),
    .dp_incidencia(paste(linea_id, lineas$PdcLin, sep = "/"), is.na(sacos), "sacos_no_verificables"),
    .dp_incidencia(paste(linea_id, lineas$PdcLin, sep = "/"),
                   !is.na(.dp_col(lineas, "ValorLinea")) & is.na(valor), "valor_no_verificable"),
    .dp_incidencia(paste(linea_id, lineas$PdcLin, sep = "/"),
                   !is.na(fecha_embarque) & is.na(fact_lineas$fecha_embarque_linea), "fecha_embarque_no_valida")
  )
  list(clientes = dim_clientes, unidades = dim_unidades, pedidos = fact_pedidos, lineas_pedido = fact_lineas,
       raw_clientes = raw_clientes, raw_pedidos = cab, raw_lineas = lineas, raw_unidades = raw_unidades,
       calidad = calidad)
}

# Analítica y contratos de salida ----

.dp_analitica <- function(tablas, corte) {
  pedidos <- tablas$pedidos
  candidatos <- pedidos[!is.na(pedidos$fecha_creacion), , drop = FALSE]
  candidatos <- candidatos[order(candidatos$fecha_creacion, candidatos$pedido_id, method = "radix"), , drop = FALSE]
  incompletos <- pedidos$PerCod[is.na(pedidos$fecha_creacion)]
  primero <- candidatos[!duplicated(candidatos$PerCod) & !candidatos$PerCod %in% incompletos,
                       c("PerCod", "pedido_id", "fecha_creacion"), drop = FALSE]
  primero$cohorte <- format(primero$fecha_creacion, "%Y-%m")
  primero$antiguedad_dias <- as.integer(corte - primero$fecha_creacion)
  tablas$primer_pedido_cliente <- primero
  tablas$cohortes_clientes <- primero[c("PerCod", "cohorte", "fecha_creacion", "pedido_id")]
  cohortes <- table(primero$cohorte)
  tablas$cohortes_mensuales <- data.frame(periodo = as.character(names(cohortes)), clientes = as.integer(cohortes))
  tablas$clientes_comerciales <- merge(tablas$clientes, primero, by = "PerCod", all.x = TRUE, sort = FALSE)
  clientes <- tablas$clientes_comerciales
  clientes$tiene_pedidos <- clientes$PerCod %in% pedidos$PerCod
  clientes$fecha_primera_observable <- !is.na(clientes$fecha_creacion)
  volumen <- unique(pedidos["PerCod"])
  volumen$sacos <- .dp_agregar(pedidos$PerCod, pedidos$sacos, volumen$PerCod)
  volumen$kg <- .dp_agregar(pedidos$PerCod, pedidos$kg, volumen$PerCod)
  volumen$pedidos <- as.integer(table(pedidos$PerCod)[volumen$PerCod])
  tablas$volumen_cliente <- volumen
  ultima <- tapply(as.numeric(candidatos$fecha_creacion), candidatos$PerCod, max)
  indices <- match(clientes$PerCod, names(ultima))
  clientes$ultimo_pedido <- as.Date(as.numeric(unname(ultima)[indices]), origin = "1970-01-01")
  clientes$pedidos_del_universo <- as.integer(table(pedidos$PerCod)[clientes$PerCod])
  clientes$sacos_del_universo <- .dp_agregar(pedidos$PerCod, pedidos$sacos, clientes$PerCod)
  tablas$clientes_comerciales <- clientes
  mes <- format(candidatos$fecha_creacion, "%Y-%m")
  periodos <- unique(mes)
  tablas$pedidos_por_periodo <- data.frame(periodo = periodos, pedidos = as.integer(table(mes)[periodos]),
                                          sacos = .dp_agregar(mes, candidatos$sacos, periodos))
  # Referencias candidatas sin precedencia ni evaluación de cumplimiento.
  lineas <- tablas$lineas_pedido
  cab <- pedidos[!is.na(pedidos$periodo_cabecera), c("pedido_id", "periodo_cabecera"), drop = FALSE]
  det <- lineas[!is.na(lineas$fecha_embarque_linea), c("pedido_id", "PdcLin", "fecha_embarque_linea"), drop = FALSE]
  tablas$referencias_embarque <- rbind(
    data.frame(pedido_id = cab$pedido_id, referencia = rep("cabecera", nrow(cab)),
               origen = rep("cabecera", nrow(cab)), periodo = cab$periodo_cabecera,
               fecha = rep(as.Date(NA_character_), nrow(cab))),
    data.frame(pedido_id = det$pedido_id, referencia = if (nrow(det)) paste0("linea:", det$PdcLin) else character(),
               origen = rep("linea", nrow(det)), periodo = rep(NA_character_, nrow(det)),
               fecha = det$fecha_embarque_linea)
  )
  tablas$jerarquia_cuentas <- data.frame(PerCod = character(), cuenta_padre = character(), desde = as.Date(character()))
  tablas
}

ContratosOperacionales <- function(tablas) {
  claves <- list(clientes = "PerCod", unidades = "UMeCod", pedidos = "pedido_id",
                 lineas_pedido = c("pedido_id", "PdcLin"),
                 primer_pedido_cliente = "PerCod", cohortes_clientes = "PerCod", clientes_comerciales = "PerCod",
                 volumen_cliente = "PerCod", pedidos_por_periodo = "periodo", cohortes_mensuales = "periodo",
                 hechos_productos = c("pedido_id", "PdcLin"), dim_marcas = "item", dim_nombres_comerciales = "item",
                 relacion_marca_nombre = c("pedido_id", "PdcLin"), pedidos_por_destino = c("periodo", "pais"),
                 jerarquia_cuentas = c("PerCod", "desde"), paises = "iso2", homologaciones = "fuente",
                 referencias_embarque = c("pedido_id", "referencia"),
                 calidad = "incidencia")
  contratos <- lapply(names(tablas), function(nombre) {
    clave <- claves[[nombre]]
    if (startsWith(nombre, "raw_")) clave <- "fuente"
    list(version = "v1", prototipo = tablas[[nombre]][FALSE, , drop = FALSE], clave = clave)
  })
  names(contratos) <- names(tablas)
  contratos$pedidos$referencias <- list(list(tabla = "clientes", columnas = "PerCod"))
  contratos$lineas_pedido$referencias <- list(list(tabla = "pedidos", columnas = "pedido_id"))
  contratos$referencias_embarque$referencias <- list(list(tabla = "pedidos", columnas = "pedido_id"))
  contratos$primer_pedido_cliente$referencias <- list(list(tabla = "clientes", columnas = "PerCod"),
                                                     list(tabla = "pedidos", columnas = "pedido_id"))
  contratos$hechos_productos$referencias <- list(list(tabla = "lineas_pedido", columnas = c("pedido_id", "PdcLin")))
  contratos$relacion_marca_nombre$referencias <- contratos$hechos_productos$referencias
  contratos$lineas_pedido$validar <- function(datos) {
    if (anyNA(datos$LinNegCod) || any(datos$LinNegCod != "20000")) stop("Línea fuera de alcance.")
  }
  contratos
}

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
  originales <- which(fuentes_linea$fuente %in% c("EXPPEDI1", "lineas.csv"))
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

# Publicación operacional ----

PublicarOperacionalRDS <- function(raiz, tablas, contratos, procedencia) {
  # La transformación se completa antes de publicar y documenta sus exportaciones de origen.
  campos <- c("fuentes", "corte")
  if (!is.list(procedencia) || !all(campos %in% names(procedencia)) ||
      !length(procedencia$fuentes) || !length(procedencia$corte)) {
    stop("Indique las fuentes y el corte de las exportaciones operacionales.", call. = FALSE)
  }
  almacen <- AbrirPersistenciaRDS(raiz, list(operacional = contratos))
  ruta <- .rds_dominio(almacen, "operacional")
  bloqueo <- .rds_bloquear(ruta)
  on.exit(unlink(bloqueo, recursive = TRUE))
  anterior <- LeerPublicacionRDS(raiz, "operacional")
  .rds_validar_corte(tablas, contratos)
  .rds_publicar(ruta, contratos, tablas, list(), anterior$manifiesto$id, procedencia)
}

# Extracción operacional y catálogos SysCafe ----

.dp_resultado_sql <- function(consulta, sql) {
  datos <- consulta("syscafe", sql)
  if (!is.data.frame(datos) || anyDuplicated(names(datos))) {
    stop("SysCafe devolvió una estructura incompatible.", call. = FALSE)
  }
  datos[] <- lapply(datos, function(x) {
    if (!is.numeric(x) || inherits(x, c("Date", "POSIXt", "integer64"))) return(as.character(x))
    salida <- as.character(x)
    salida[!is.na(x)] <- trimws(format(x[!is.na(x)], scientific = FALSE, digits = 17L, trim = TRUE))
    salida
  })
  datos
}

.dp_catalogo_valor <- function(claves, catalogo, clave, valor) {
  grupos <- split(catalogo[[valor]], catalogo[[clave]])
  unicos <- vapply(grupos, function(x) {
    valores <- unique(x[!is.na(x) & nzchar(x)])
    if (length(valores) == 1L) valores[[1L]] else NA_character_
  }, character(1))
  unname(unicos[match(claves, names(unicos))])
}

.dp_completar <- function(datos, campo, valor) {
  actual <- .dp_col(datos, campo)
  faltan <- is.na(actual) | !nzchar(trimws(actual))
  actual[faltan] <- valor[faltan]
  datos[[campo]] <- actual
  datos
}

.dp_extraer <- function(corte, consulta = racafeBD::ConsultaSistema) {
  leer <- function(sql) .dp_resultado_sql(consulta, sql)
  fecha <- format(corte + 1, "%Y%m%d")
  limite <- paste0("(pd.PdcFecCre IS NULL OR pd.PdcFecCre < CONVERT(date, '", fecha, "', 112))")
  enlace <- "p1.CiaCod = pd.CiaCod AND p1.PdcCod = pd.PdcCod"
  pedidos <- leer(paste("SELECT pd.* FROM EXPPEDID pd WHERE", limite,
                        "AND EXISTS (SELECT 1 FROM EXPPEDI1 p1 WHERE", enlace, "AND p1.LinNegCod = 20000)"))
  lineas <- leer(paste("SELECT p1.* FROM EXPPEDI1 p1 WHERE p1.LinNegCod = 20000",
                       "AND EXISTS (SELECT 1 FROM EXPPEDID pd WHERE", enlace, "AND", paste0(limite, ")")))

  # Los catálogos pertenecen a SysCafe y no alteran la granularidad de los hechos.
  sql <- c(personas = "SELECT PerCod, PerRazSoc FROM NPERSONA",
           clientes = "SELECT CliNit, CiuExtCod FROM NCLIENTE",
           ciudades = "SELECT CiuExtCod, CiuExtNom, PaiExtCod FROM NCIUEXT",
           paises = "SELECT PaiCod, PaiNom, PaiSglDian FROM NPAISES",
           puertos = "SELECT PaiCod, PueCod, PueNom FROM NPUERTOS",
           marcas = "SELECT MrcCod, MrcNom FROM NMARCAS",
           productos = "SELECT CiaCod, LinProCod, LinProNom FROM NTIPPROD",
           negocios = "SELECT CiaCod, LinNegCod, LinNegNom FROM NLINEANE WHERE LinNegCod = 20000",
           unidades = "SELECT UMeCod, UMeNom, UMeFac, UMeSgl FROM NUNIMEDI",
           pagos = "SELECT ForPagCod, ForPagNom, ForPagFrf, ForPagPlz FROM NFORPAG")
  catalogos <- lapply(sql, leer)
  esquema <- tryCatch(names(leer("SELECT * FROM NNOMCOM WHERE 1 = 0")), error = function(e) character())
  tabla_nombres <- if (all(c("MCCod", "MCNom") %in% esquema)) "NNOMCOM" else "NMARCOM"
  catalogos$nombres <- leer(paste("SELECT MCCod, MCNom FROM", tabla_nombres))
  paises <- catalogos$paises
  paises$iso2 <- toupper(trimws(paises$PaiSglDian))
  paises$iso2[!is.na(paises$iso2) & !grepl("^[A-Z]{2}$", paises$iso2)] <- NA_character_
  ids <- unique(c(catalogos$clientes$CliNit, pedidos$CliNit))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  nombre <- .dp_catalogo_valor(ids, catalogos$personas, "PerCod", "PerRazSoc")
  ciudad <- .dp_catalogo_valor(ids, catalogos$clientes, "CliNit", "CiuExtCod")
  codigo_pais <- .dp_catalogo_valor(ciudad, catalogos$ciudades, "CiuExtCod", "PaiExtCod")
  clientes <- data.frame(PerCod = ids, PerRazSoc = nombre, CiuExtCod = ciudad, PaiCod = codigo_pais,
                          Pais = .dp_catalogo_valor(codigo_pais, paises, "PaiCod", "PaiNom"),
                          ISO2 = .dp_catalogo_valor(codigo_pais, paises, "PaiCod", "iso2"))
  pedidos <- .dp_completar(pedidos, "ForPagNom",
                           .dp_catalogo_valor(.dp_col(pedidos, "ForPagCod"), catalogos$pagos, "ForPagCod", "ForPagNom"))
  lineas <- .dp_completar(lineas, "MCCod", .dp_col(lineas, "NomComCod"))
  lineas <- .dp_completar(lineas, "MrcNom",
                          .dp_catalogo_valor(.dp_col(lineas, "MrcCod"), catalogos$marcas, "MrcCod", "MrcNom"))
  lineas <- .dp_completar(lineas, "MCNom",
                          .dp_catalogo_valor(lineas$MCCod, catalogos$nombres, "MCCod", "MCNom"))
  for (campo in c("productos", "negocios")) {
    cat <- catalogos[[campo]]
    codigo <- if (campo == "productos") "LinProCod" else "LinNegCod"
    etiqueta <- paste0(sub("Cod$", "", codigo), "Nom")
    cat$llave <- .prod_id(cat$CiaCod, cat[[codigo]])
    llave <- .prod_id(lineas$CiaCod, .dp_col(lineas, codigo))
    lineas <- .dp_completar(lineas, etiqueta, .dp_catalogo_valor(llave, cat, "llave", etiqueta))
  }
  puertos <- catalogos$puertos
  puertos$llave <- .prod_id(puertos$PaiCod, puertos$PueCod)
  for (lado in c("Ori", "Dtn")) {
    pais <- .dp_col(lineas, paste0("PaiCod", lado))
    puerto <- .dp_col(lineas, paste0("PueCod", lado))
    lineas <- .dp_completar(lineas, paste0("Pais", lado), .dp_catalogo_valor(pais, paises, "PaiCod", "PaiNom"))
    lineas <- .dp_completar(lineas, paste0("ISO2", lado), .dp_catalogo_valor(pais, paises, "PaiCod", "iso2"))
    lineas <- .dp_completar(lineas, paste0("Puerto", lado),
                            .dp_catalogo_valor(.prod_id(pais, puerto), puertos, "llave", "PueNom"))
  }
  unidades_raw <- catalogos$unidades
  ids_unidad <- unique(unidades_raw$UMeCod[!is.na(unidades_raw$UMeCod)])
  siglas <- .dp_catalogo_valor(ids_unidad, unidades_raw, "UMeCod", "UMeSgl")
  nombres <- .dp_catalogo_valor(ids_unidad, unidades_raw, "UMeCod", "UMeNom")
  kg <- .dp_catalogo_valor(ids_unidad, unidades_raw, "UMeCod", "UMeFac")
  # La unidad de sacos procede del catálogo. El factor kg no implica un peso universal por saco.
  saco <- toupper(trimws(siglas)) %in% c("SAC", "SACO", "SACOS") |
    grepl("^SACOS?([[:space:]]|$)", toupper(trimws(nombres)))
  unidades <- data.frame(UMeCod = ids_unidad, UMeNom = nombres, UMeSgl = siglas,
                          KgPorUnidad = kg, SacosPorUnidad = ifelse(saco, "1", NA_character_))
  fuentes <- c("EXPPEDID", "EXPPEDI1", "NPERSONA", "NCLIENTE", "NCIUEXT", "NPAISES", "NPUERTOS",
               "NMARCAS", "NTIPPROD", "NLINEANE", "NUNIMEDI", "NFORPAG", tabla_nombres)
  list(pedidos = pedidos, lineas = lineas, clientes = clientes, unidades = unidades,
       catalogos = catalogos, paises = paises, fuentes = stats::setNames(rep("syscafe", length(fuentes)), fuentes))
}

# Productos y destinos desde los pedidos ----

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
  if (!length(filas)) {
    return(data.frame(item = character(), CiaCod = character(), codigo = character(), nombre = character(),
                      descripciones = character(), conflicto = logical(), linea_negocio = character(),
                      fuente = character(), corte = character()))
  }
  do.call(rbind, filas)
}

.prod_contar <- function(x) length(unique(x[!is.na(x) & nzchar(x)]))

.dp_productos_destinos <- function(tablas, corte) {
  hechos <- .prod_hechos(list(tablas = tablas))
  tablas$hechos_productos <- hechos
  tablas$dim_marcas <- .prod_catalogo(.prod_dimension(hechos, "marca"), corte)
  tablas$dim_nombres_comerciales <- .prod_catalogo(.prod_dimension(hechos, "nombre"), corte)
  observadas <- !is.na(hechos$marca) & !is.na(hechos$nombre)
  campos <- c("pedido_id", "PdcLin", "CiaCod", "PerCod", "fecha", "marca", "nombre", "sacos", "fuente")
  tablas$relacion_marca_nombre <- hechos[observadas, campos, drop = FALSE]

  # Un pedido con varios destinos cuenta una vez por destino. Los sacos se conservan en su grano de línea.
  destino <- hechos[!is.na(hechos$fecha), , drop = FALSE]
  destino$periodo <- format(destino$fecha, "%Y-%m")
  destino$pais[is.na(destino$pais) | !nzchar(destino$pais)] <- "Sin código"
  grupos <- split(destino, .prod_id(destino$periodo, destino$pais))
  filas <- lapply(grupos, function(x) {
    data.frame(periodo = x$periodo[1L], pais = x$pais[1L], pedidos = .prod_contar(x$pedido_id),
               clientes = .prod_contar(x$PerCod), sacos = .dp_suma(x$sacos))
  })
  tablas$pedidos_por_destino <- if (length(filas)) do.call(rbind, filas) else
    data.frame(periodo = character(), pais = character(), pedidos = integer(), clientes = integer(), sacos = numeric())
  tablas
}

# Extracción, objetos de consumo y publicación ----

EjecutarDataPrep <- function(corte = as.Date(Sys.time(), tz = "America/Bogota"),
                              raiz = ".", consulta = racafeBD::ConsultaSistema) {
  fecha_corte <- .dp_fecha(as.character(corte))
  if (length(fecha_corte) != 1L || is.na(fecha_corte) || !identical(as.character(fecha_corte), as.character(corte))) {
    stop("Indique una fecha de corte YYYY-MM-DD.", call. = FALSE)
  }

  # Pedidos, clientes y catálogos ----
  origen <- .dp_extraer(fecha_corte, consulta)
  tablas <- .dp_pedidos(origen$pedidos, origen$lineas, origen$clientes, origen$unidades, fecha_corte)
  fuentes <- c(raw_clientes = "NPERSONA/NCLIENTE", raw_pedidos = "EXPPEDID",
               raw_lineas = "EXPPEDI1", raw_unidades = "NUNIMEDI")
  for (nombre in names(fuentes)) {
    tablas[[nombre]] <- data.frame(fuente = unname(fuentes[nombre]), datos = I(list(tablas[[nombre]])))
  }
  tablas$homologaciones <- data.frame(fuente = names(origen$catalogos), datos = I(unname(origen$catalogos)))
  iso <- sort(unique(stats::na.omit(origen$paises$iso2)))
  tablas$paises <- data.frame(iso2 = iso, nombre = .dp_catalogo_valor(iso, origen$paises, "iso2", "PaiNom"))

  # Objetos analíticos para la app ----
  tablas <- .dp_analitica(tablas, fecha_corte)
  tablas <- .dp_productos_destinos(tablas, fecha_corte)
  tablas$calidad$incidencia <- seq_len(nrow(tablas$calidad))

  # Las exportaciones, los embarques efectivos y FCC quedan pendientes de fuente validada.
  procedencia <- list(fuentes = origen$fuentes, corte = fecha_corte,
                      pendientes = c("exportaciones", "lotes", "embarques", "margenes"))
  manifiesto <- PublicarOperacionalRDS(raiz, tablas, ContratosOperacionales(tablas), procedencia)
  message("Corte operacional publicado: ", manifiesto$id, " · ", length(tablas), " objetos.")
  invisible(manifiesto)
}

if (sys.nframe() == 0L) {
  argumentos <- commandArgs(trailingOnly = TRUE)
  if (length(argumentos) > 1L) stop("Uso: Rscript DataPrep.R [YYYY-MM-DD]", call. = FALSE)
  if (length(argumentos)) EjecutarDataPrep(corte = argumentos[[1L]]) else EjecutarDataPrep()
}
