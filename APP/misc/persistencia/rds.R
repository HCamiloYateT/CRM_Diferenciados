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
  if (!requireNamespace("filelock", quietly = TRUE)) stop("Instale el paquete filelock.", call. = FALSE)
  bloqueo <- filelock::lock(file.path(ruta, "escritura.lock"), exclusive = TRUE, timeout = 10000)
  if (is.null(bloqueo)) stop("No fue posible adquirir el bloqueo de escritura RDS.", call. = FALSE)
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
                               referencia = NULL, procedencia = NULL) {
  ruta <- .rds_dominio(almacen, dominio)
  bloqueo <- .rds_bloquear(ruta)
  on.exit(filelock::unlock(bloqueo))
  contratos <- almacen$contratos[[dominio]]
  vigente <- .rds_vigente(ruta, contratos)
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
  on.exit(filelock::unlock(bloqueo))
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
