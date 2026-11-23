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

# Ejecución del bloque de pedidos y clientes ----

EjecutarDataPrep <- function(carpeta, corte, raiz = ".") {
  source(file.path(raiz, "APP", "misc", "datos", "dataprep_pedidos.R"), local = TRUE)
  source(file.path(raiz, "APP", "misc", "persistencia", "rds.R"), local = TRUE)
  fecha_corte <- .dp_fecha(as.character(corte))
  if (length(fecha_corte) != 1L || is.na(fecha_corte) || !identical(as.character(fecha_corte), as.character(corte))) {
    stop("Indique una fecha de corte YYYY-MM-DD.", call. = FALSE)
  }
  # El informe incluso de cortes rechazados se conserva fuera de la generación vigente.
  datos <- AbrirPersistenciaRDS(raiz, list(operacional = list(
    calidad = list(version = "v1", prototipo = data.frame(incidencia = integer()), clave = "incidencia")
  )))$datos
  carpeta_auditoria <- .rds_directorio(file.path(datos, "auditoria_etl"), normalizePath(raiz, winslash = "/"))
  informe <- tempfile("dataprep-", tmpdir = carpeta_auditoria, fileext = ".csv")
  registrar <- function(estado, detalle, incidencias = NULL) {
    registro <- data.frame(corte = as.character(fecha_corte), estado, clave = NA_character_,
                           codigo = NA_character_, detalle)
    if (!is.null(incidencias) && nrow(incidencias)) {
      filas <- data.frame(corte = as.character(fecha_corte), estado, incidencias[c("clave", "codigo")],
                          detalle = NA_character_)
      registro <- rbind(registro, filas)
    }
    utils::write.csv(registro, informe, row.names = FALSE, na = "")
  }
  tryCatch({
    archivos <- c(raw_clientes = "clientes.csv", raw_pedidos = "pedidos.csv", raw_lineas = "lineas.csv",
                  raw_unidades = "unidades.csv")
    fuentes <- c(unname(archivos), "lotes_productos.csv")
    origen <- setNames(file.path(carpeta, fuentes), fuentes)
    firmar <- function() {
      presentes <- origen[file.exists(origen)]
      setNames(unname(tools::md5sum(presentes)), names(presentes))
    }
    firmas <- firmar()
    raw_clientes <- .dp_leer(carpeta, "clientes", "PerCod")
    raw_pedidos <- .dp_leer(carpeta, "pedidos", c("CiaCod", "PdcCod", "CliNit", "PdcFecCre"))
    raw_lineas <- .dp_leer(carpeta, "lineas", c("CiaCod", "PdcCod", "PdcLin", "LinNegCod", "PdcCan"))
    raw_unidades <- .dp_leer(carpeta, "unidades", "UMeCod", opcional = TRUE)
    tablas <- .dp_pedidos(raw_pedidos, raw_lineas, raw_clientes, raw_unidades, fecha_corte)
    tablas <- .dp_analitica(tablas, fecha_corte)
    tablas$calidad$incidencia <- seq_len(nrow(tablas$calidad))
    cifras <- sprintf("Cabeceras: %d; líneas: %d; líneas fuera de alcance: %d; incidencias: %d.",
                      nrow(tablas$pedidos), nrow(tablas$lineas_pedido),
                      nrow(raw_lineas) - nrow(tablas$lineas_pedido), nrow(tablas$calidad))
    for (nombre in names(archivos)) {
      tablas[[nombre]] <- data.frame(fuente = unname(archivos[nombre]), datos = I(list(tablas[[nombre]])))
    }
    if ("lotes_productos.csv" %in% names(firmas)) {
      source(file.path(raiz, "APP", "misc", "modules", "productos", "datos.R"), local = TRUE)
      lotes <- .dp_leer(carpeta, "lotes_productos", c("CiaCod", "CLPdcCod", "CLPdcLin", "CLLotCod",
                                                      "CLLinNegCo", "SacosAsignados"))
      normalizados <- .prod_normalizar_lotes(lotes)
      lotes <- lotes[lotes$CLLinNegCo == "20000", , drop = FALSE]
      tablas$raw_lineas <- rbind(tablas$raw_lineas,
                                data.frame(fuente = "lotes_productos.csv", datos = I(list(lotes))))
      .prod_hechos(list(tablas = c(tablas, list(lotes_productos = normalizados))))
    }
    registrar("validado", cifras, tablas$calidad)
    if (anyNA(firmas) || !identical(firmar(), firmas)) stop("Las exportaciones cambiaron durante el ETL.")
    procedencia <- list(fuentes = firmas, corte = fecha_corte, informe = basename(informe))
    manifiesto <- PublicarOperacionalRDS(raiz, tablas, ContratosOperacionales(tablas), procedencia)
    message("Corte operacional publicado: ", manifiesto$id, ". Informe: ", informe)
    invisible(list(manifiesto = manifiesto, informe = informe))
  }, error = function(e) {
    registrar("rechazado", conditionMessage(e))
    stop(conditionMessage(e), " Informe: ", informe, call. = FALSE)
  })
}

if (sys.nframe() == 0L) {
  argumentos <- commandArgs(trailingOnly = TRUE)
  if (length(argumentos) != 2L) stop("Uso: Rscript DataPrep.R <carpeta-exportaciones> <YYYY-MM-DD>", call. = FALSE)
  EjecutarDataPrep(argumentos[1L], argumentos[2L])
}
