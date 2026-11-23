# Adaptadores y validaciones ----

.dp_leer <- function(carpeta, nombre, requeridas, opcional = FALSE) {
  ruta <- file.path(carpeta, paste0(nombre, ".csv"))
  if (opcional && !file.exists(ruta)) return(NULL)
  datos <- utils::read.csv(ruta, colClasses = "character", na.strings = "", check.names = FALSE,
                           strip.white = FALSE, fileEncoding = "UTF-8")
  if (anyDuplicated(names(datos)) || !all(requeridas %in% names(datos))) {
    faltantes <- setdiff(requeridas, names(datos))
    stop("Columnas duplicadas o faltantes en ", nombre, ": ", paste(faltantes, collapse = ", "), call. = FALSE)
  }
  datos
}

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
                            codigo_pais = .dp_col(raw_clientes, "PaiCod"))
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
  tablas$cohortes_clientes <- primero[c("PerCod", "cohorte")]
  tablas$clientes_comerciales <- merge(tablas$clientes, primero, by = "PerCod", all.x = TRUE, sort = FALSE)
  clientes <- tablas$clientes_comerciales
  clientes$tiene_pedidos <- clientes$PerCod %in% pedidos$PerCod
  clientes$fecha_primera_observable <- !is.na(clientes$fecha_creacion)
  tablas$clientes_comerciales <- clientes
  volumen <- unique(pedidos["PerCod"])
  volumen$sacos <- .dp_agregar(pedidos$PerCod, pedidos$sacos, volumen$PerCod)
  volumen$kg <- .dp_agregar(pedidos$PerCod, pedidos$kg, volumen$PerCod)
  volumen$pedidos <- as.integer(table(pedidos$PerCod)[volumen$PerCod])
  tablas$volumen_cliente <- volumen
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
                 volumen_cliente = "PerCod", pedidos_por_periodo = "periodo",
                 jerarquia_cuentas = c("PerCod", "desde"), referencias_embarque = c("pedido_id", "referencia"),
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
  contratos$lineas_pedido$validar <- function(datos) {
    if (anyNA(datos$LinNegCod) || any(datos$LinNegCod != "20000")) stop("Línea fuera de alcance.")
  }
  contratos
}
