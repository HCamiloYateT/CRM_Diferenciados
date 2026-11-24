# Consultas vigentes adaptadas de CRM Nacional ----

# Las consultas vigentes requieren dplyr y tidyr.
.GetFncData <- function() {
  tryCatch({
    `%>%` <- dplyr::`%>%`
    contenido <- rvest::read_html("https://federaciondecafeteros.org/wp/")

    items <- contenido %>%
      rvest::html_elements("ul.e-n-menu-heading li.e-n-menu-item span.e-n-menu-title-text") %>%
      rvest::html_text(trim = TRUE)

    precio_raw <- items[[1]]
    bolsa_raw <- items[[2]]

    precio <- as.numeric(gsub("\\D", "", precio_raw))
    bolsa <- bolsa_raw %>%
      gsub(".*:\\s*\\$?", "", .) %>%
      gsub(",", ".", .) %>%
      as.numeric()

    list(
      precio = precio,
      bolsa = bolsa
    )
  }, error = function(e) {
    warning("Error obteniendo datos de FNC: ", e$message)

    list(
      precio = NA_real_,
      bolsa = NA_real_
    )
  })
}
.GetSystemData <- function(consulta = racafeBD::ConsultaSistema) {
  tryCatch({
    `%>%` <- dplyr::`%>%`
    cons_main <- consulta(
      "syscafe",
      paste0(
        "SELECT (SELECT MedTRM FROM EXPMECAF ",
        "WHERE MedFec = (SELECT MAX(MedFec) FROM EXPMECAF)) AS TRM"
      )
    )

    ny <- consulta(
      "syscafe",
      paste0(
        "WITH Futuros AS (",
        "SELECT InFuFch AS Fecha, ",
        "DATEFROMPARTS(CAST(AnoBol AS INT), CAST(MesBolCod AS INT), 1) AS Date, ",
        "AVG(InFuCieVr) AS InFuCieVr, ",
        "ROW_NUMBER() OVER (PARTITION BY InFuFch ",
        "ORDER BY DATEFROMPARTS(CAST(AnoBol AS INT), CAST(MesBolCod AS INT), 1)) AS Posicion ",
        "FROM INFFUT1 ",
        "WHERE CiaCod = 10 AND TipCFCod = 'A' ",
        "AND InFuFch >= (SELECT MAX(InFuFch) FROM INFFUT1 ",
        "WHERE CiaCod = 10 AND TipCFCod = 'A') ",
        "AND InFuCieVr > 0 ",
        "GROUP BY InFuFch, AnoBol, MesBolCod",
        ") ",
        "SELECT Fecha, InFuCieVr AS NY ",
        "FROM Futuros ",
        "WHERE Posicion = 2"
      )
    )

    precio_carga_raw <- consulta(
      "syscafe",
      paste0(
        "SELECT h2.HTFec, ",
        "SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE h2.HTKilCom END) AS KilTo, ",
        "SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE ",
        "((h2.HTPreCom * (f.FactRen / 125.0) - ",
        "(f.FactRen * ((h.HTPreCon * f.PorCon / 100.0) + ",
        "(h.HTPrePas * f.PorPas / 100.0) + ",
        "(h.HTPreRip * f.PorRip / 100.0))) + ",
        "(f.FactBas * ((h.HTPreCon * f.PorCon / 100.0) + ",
        "(h.HTPrePas * f.PorPas / 100.0) + ",
        "(h.HTPreRip * f.PorRip / 100.0)))) * ",
        "(125.0 / f.FactBas) * h2.HTKilCom) END) AS PreTo ",
        "FROM EXPHOTR2 h2 ",
        "LEFT JOIN EXPFACON f ON h2.FactSec = f.FactSec ",
        "LEFT JOIN EXPHOTRA h ON h2.HTFec = h.HTFec ",
        "WHERE h2.HTFec = (SELECT MAX(HTFec) FROM EXPHOTR2) ",
        "GROUP BY h2.HTFec ",
        "HAVING SUM(CASE WHEN h2.HTComEsp = 'S' THEN 0 ELSE h2.HTKilCom END) > 0"
      )
    )

    precio_carga <- precio_carga_raw %>%
      dplyr::group_by(Fecha = as.Date(HTFec)) %>%
      dplyr::summarise(PrecioCarga = mean(PreTo / KilTo, na.rm = TRUE), .groups = "drop")

    precios_adicionales <- consulta(
      "syscafe",
      paste0(
        "SELECT HTPreRip, HTPrePas, HTPreCon, ",
        "CASE WHEN HTSigno = '+' THEN HTPtos ELSE -HTPtos END AS Diferencial ",
        "FROM EXPHOTRA ",
        "WHERE HTFec = (SELECT MAX(HTFec) FROM EXPHOTR2);"
      )
    )

    compras_arenales <- consulta(
      "cafesys",
      paste0(
        "SELECT r.ResFch, r.TipCaf, r.CalCod, r.TotKls, r.ResVlrNeg, ",
        "c.CalNom AS Producto ",
        "FROM RESDIA r ",
        "LEFT JOIN CALTRN c ON r.TipCaf = c.TipCaf AND r.CalCod = c.CalCod ",
        "WHERE r.CiaCod = 10 AND r.SucCod = 32 ",
        "AND r.ResFch >= DATEADD(month, -1, GETDATE()) ",
        "AND r.TMoDes = 'COMPRAS        '"
      )
    ) %>%
      dplyr::mutate(VlrKilo = ResVlrNeg / TotKls) %>%
      dplyr::group_by(Producto) %>%
      dplyr::summarise(VlrKilo = stats::weighted.mean(VlrKilo, TotKls, na.rm = TRUE),
                       .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Producto, values_from = VlrKilo)

    list(
      trm = cons_main$TRM,
      ny = ny$NY,
      precio_carga = precio_carga$PrecioCarga,
      precios_adicionales = precios_adicionales,
      precios_compras = compras_arenales
    )
  }, error = function(e) {
    warning("Error obteniendo datos del sistema: ", e$message)

    list(
      trm = NA_real_,
      ny = NA_real_,
      precio_carga = NA_real_,
      precios_adicionales = data.frame(
        HTPreRip = NA_real_,
        HTPrePas = NA_real_,
        HTPreCon = NA_real_,
        Diferencial = NA_real_
      ),
      precios_compras = data.frame(
        CONSUMO = NA_real_,
        MOLIDOS = NA_real_,
        SOLUBLE = NA_real_,
        RIPIO = NA_real_,
        ROBUSTA = NA_real_
      )
    )
  })
}
.ValorColumnaIndicador <- function(datos, columna) {
  if (is.null(datos) || nrow(datos) == 0L || !columna %in% names(datos)) return(NA_real_)

  valor <- suppressWarnings(as.numeric(datos[[columna]][[1]]))

  if (length(valor) == 0L || !is.finite(valor)) return(NA_real_)

  valor
}
.ObtenerIndicadoresActuales <- function(fnc = .GetFncData, sistema = .GetSystemData) {
  fnc_data <- fnc()
  system_data <- sistema()

  precio_fnc <- as.numeric(fnc_data$precio)[1L]
  precio_carga <- as.numeric(system_data$precio_carga)[1L]

  datos <- data.frame(
    PrecioBolsa = as.numeric(fnc_data$bolsa)[1L],
    TRM = as.numeric(system_data$trm)[1L],
    PrecioFNC = precio_fnc,
    UGCFNC = (precio_fnc / 125) / (70 / 96.89),
    PrecioNY = as.numeric(system_data$ny)[1L],
    PrecioCarga = precio_carga,
    Diferencial = .ValorColumnaIndicador(system_data$precios_adicionales, "Diferencial"),
    UGCRacafe = (precio_carga / 125) / (70 / 96.89),
    CALConsumo = .ValorColumnaIndicador(system_data$precios_adicionales, "HTPreCon"),
    COMConsumo = .ValorColumnaIndicador(system_data$precios_compras, "CONSUMO"),
    CALPasilla = .ValorColumnaIndicador(system_data$precios_adicionales, "HTPrePas"),
    COMMolidos = .ValorColumnaIndicador(system_data$precios_compras, "MOLIDOS"),
    COMSoluble = .ValorColumnaIndicador(system_data$precios_compras, "SOLUBLE"),
    CALRipio = .ValorColumnaIndicador(system_data$precios_adicionales, "HTPreRip"),
    COMRipio = .ValorColumnaIndicador(system_data$precios_compras, "RIPIO"),
    COMRobusta = .ValorColumnaIndicador(system_data$precios_compras, "ROBUSTA")
  )

  valores <- unlist(datos[.IND_CLAVES_CANON], use.names = FALSE)
  valores[!is.finite(valores)] <- NA_real_
  data.frame(Item = factor(unname(.IND_NOMBRES_CORTOS), levels = unname(.IND_NOMBRES_CORTOS)), Valor = valores)
}

# Histórico compartido y cálculos de presentación ----

.HoyIndicadores <- function() as.Date(Sys.time(), tz = "America/Bogota")

.LeerHistoricoIndicadores <- function() racafeBD::CargarDatos("CRMINDICADORES")

.CargarHistoricoIndicadores <- function(fecha, historico = .LeerHistoricoIndicadores()) {
  if (!is.data.frame(historico) || !"FechaActualizacion" %in% names(historico)) {
    stop("El histórico no contiene FechaActualizacion.", call. = FALSE)
  }
  fechas <- as.Date(historico$FechaActualizacion)
  coincidencias <- which(fechas == fecha)
  historico[head(coincidencias, 1L), , drop = FALSE]
}

.CargarAnteriorIndicadores <- function(fecha = .HoyIndicadores() - 1, historico = .LeerHistoricoIndicadores()) {
  datos <- .CargarHistoricoIndicadores(fecha, historico)
  claves <- intersect(.IND_CLAVES_CANON, names(datos))
  if (!nrow(datos)) claves <- character()
  data.frame(Item = unname(.IND_NOMBRES_CORTOS[claves]),
             anterior = vapply(claves, function(clave) .ValorColumnaIndicador(datos, clave), numeric(1)))
}

.FormatearIndicadores <- function(valores, claves) {
  vapply(seq_along(valores), function(i) {
    if (!is.finite(valores[i])) return("—")
    texto <- formatC(valores[i], digits = .IND_DECIMALES[[claves[i]]], format = "f",
                     big.mark = ",", decimal.mark = ".")
    paste(paste0("$", texto), .IND_UNIDADES[[claves[i]]])
  }, character(1))
}

.PrepararTendenciaIndicadores <- function(datos_actuales, datos_anteriores) {
  items <- unname(.IND_NOMBRES_CORTOS)
  actual <- datos_actuales$Valor[match(items, as.character(datos_actuales$Item))]
  anterior <- datos_anteriores$anterior[match(items, datos_anteriores$Item)]
  tendencia <- rep("igual", length(items))
  tendencia[which(actual > anterior)] <- "sube"
  tendencia[which(actual < anterior)] <- "baja"
  texto <- .FormatearIndicadores(actual, .IND_CLAVES_CANON)
  texto[is.finite(actual)] <- paste(.IND_FLECHAS_TENDENCIA[tendencia[is.finite(actual)]], texto[is.finite(actual)])
  data.frame(Item = items, Valor = unname(texto), tendencia)
}

.NormalizarIndicadoresActuales <- function(datos) {
  valores <- datos$Valor[match(unname(.IND_NOMBRES_CORTOS), as.character(datos$Item))]
  as.data.frame(as.list(stats::setNames(valores, .IND_CLAVES_CANON)))
}

.ConstruirComparacionIndicadores <- function(datos_anteriores, datos_actuales) {
  claves <- .IND_CLAVES_CANON
  anterior <- vapply(claves, function(x) .ValorColumnaIndicador(datos_anteriores, x), numeric(1))
  actual <- vapply(claves, function(x) .ValorColumnaIndicador(datos_actuales, x), numeric(1))
  diferencia <- actual - anterior
  variacion <- diferencia / abs(anterior)
  variacion[!is.finite(variacion)] <- NA_real_
  data.frame(Indicador = claves, Nombre_Indicador = unname(.IND_NOMBRES_COMPARACION[claves]),
             Valor_Anterior = anterior, Valor_Actual = actual,
             Diferencia_Absoluta = diferencia, Variacion_Porcentual = variacion, row.names = NULL)
}

.ConsultarComparacionIndicadores <- function(inicio, fin, hoy = .HoyIndicadores(),
                                             leer = .LeerHistoricoIndicadores,
                                             vigentes = .ObtenerIndicadoresActuales) {
  if (inicio > fin || fin > hoy) stop("Seleccione fechas válidas hasta hoy, en orden cronológico.", call. = FALSE)
  historico <- leer()
  anterior <- .CargarHistoricoIndicadores(inicio, historico)
  if (!nrow(anterior)) stop("Sin indicadores históricos para la fecha inicial.", call. = FALSE)
  if (fin == hoy) {
    actual <- .NormalizarIndicadoresActuales(vigentes())
    if (!any(is.finite(unlist(actual)))) stop("Sin indicadores vigentes para la fecha final.", call. = FALSE)
  } else {
    actual <- .CargarHistoricoIndicadores(fin, historico)
    if (!nrow(actual)) stop("Sin indicadores históricos para la fecha final.", call. = FALSE)
  }
  list(datos = .ConstruirComparacionIndicadores(anterior, actual), inicio = inicio, fin = fin,
       fuente = if (fin == hoy) "FNC y consultas operacionales vigentes" else "CRMINDICADORES")
}
