# Componentes reutilizados en los módulos ----

.disponible_api <- function(paquete, funcion) {
  requireNamespace(paquete, quietly = TRUE) && funcion %in% getNamespaceExports(paquete)
}

.nota <- function(texto, tipo = "info") {
  if (.disponible_api("racafeForms", "racafeFormMessage")) {
    return(racafeForms::racafeFormMessage(texto, type = tipo))
  }
  clases <- c(info = "info", warning = "warning", error = "danger", empty = "secondary", muted = "secondary")
  div(class = paste("alert", paste0("alert-", clases[[tipo]])), role = "status", texto)
}

.etiquetas_crm_ui <- function(etiquetas) {
  etiquetas <- unique(trimws(as.character(etiquetas)))
  etiquetas <- etiquetas[!is.na(etiquetas) & nzchar(etiquetas)]
  if (!length(etiquetas)) return(NULL)
  tags$ul(class = "crm-etiquetas list-unstyled d-flex flex-wrap mb-0", lapply(etiquetas, function(etiqueta) {
    tags$li(racafeForms::racafeStatusBadge(etiqueta, variant = "info", size = "sm"))
  }))
}

.boton <- function(id, etiqueta, simbolo) {
  if (.disponible_api("racafeForms", "racafeButton")) {
    return(racafeForms::racafeButton(id, etiqueta, icon = simbolo, size = "xxs", variant = "secondary", title = etiqueta))
  }
  actionButton(id, etiqueta, icon = icon(simbolo), class = "btn-sm btn-outline-secondary", title = etiqueta)
}

.bloqueado <- function(etiqueta, razon) {
  div(class = "crm-bloqueado", tags$button(type = "button", disabled = NA, class = "btn btn-sm btn-outline-secondary",
                                         etiqueta), tags$small(razon))
}

.tabla_ui <- function(id) {
  if (.disponible_api("racafeModulos", "TablaVancouverUI")) return(racafeModulos::TablaVancouverUI(id, descargable = FALSE))
  div(class = "crm-tabla table-responsive", tableOutput(id))
}

# Fechas para presentación en Colombia ----

.formato_fecha_crm <- function(x, mes = FALSE) {
  if (mes) return(sub("^([0-9]{4})-([0-9]{2})$", "\\2/\\1", as.character(x)))
  if (inherits(x, "POSIXt")) {
    zona <- "America/Bogota"
    fecha <- format(x, "%d/%m/%Y %I:%M", tz = zona)
    sufijo <- ifelse(format(x, "%H", tz = zona) < "12", "a. m.", "p. m.")
    return(ifelse(is.na(x), NA_character_, paste(fecha, sufijo)))
  }
  format(x, "%d/%m/%Y")
}

.formato_moneda_crm <- function(x) {
  vapply(x, function(valor) {
    if (is.na(valor)) return(NA_character_)
    paste0("$", format(valor, big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE, digits = 10L))
  }, character(1), USE.NAMES = FALSE)
}

.tabla_server <- function(id, datos, output) {
  presentacion <- reactive({
    tabla <- datos()
    if (is.null(tabla)) return(NULL)
    for (campo in names(tabla)) {
      x <- tabla[[campo]]
      monetario <- campo %in% c("valor", "MNFCC", "MNFCC_COP", "PptoMNFCC", "DiferenciaMNFCC",
                                "MargenCOP", "PptoMargenUScentLb", "diferencial_venta", "fcc")
      if (monetario && is.numeric(x)) tabla[[campo]] <- .formato_moneda_crm(x)
      if (inherits(x, c("Date", "POSIXt"))) tabla[[campo]] <- .formato_fecha_crm(x)
      if (campo %in% c("Mes", "Cohorte", "mes", "cohorte", "periodo", "periodo_cabecera")) {
        tabla[[campo]] <- .formato_fecha_crm(tabla[[campo]], mes = TRUE)
      }
      if (campo %in% c("fecha_creacion_original", "fecha_embarque_original")) {
        hora <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}", x)
        dia <- !hora & !is.na(.dp_fecha(x))
        tabla[[campo]][dia] <- .formato_fecha_crm(.dp_fecha(x[dia]))
        instante <- as.POSIXct(sub("T", " ", x[hora], fixed = TRUE), format = "%Y-%m-%d %H:%M",
                               tz = "America/Bogota")
        tabla[[campo]][hora] <- .formato_fecha_crm(instante)
      }
    }
    names(tabla)[names(tabla) == "PerCod"] <- "Código de cliente"
    tabla
  })
  if (.disponible_api("racafeModulos", "TablaVancouverServer")) {
    racafeModulos::TablaVancouverServer(id, data = presentacion, descargable = FALSE)
  } else {
    output[[id]] <- renderTable({
      tabla <- presentacion()
      req(!is.null(tabla), nrow(tabla))
      tabla
    }, striped = TRUE, bordered = FALSE, digits = 6, na = "—", spacing = "s")
  }
}

.kpi_ui <- function(id, etiqueta, simbolo = "chart-simple") {
  if (.disponible_api("racafeModulos", "cajaValor_ui")) {
    return(racafeModulos::cajaValor_ui(id, texto = etiqueta, icono = simbolo, mostrar_boton = FALSE, compact = TRUE))
  }
  div(class = "crm-kpi", tags$span(etiqueta), strong(textOutput(id, inline = TRUE)))
}

.kpi_server <- function(id, valor, output) {
  if (.disponible_api("racafeModulos", "cajaValor_server")) {
    racafeModulos::cajaValor_server(id, valor, formato = NULL, formatear = function(x) tags$span(x))
  } else {
    output[[id]] <- renderText(valor())
  }
}

.fuentes_pendientes <- c("Sacos embarcados" = "embarques", "Compromisos pendientes" = "embarques",
                         "Margen disponible" = "margenes", "Presupuesto" = "presupuesto",
                         "Tier" = "tiers",
                         "Productos homologados" = "productos")

# Identificación comercial ----

.cliente_etiqueta <- function(percod, nombre) {
  nombre <- rep_len(nombre, length(percod))
  ifelse(is.na(nombre) | !nzchar(trimws(nombre)), percod, paste(percod, "·", nombre))
}

.etiquetar_clientes <- function(datos, clientes) {
  if (is.null(datos) || !"PerCod" %in% names(datos)) return(datos)
  nombre <- if ("PerRazSoc" %in% names(datos)) {
    datos$PerRazSoc
  } else if (!is.null(clientes)) clientes$PerRazSoc[match(datos$PerCod, clientes$PerCod)] else NA_character_
  datos$PerCod <- .cliente_etiqueta(datos$PerCod, nombre)
  datos
}

.formulario_crm <- function(contenido) {
  if (.disponible_api("racafeForms", "racafeForm")) {
    racafeForms::racafeForm(contenido, variant = "plain")
  } else contenido
}

.clientes_vista <- function(clientes, pedidos = NULL, margenes = NULL) {
  if (is.null(clientes)) return(NULL)
  ids <- clientes$PerCod
  conteo <- if (is.null(pedidos)) rep(NA_integer_, length(ids)) else {
    tabulate(match(pedidos$PerCod, ids), nbins = length(ids))
  }
  grupos <- if (is.null(pedidos)) list() else split(pedidos$fecha_creacion, pedidos$PerCod)
  ultima <- vapply(grupos, function(x) if (all(is.na(x))) NA_real_ else as.numeric(max(x, na.rm = TRUE)), numeric(1))
  datos <- data.frame(Cliente = .cliente_etiqueta(ids, clientes$PerRazSoc), Estado = rep("Cliente", length(ids)),
             Pedidos = conteo,
             Sacos = if (is.null(pedidos)) rep(NA_real_, length(ids)) else .dp_agregar(pedidos$PerCod, pedidos$sacos, ids),
             MNFCC_COP = if (is.null(margenes)) rep(NA_real_, length(ids)) else
               .dp_agregar(margenes$PerCod, margenes$MNFCC, ids),
             UltimoPedido = as.Date(unname(ultima[match(ids, names(ultima))]), origin = "1970-01-01"))
  extras <- c(pais = "País", CiuExtNom = "Ciudad", CliCont = "Contacto", CliTel = "Teléfono",
              CliConCom = "Contacto comercial", CliTelCom = "Teléfono comercial", CliEmlCom = "Correo comercial")
  for (campo in intersect(names(extras), names(clientes))) datos[[extras[[campo]]]] <- clientes[[campo]]
  datos
}

.selector_clientes <- function(id, etiqueta, multiple = FALSE, placeholder = "Buscar por código de cliente o nombre") {
  shiny::selectizeInput(id, etiqueta, choices = NULL, multiple = multiple,
                        options = list(placeholder = placeholder, maxOptions = 50))
}

.actualizar_selector_clientes <- function(session, id, clientes, seleccion) {
  opciones <- character()
  if (!is.null(clientes) && nrow(clientes)) {
    nombre <- clientes$PerRazSoc
    rotulos <- .cliente_etiqueta(clientes$PerCod, nombre)
    opciones <- stats::setNames(clientes$PerCod, rotulos)
  }
  shiny::updateSelectizeInput(session, id, choices = opciones, selected = intersect(seleccion, opciones),
                              server = TRUE)
}
