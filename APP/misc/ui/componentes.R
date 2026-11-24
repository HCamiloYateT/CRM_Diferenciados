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

.tabla_server <- function(id, datos, output) {
  presentacion <- reactive({
    tabla <- datos()
    if (is.null(tabla)) return(NULL)
    tabla[] <- lapply(tabla, function(columna) {
      if (inherits(columna, "Date")) format(columna, "%d/%m/%Y") else columna
    })
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

.selector_clientes <- function(id, etiqueta, multiple = FALSE, placeholder = "Buscar por PerCod o nombre") {
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
