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
                         "Cuenta padre" = "jerarquia_cuentas", "Tier" = "tiers",
                         "Productos homologados" = "productos")
