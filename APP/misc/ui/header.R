# Identidad y acciones rápidas ----

.acciones <- list(
  contacto = list(titulo = "Crear contacto", campos = c("Empresa", "Nombre del contacto", "Correo", "Cargo",
                                                         "País", "Tipo de comprador", "Origen")),
  gestion = list(titulo = "Nueva gestión", campos = c("Empresa o PerCod", "Contacto", "Tipo de gestión",
                                                       "Fecha", "Responsable", "Resultado", "Observaciones")),
  oportunidad = list(titulo = "Nueva oportunidad", campos = c("Empresa o PerCod", "Tipo de oportunidad", "Criterio",
                                                                "Origen", "Responsable", "Observaciones")),
  muestra = list(titulo = "Nueva muestra", campos = c("Empresa o PerCod", "Contacto", "Descripción original",
                                                       "Origen", "Requisitos", "Fecha de solicitud", "Courier", "Guía",
                                                       "Fecha de envío", "Resultado de evaluación", "Observaciones",
                                                       "Responsable")),
  tarea = list(titulo = "Tareas y notas", campos = c("Empresa o PerCod", "Contacto", "Oportunidad", "Prioridad",
                                                      "Vencimiento", "Responsable", "Nota"))
)

.formulario_accion <- function(accion) {
  configuracion <- .acciones[[accion]]
  controles <- lapply(seq_along(configuracion$campos), function(i) {
    etiqueta <- configuracion$campos[i]
    id <- paste("captura", accion, i, sep = "_")
    if (etiqueta %in% c("Observaciones", "Criterio", "Nota", "Requisitos")) {
      return(textAreaInput(id, etiqueta, rows = 2L))
    }
    if (startsWith(etiqueta, "Fecha") || etiqueta == "Vencimiento") {
      return(div(class = "form-group", tags$label(`for` = id, etiqueta),
                 tags$input(id = id, type = "date", class = "form-control")))
    }
    if (etiqueta == "Tipo de oportunidad") {
      return(selectInput(id, etiqueta, c("Seleccione…" = "", "Adquisición", "Cross-selling", "Up-selling")))
    }
    textInput(id, etiqueta)
  })
  formulario <- tags$fieldset(disabled = NA, class = "crm-form-grid", controles)
  if (.disponible_api("racafeForms", "racafeForm")) {
    formulario <- racafeForms::racafeForm(formulario, variant = "plain")
  }
  modalDialog(title = configuracion$titulo, size = "l", easyClose = TRUE,
              .nota("Captura pendiente de servicio de escritura, permisos y validación del flujo.", "warning"),
              formulario, footer = tagList(.bloqueado("Guardar", "Acción aún no disponible"), modalButton("Cerrar")))
}

.header_crm <- function() {
  marca <- dashboardBrand(title = .titulo, href = "https://analitica.racafe.com/PortalAnalitica/",
                          image = .logo_crm)
  indicadores <- if (.MenuIndicadoresCorporativo()) {
    racafeModulos::MenuHeaderUI("MenuIndicadores", icon = shiny::icon("chart-line"),
                                min_width = "400px", title = "Indicadores")
  } else {
    shiny::tags$li(class = "nav-item dropdown crm-nav-acciones",
                   .boton("abrir_indicadores", "Indicadores", "chart-line"))
  }
  bs4DashNavbar(title = marca, status = "white", sidebarIcon = icon("bars"), controlbarIcon = icon("sliders"),
                leftUi = tags$li(class = "nav-item dropdown crm-nav-acciones",
                                 .boton("actualizar", "Actualizar", "rotate"),
                                 uiOutput("acciones_clientes", inline = TRUE),
                                 .boton("header_contacto", "Contacto", "user-plus"),
                                 .boton("header_gestion", "Gestión", "clipboard-list"),
                                 .boton("header_oportunidad", "Oportunidad", "bullseye"),
                                 .boton("header_muestra", "Muestra", "flask")),
                rightUi = tagList(indicadores, tags$li(class = "nav-item dropdown crm-nav-acciones",
                                  .boton("notificaciones", "Alertas", "bell"),
                                  uiOutput("identidad_header", inline = TRUE))))
}
