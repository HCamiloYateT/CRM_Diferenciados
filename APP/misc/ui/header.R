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

.boton_header <- function(id, titulo, simbolo) {
  boton <- if (.disponible_api("racafeForms", "racafeButton")) {
    racafeForms::racafeButton(id, NULL, icon = simbolo, title = titulo, size = "xxs", variant = "secondary")
  } else {
    shiny::actionButton(id, NULL, icon = shiny::icon(simbolo), title = titulo, class = "btn-sm btn-outline-secondary")
  }
  shiny::tagAppendAttributes(boton, `aria-label` = titulo)
}

.acciones_header_ui <- function() {
  simbolos <- c(contacto = "user-plus", gestion = "clipboard-list", oportunidad = "bullseye",
                muestra = "flask", tarea = "list-check")
  shiny::div(class = "crm-header-menu", lapply(names(simbolos), function(id) {
    .boton(paste0("header_", id), .acciones[[id]]$titulo, simbolos[[id]])
  }))
}

.header_crm <- function() {
  marca <- dashboardBrand(title = .titulo, href = "https://analitica.racafe.com/PortalAnalitica/",
                          image = .logo_crm)
  actualizar <- if (.disponible_api("racafeShiny", "Boton")) {
    shiny::tagAppendAttributes(racafeShiny::Boton("actualizar", label = NULL, icono = "sync",
                                                  size = "xxs", title = "Actualizar corte operacional",
                                                  color_fondo = "#6c757d", color_hover = "#DA291C"),
                                `aria-label` = "Actualizar corte operacional")
  } else .boton_header("actualizar", "Actualizar corte operacional", "sync")
  corporativo <- .MenuIndicadoresCorporativo()
  indicadores <- if (corporativo) {
    racafeModulos::MenuHeaderUI("MenuIndicadores", icon = shiny::icon("arrow-trend-up"),
                                min_width = "400px", title = "Indicadores")
  } else {
    tags$li(class = "nav-item dropdown", .boton_header("abrir_indicadores", "Indicadores", "arrow-trend-up"))
  }
  acciones <- if (corporativo) {
    racafeModulos::MenuHeaderUI("MenuAccionesCRM", icon = shiny::icon("circle-plus"), min_width = "240px",
                                title = "Acciones rápidas")
  } else {
    tags$li(class = "nav-item dropdown",
            shiny::tagAppendAttributes(.boton_header("menu_acciones_crm", "Acciones rápidas", "circle-plus"),
                                       `data-toggle` = "dropdown", `aria-expanded` = "false"),
            div(class = "dropdown-menu dropdown-menu-right", .acciones_header_ui()))
  }
  bs4DashNavbar(title = marca, status = "white", border = FALSE, sidebarIcon = icon("bars"),
                controlbarIcon = icon("gears"),
                leftUi = tags$li(class = "dropdown crm-header-identidad",
                                 uiOutput("identidad_header", inline = TRUE),
                                 actualizar,
                                 uiOutput("acciones_clientes", inline = TRUE),
                                 .boton_header("glosario", "Glosario comercial", "book-open")),
                rightUi = tagList(indicadores, acciones, tags$li(class = "dropdown",
                                  .boton_header("notificaciones", "Notificaciones", "bell"))))
}
