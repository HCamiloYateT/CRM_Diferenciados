# Identidad y acciones rápidas ----

.acciones <- list(contacto = list(titulo = "Crear contacto"), gestion = list(titulo = "Nueva gestión"),
                   oportunidad = list(titulo = "Nueva oportunidad"), muestra = list(titulo = "Nueva muestra"),
                   tarea = list(titulo = "Tareas y notas"))

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
  tareas <- if (corporativo) {
    racafeModulos::MenuHeaderUI("MenuTareas", icon = shiny::icon("bars-progress"), title = "Tareas pendientes",
                                min_width = "320px")
  } else tags$li(class = "dropdown", .boton_header("pendientes_header", "Tareas pendientes", "bars-progress"))
  bs4DashNavbar(title = marca, status = "white", border = FALSE, sidebarIcon = icon("bars"),
                controlbarIcon = icon("gears"),
                leftUi = tags$li(class = "dropdown crm-header-identidad",
                                 uiOutput("identidad_header", inline = TRUE),
                                 actualizar,
                                 uiOutput("acciones_clientes", inline = TRUE),
                                 .boton_header("glosario", "Glosario comercial", "book-open")),
                rightUi = tagList(indicadores, tareas, acciones, tags$li(class = "dropdown",
                                  .boton_header("notificaciones", "Notificaciones", "bell"))))
}
