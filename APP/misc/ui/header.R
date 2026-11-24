# Identidad y acciones rápidas ----

.acciones <- list(contacto = list(titulo = "Crear contacto"), gestion = list(titulo = "Nueva gestión"),
                   oportunidad = list(titulo = "Nueva oportunidad"), muestra = list(titulo = "Nueva muestra"),
                   tarea = list(titulo = "Tareas y notas"))

.boton_header <- function(id, titulo, simbolo) {
  boton <- if (.disponible_api("racafeForms", "racafeButton")) {
    racafeForms::racafeButton(id, NULL, icon = simbolo, title = titulo, size = "xxs", variant = "secondary")
  } else {
    shiny::actionButton(id, NULL, icon = shiny::icon(simbolo), title = titulo, class = "btn-sm btn-secondary")
  }
  shiny::tagAppendAttributes(boton, `aria-label` = titulo)
}

.paleta_header_ui <- function() {
  boton <- if (.disponible_api("racafeForms", "racafeButton")) {
    racafeForms::racafeButton("escalas_color", NULL, icon = "palette", title = "Escalas de color",
                              variant = "secondary", size = "xs")
  } else .boton_header("escalas_color", "Escalas de color", "palette")
  escala <- function(paleta) {
    lapply(names(paleta), function(valor) {
      div(tags$span(class = "kb-scale-swatch", style = paste0("background:", paleta[[valor]])), valor)
    })
  }
  div(class = "dropdown",
      shiny::tagAppendAttributes(boton, `data-toggle` = "dropdown", `aria-haspopup` = "true",
                                 `aria-expanded` = "false", `aria-label` = "Escalas de color"),
      div(class = "dropdown-menu kb-scale-menu", tags$strong("ETAPA DE LA ENTIDAD"),
          escala(.colores_etapa_crm), tags$hr(), tags$strong("PRIORIDAD DE TAREAS"),
          escala(.colores_prioridad_crm)))
}

# Header ----

header <- bs4DashNavbar(
  status = "white", border = FALSE, sidebarIcon = icon("bars"),
  title = dashboardBrand(title = .titulo, href = "https://analitica.racafe.com/PortalAnalitica/",
                         image = .logo_crm),
  controlbarIcon = icon("gears"),
  leftUi = tagList(tags$li(
    class = "dropdown crm-header-identidad",
    tags$span(uiOutput("identidad_header", inline = TRUE)),
    if (.disponible_api("racafeShiny", "Boton")) {
      shiny::tagAppendAttributes(racafeShiny::Boton("actualizar", label = NULL, icono = "sync", size = "xxs",
                                                   title = "Actualizar corte operacional", color_fondo = "#6c757d",
                                                   color_hover = "#DA291C"),
                                 `aria-label` = "Actualizar corte operacional")
    } else .boton_header("actualizar", "Actualizar corte operacional", "sync"),
    shiny::tagAppendAttributes(
      if (.disponible_api("racafeForms", "racafeButtonDownload")) {
        racafeForms::racafeButtonDownload("descarga_clientes", label = NULL, icon = "users",
                                          size = "xxs", variant = "secondary")
      } else downloadButton("descarga_clientes", NULL, icon = icon("users"), class = "btn-sm btn-secondary"),
      title = "Descargar clientes", `aria-label` = "Descargar clientes"
    ),
    .boton_header("glosario", "Glosario comercial", "book-open"),
    .paleta_header_ui()
  )),
  rightUi = tagList(
    if (.MenuIndicadoresCorporativo()) {
      racafeModulos::MenuHeaderUI("MenuIndicadores", icon = icon("arrow-trend-up"),
                                  min_width = "400px", title = "Indicadores")
    } else {
      tags$li(class = "nav-item dropdown", .boton_header("abrir_indicadores", "Indicadores", "arrow-trend-up"))
    },
    .SeguimientoHeaderUI("MenuNotas", .menus_seguimiento_header$MenuNotas),
    .SeguimientoHeaderUI("MenuTareas", .menus_seguimiento_header$MenuTareas)
  )
)
