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
    .GlosarioHeaderUI(),
    div(class = "dropdown",
        shiny::tagAppendAttributes(
          if (.disponible_api("racafeForms", "racafeButton")) {
            racafeForms::racafeButton("escalas_color", NULL, icon = "palette", title = "Escalas de color",
                                      variant = "secondary", size = "xs")
          } else .boton_header("escalas_color", "Escalas de color", "palette"),
          `data-toggle` = "dropdown", `aria-haspopup` = "true", `aria-expanded` = "false",
          `aria-label` = "Escalas de color"
        ),
        div(class = "dropdown-menu kb-scale-menu",
            tags$strong("ETAPA DE LA ENTIDAD"),
            lapply(names(.colores_etapa_crm), function(etapa) {
              div(tags$span(class = "kb-scale-swatch", style = paste0("background:", .colores_etapa_crm[[etapa]])),
                  etapa)
            }),
            tags$hr(),
            tags$strong("PRIORIDAD DE TAREAS"),
            lapply(names(.colores_prioridad_crm), function(prioridad) {
              div(tags$span(class = "kb-scale-swatch",
                            style = paste0("background:", .colores_prioridad_crm[[prioridad]])), prioridad)
            })))
  )),
  rightUi = tagList(
    if (.MenuIndicadoresCorporativo()) {
      racafeModulos::MenuHeaderUI("MenuIndicadores", icon = icon("arrow-trend-up"),
                                  min_width = "400px", title = "Indicadores")
    } else {
      tags$li(class = "nav-item dropdown", .boton_header("abrir_indicadores", "Indicadores", "arrow-trend-up"))
    },
    .SeguimientoHeaderUI()
  )
)
