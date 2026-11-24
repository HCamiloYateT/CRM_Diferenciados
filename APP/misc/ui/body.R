# Cuerpo y recursos corporativos versionados ----

.recursos_crm <- function() {
  incluir <- function(recurso, funcion) {
    ruta <- if (nzchar(.compartido_local)) file.path(.compartido_local, recurso) else
      paste0(.compartido, "/", recurso)
    tryCatch(shiny::tags$head(funcion(ruta)), error = function(e) {
      message("Recurso de Compartido no disponible: ", recurso)
      NULL
    })
  }
  scripts <- c("clipboard.js", "forms.js", "controlbar.js", "interactions.js")
  shiny::singleton(shiny::tagList(
    shiny::tags$span(hidden = NA, `data-rf-interactions` = "true"),
    if (.disponible_api("racafeForms", "racafeFormsDependencies")) racafeForms::racafeFormsDependencies(),
    if (.disponible_api("racafeModulos", "MenuAccionesDependencies")) racafeModulos::MenuAccionesDependencies(),
    incluir("Styles/style.css", shiny::includeCSS),
    lapply(paste0("Scripts/", scripts), incluir, shiny::includeScript),
    incluir("Styles/crm-nacional.css", shiny::includeCSS),
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", href = "crm.css"), shiny::tags$script(src = "crm.js"))
  ))
}

.body_crm <- function() {
  tabs <- lapply(names(.vistas), function(id) {
    vista <- .vistas[[id]]
    contenido <- if (id %in% names(.gestion_vistas)) GestionUI(id, vista$titulo) else VistaUI(id, vista)
    bs4TabItem(tabName = id, contenido)
  })
  tabs <- c(tabs, list(bs4TabItem(tabName = "comparacion_indicadores",
                                 ComparacionIndicadoresUI("comparacion_indicadores"))))
  tabs <- c(tabs, list(bs4TabItem(tabName = "analisis_productos", ProductosUI("analisis_productos"))))
  tabs <- c(list(bs4TabItem(tabName = "inicio", .InicioDiferenciadosUI())),
             tabs, list(bs4TabItem(tabName = "presupuesto_cero", .PresupuestoCeroUI("presupuesto_cero"))))
  bs4DashBody(.recursos_crm(), .overlay_crm(), do.call(bs4TabItems, tabs))
}
