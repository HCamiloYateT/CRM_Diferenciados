# Cuerpo y recursos corporativos versionados ----

.body_crm <- function() {
  recursos <- tags$head(
    tags$link(rel = "stylesheet", href = .estilos_crm),
    tags$link(rel = "stylesheet", href = "crm.css"),
    tags$script(src = "crm.js")
  )
  if (.disponible_api("racafeForms", "racafeFormsDependencies")) {
    recursos <- tagList(recursos, racafeForms::racafeFormsDependencies())
  }
  tabs <- lapply(names(.vistas), function(id) {
    bs4TabItem(tabName = id, VistaUI(id, .vistas[[id]]))
  })
  tabs <- c(tabs, list(bs4TabItem(tabName = "comparacion_indicadores",
                                 ComparacionIndicadoresUI("comparacion_indicadores"))))
  bs4DashBody(recursos, .overlay_crm(), div(class = "crm-corte", uiOutput("estado_corte")),
              do.call(bs4TabItems, tabs))
}
