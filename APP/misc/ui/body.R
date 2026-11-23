# Cuerpo y recursos corporativos versionados ----

.body_crm <- function() {
  recursos <- tags$head(
    tags$link(rel = "stylesheet", href = .estilos_crm),
    tags$link(rel = "stylesheet", href = .estilos_nacional),
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
  tabs <- c(tabs, list(bs4TabItem(tabName = "analisis_productos", ProductosUI("analisis_productos"))))
  tabs <- c(list(bs4TabItem(tabName = "inicio", .InicioDiferenciadosUI())),
             tabs, list(bs4TabItem(tabName = "presupuesto_cero", .PresupuestoCeroUI("presupuesto_cero"))))
  bs4DashBody(recursos, .overlay_crm(), div(class = "crm-corte", uiOutput("estado_corte")),
              do.call(bs4TabItems, tabs))
}
