# Cuerpo de Diferenciados ----

.body_crm <- function() {
  tabs <- lapply(names(.vistas), function(id) {
    vista <- .vistas[[id]]
    contenido <- if (id == "resumen") .ResumenEjecutivoUI(id) else
      if (id == "notas") TableroTareasUI(id) else
      if (id == "catalogos") tagList(.AsesoresUI(id), VistaUI(id, vista)) else
      if (id %in% names(.operacion_vistas)) OperacionUI(id) else
      if (id %in% names(.gestion_vistas)) GestionUI(id, vista$titulo) else VistaUI(id, vista)
    bs4TabItem(tabName = id, .formulario_crm(contenido))
  })
  tabs <- c(tabs, list(bs4TabItem(tabName = "comparacion_indicadores",
                                 .formulario_crm(ComparacionIndicadoresUI("comparacion_indicadores")))))
  tabs <- c(tabs, list(bs4TabItem(tabName = "analisis_productos", .formulario_crm(ProductosUI("analisis_productos")))))
  tabs <- c(list(bs4TabItem(tabName = "inicio", .InicioDiferenciadosUI())),
             tabs, list(bs4TabItem(tabName = "presupuesto_cero", .PresupuestoCeroUI("presupuesto_cero"))))
  bs4DashBody(.recursos_compartidos, .overlay_crm, do.call(bs4TabItems, tabs))
}
