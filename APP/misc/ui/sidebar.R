# Navegación ----

.sidebar_crm <- function() {
  grupos <- lapply(seq_along(.grupos), function(i) {
    grupo <- .grupos[[i]]
    if (length(grupo) == 1L) return(bs4SidebarMenuItem("Inicio", tabName = "inicio", icon = icon(.iconos[i])))
    hijos <- lapply(names(grupo), function(id) bs4SidebarMenuSubItem(grupo[[id]]$titulo, tabName = id))
    do.call(bs4SidebarMenuItem, c(list(text = names(.grupos)[i], icon = icon(.iconos[i])), hijos))
  })
  generalidades <- bs4SidebarMenuItem("Generalidades", icon = icon("chart-simple"),
                                        bs4SidebarMenuSubItem("Comparación de Indicadores",
                                                              tabName = "comparacion_indicadores"))
  grupos <- append(grupos, list(generalidades), after = 1L)
  bs4DashSidebar(skin = "dark", status = "danger", fixed = TRUE,
                 do.call(bs4SidebarMenu, c(list(id = "navegacion"), grupos)))
}
