# Navegación ----

.sidebar_crm <- function() {
  iconos_vista <- c(resumen = "gauge-high", kanban = "columns", contactos = "address-book", prospectos = "handshake",
                    leads = "bullseye", prospectos_clientes = "users", descartados = "ban",
                    conversiones = "funnel-dollar", oportunidades = "bullseye", actividades = "clipboard-list",
                    tareas = "list-check", notas = "tasks", cohortes = "people-arrows")
  grupos <- lapply(seq_along(.grupos), function(i) {
    grupo <- .grupos[[i]]
    nombre <- names(.grupos)[i]
    hijos <- lapply(names(grupo), function(id) {
      simbolo <- unname(iconos_vista[id])
      bs4SidebarMenuSubItem(grupo[[id]]$titulo, tabName = id, icon = if (!is.na(simbolo)) icon(simbolo))
    })
    if (nombre == "Generalidades") {
      hijos <- c(hijos, list(bs4SidebarMenuSubItem("Comparación de Indicadores", tabName = "comparacion_indicadores",
                                                   icon = icon("chart-bar"))))
    }
    if (nombre == "Gestión comercial") {
      hijos <- c(list(bs4SidebarMenuSubItem("Presupuesto", tabName = "presupuesto_cero",
                                           icon = icon("money-bill-trend-up"))), hijos)
    }
    do.call(bs4SidebarMenuItem, c(list(text = nombre, icon = icon(.iconos[i]), selected = FALSE), hijos))
  })
  productos <- bs4SidebarMenuItem("Productos", icon = icon("boxes-stacked"), tabName = "analisis_productos")
  grupos <- append(grupos, list(productos), after = 4L)
  inicio <- bs4SidebarMenuItem("Inicio", tabName = "inicio", icon = icon("house"), selected = TRUE)
  bs4DashSidebar(status = "danger", expandOnHover = FALSE,
                 do.call(bs4SidebarMenu, c(list(id = "navegacion"), list(inicio), grupos)))
}
