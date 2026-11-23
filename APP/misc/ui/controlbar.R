# Filtros y acceso a las mismas acciones ----

.controlbar_crm <- function() {
  bs4DashControlbar(id = "controlbar", skin = "light", overlay = TRUE, width = "400px",
                   div(class = "crm-control",
                       h2("Filtros"),
                       checkboxInput("usar_periodo", "Limitar por fecha de creación"),
                       conditionalPanel("input.usar_periodo",
                                        dateRangeInput("periodo", NULL, format = "dd/mm/yyyy", language = "es")),
                       uiOutput("opciones_filtros"),
                       textInput("percod", "PerCod exacto"),
                       .boton("limpiar", "Restablecer filtros", "eraser"),
                       tags$details(tags$summary("Filtros pendientes"), tags$ul(lapply(
                         c("Comercial", "Etapa comercial", "Tipo de comprador", "Estado de muestra",
                           "Tipo de oportunidad", "Cuenta padre", "Tier", "Categoría y producto homologado"),
                         tags$li))),
                       h2("Acciones rápidas"),
                       div(class = "crm-acciones-grid", lapply(names(.acciones), function(id) {
                         .boton(paste0("control_", id), .acciones[[id]]$titulo, "plus")
                       }))))
}
