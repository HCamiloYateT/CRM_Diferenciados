# Controlbar ----

controlbar <- bs4DashControlbar(
  id = "controlbar", skin = "light", pinned = NULL, overlay = FALSE, width = "450px",
  controlbarMenu(
    id = "controlbarMenu", type = "tabs",
    controlbarItem(
      "Filtros",
      div(class = "crm-control",
          checkboxInput("usar_periodo", "Limitar por fecha de creación"),
          conditionalPanel("input.usar_periodo",
                           dateRangeInput("periodo", NULL, format = "dd/mm/yyyy", language = "es")),
          uiOutput("opciones_filtros"),
          .selector_clientes("filtro_cliente", "Cliente", multiple = TRUE, placeholder = "Todos"),
          textInput("percod", "PerCod exacto"),
          .boton("limpiar", "Restablecer filtros", "eraser"),
          tags$details(tags$summary("Filtros pendientes"),
                       tags$ul(tags$li("Comercial"), tags$li("Etapa comercial"), tags$li("Tipo de comprador"),
                               tags$li("Estado de muestra"), tags$li("Tipo de oportunidad"), tags$li("Tier"),
                               tags$li("Categoría y producto homologado"))))
    ),
    controlbarItem(
      "Tareas y notas",
      div(class = "crm-control",
          .boton("control_tarea", .acciones$tarea$titulo, "plus"),
          .boton("control_gestion", .acciones$gestion$titulo, "plus"))
    ),
    controlbarItem(
      "Contactos",
      div(class = "crm-control", .boton("control_contacto", .acciones$contacto$titulo, "plus"))
    ),
    controlbarItem(
      "Oportunidades",
      div(class = "crm-control", .boton("control_oportunidad", .acciones$oportunidad$titulo, "plus"))
    ),
    controlbarItem(
      "Muestras",
      div(class = "crm-control", .boton("control_muestra", .acciones$muestra$titulo, "plus"))
    )
  )
)
