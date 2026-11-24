# Controlbar ----

controlbar <- bs4DashControlbar(
  id = "controlbar", skin = "light", pinned = NULL, overlay = FALSE, width = "450px",
  controlbarMenu(
    id = "controlbarMenu", type = "tabs",
    controlbarItem(
      "Filtros",
      racafeForms::racafeForm(
        racafeForms::racafeFormGroup(
          racafeForms::racafeInputDateRange("periodo", "Fecha de creación del pedido"),
          racafeForms::racafeInputSwitch("creacion_todas", "Todas", value = TRUE)
        ),
        racafeForms::racafeFormGroup(
          uiOutput("rango_embarque"),
          racafeForms::racafeInputSwitch("embarque_todos", "Todos", value = TRUE)
        ),
        racafeForms::racafeFormGroup(
          uiOutput("creador_pedido"),
          racafeForms::racafeFormRow(
            racafeForms::racafeInputSelect("filtro_segmento", "Segmento", c("Pendiente" = ""), disabled = TRUE),
            racafeForms::racafeInputSelect("filtro_categoria", "Categoría", c("Pendiente" = ""), disabled = TRUE),
            responsive = "fixed"
          ),
          racafeForms::racafeInputSelect("filtro_producto", "Producto", c("Pendiente" = ""), disabled = TRUE)
        ),
        racafeForms::racafeFormFooter(
          racafeForms::racafeButton("aplicar_filtros", "Aplicar filtros", icon = "check", variant = "primary")
        ),
        size = "compact", variant = "plain"
      )
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
