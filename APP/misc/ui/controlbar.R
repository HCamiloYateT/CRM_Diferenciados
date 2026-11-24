# Controlbar ----

controlbar <- bs4DashControlbar(
  id = "controlbar", skin = "light", pinned = NULL, overlay = FALSE, width = "450px",
  controlbarMenu(
    id = "controlbarMenu", type = "tabs",
    controlbarItem("Filtros", FiltrosUI("Filtros")),
    controlbarItem("Tareas y notas", CrearNotasUI("CrearNota")),
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
