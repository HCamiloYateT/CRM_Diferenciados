# Filtros globales ----

FiltrosUI <- function(id) {
  ns <- NS(id)
  racafeForms::racafeForm(
    racafeForms::racafeFormGroup(
      racafeForms::racafeInputDateRange(ns("periodo"), "Fecha de creación del pedido"),
      racafeForms::racafeInputSwitch(ns("creacion_todas"), "Todas", value = TRUE)
    ),
    racafeForms::racafeFormGroup(
      uiOutput(ns("rango_embarque")),
      racafeForms::racafeInputSwitch(ns("embarque_todos"), "Todos", value = TRUE)
    ),
    racafeForms::racafeFormGroup(
      uiOutput(ns("creador_pedido")),
      racafeForms::racafeFormRow(
        racafeForms::racafeInputSelect(ns("filtro_segmento"), "Segmento", c("Pendiente" = ""), disabled = TRUE),
        racafeForms::racafeInputSelect(ns("filtro_categoria"), "Categoría", c("Pendiente" = ""), disabled = TRUE),
        responsive = "fixed"
      ),
      racafeForms::racafeInputSelect(ns("filtro_producto"), "Producto", c("Pendiente" = ""), disabled = TRUE)
    ),
    racafeForms::racafeFormFooter(
      racafeForms::racafeButton(ns("aplicar_filtros"), "Aplicar filtros", icon = "check", variant = "primary")
    ),
    size = "compact", variant = "plain"
  )
}

Filtros <- function(id, publicaciones) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filtros <- reactiveVal(list(percod = ""))
    observeEvent(input$aplicar_filtros, {
      periodo <- input$periodo
      limitar_creacion <- identical(input$creacion_todas, FALSE)
      if (limitar_creacion && (length(periodo) != 2L || anyNA(periodo) || periodo[1L] > periodo[2L])) {
        return(showNotification("Seleccione un intervalo válido de creación.", type = "warning"))
      }
      embarque <- NULL
      if (identical(input$embarque_todos, FALSE)) {
        fechas <- .dp_fecha(as.character(c(input$embarque_desde, input$embarque_hasta)))
        limites <- as.Date(format(fechas, "%Y-%m-01"))
        if (length(limites) != 2L || anyNA(limites) || limites[1L] > limites[2L]) {
          return(showNotification("Seleccione un rango válido de meses de embarque.", type = "warning"))
        }
        embarque <- format(seq(limites[1L], limites[2L], by = "month"), "%Y-%m")
      }
      filtros(list(periodo = if (limitar_creacion) periodo, embarque = embarque,
                   creador = input$filtro_creador, percod = ""))
    })

    output$rango_embarque <- renderUI({
      pedidos <- .tabla_fuente(publicaciones(), "pedidos")
      meses <- sort(unique(na.omit(pedidos$periodo_cabecera)))
      minimo <- if (length(meses)) as.Date(paste0(meses[1L], "-01"))
      maximo <- if (length(meses)) as.Date(paste0(tail(meses, 1L), "-01"))
      desde <- isolate(input$embarque_desde) %||% minimo %||% .HoyProductos()
      hasta <- isolate(input$embarque_hasta) %||% maximo %||% .HoyProductos()
      racafeForms::racafeFormRow(
        racafeForms::racafeInputMonth(ns("embarque_desde"), "Embarque desde", value = desde, min = minimo, max = maximo),
        racafeForms::racafeInputMonth(ns("embarque_hasta"), "Embarque hasta", value = hasta, min = minimo, max = maximo),
        responsive = "fixed"
      )
    })
    output$creador_pedido <- renderUI({
      catalogo <- .asesores_catalogo(publicaciones()$crm$tablas)
      activos <- catalogo[which(catalogo$vigente), , drop = FALSE]
      activos <- activos[order(activos$nombre, activos$usuario), , drop = FALSE]
      opciones <- stats::setNames(activos$usuario, activos$nombre)
      racafeForms::racafeInputSelect(ns("filtro_creador"), "Asesor que creó el pedido", opciones, multiple = TRUE,
                                    selected = intersect(isolate(input$filtro_creador), activos$usuario),
                                    empty_label = "Todos", uppercase = FALSE)
    })

    reactive({
      actual <- filtros()
      catalogo <- .asesores_catalogo(publicaciones()$crm$tablas)
      actual$asesores_inactivos <- catalogo$usuario[!catalogo$vigente]
      actual$creador <- intersect(actual$creador, catalogo$usuario[which(catalogo$vigente)])
      actual
    })
  })
}
