# Servidor ----

server <- function(input, output, session) {
  # Datos reactivos ----
  publicaciones <- reactiveVal(list(operacional = NULL, crm = NULL))
  cargando <- reactiveVal(FALSE)
  refrescar <- function() {
    if (isolate(cargando())) return(invisible(NULL))
    cargando(TRUE)
    session$sendCustomMessage("crm-carga", TRUE)
    session$onFlushed(function() {
      on.exit({
        cargando(FALSE)
        session$sendCustomMessage("crm-carga", FALSE)
      })
      publicaciones(.leer_dominios(.raiz_crm))
    }, once = TRUE)
  }
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
  operativo <- reactive(.filtrar_operacional(publicaciones(), filtros()))

  identidad <- toupper(session$user %||% "HCYATE")
  grupos <- toupper(session$groups %||% "ANALÍTICA")

  refrescar_gestion <- function() {
    actualizado <- isolate(publicaciones())
    actualizado$crm <- LeerPublicacionRDS(.raiz_crm, "crm")
    publicaciones(actualizado)
  }
  capturar <- .GestionCapturaServer(publicaciones, .raiz_crm, refrescar_gestion)
  abrir_accion <- function(id, cliente = NULL) {
    tipo <- if (id %in% c("gestion", "tarea")) "tarea" else id
    capturar(tipo, cliente = cliente)
  }

  # Carga diferida ----
  session$onFlushed(refrescar, once = TRUE)

  # Outputs ----
  ## Controlbar ----
  output$rango_embarque <- renderUI({
    pedidos <- .tabla_fuente(publicaciones(), "pedidos")
    meses <- sort(unique(na.omit(pedidos$periodo_cabecera)))
    minimo <- if (length(meses)) as.Date(paste0(meses[1L], "-01"))
    maximo <- if (length(meses)) as.Date(paste0(tail(meses, 1L), "-01"))
    desde <- isolate(input$embarque_desde) %||% minimo %||% .HoyProductos()
    hasta <- isolate(input$embarque_hasta) %||% maximo %||% .HoyProductos()
    racafeForms::racafeFormRow(
      racafeForms::racafeInputMonth("embarque_desde", "Embarque desde", value = desde, min = minimo, max = maximo),
      racafeForms::racafeInputMonth("embarque_hasta", "Embarque hasta", value = hasta, min = minimo, max = maximo),
      responsive = "fixed"
    )
  })
  output$creador_pedido <- renderUI({
    pedidos <- .tabla_fuente(publicaciones(), "pedidos")
    creadores <- sort(unique(na.omit(as.character(pedidos$PdcUsu))))
    creadores <- creadores[nzchar(creadores)]
    racafeForms::racafeFormRow(
      racafeForms::racafeInputSelect("filtro_creador", "Usuario de creación", creadores, multiple = TRUE,
                                    selected = intersect(isolate(input$filtro_creador), creadores), empty_label = "Todos"),
      racafeForms::racafeInputSelect("filtro_asesor", "Asesor", c("Pendiente" = ""), disabled = TRUE),
      responsive = "fixed"
    )
  })

  observeEvent(input$control_tarea, abrir_accion("tarea"))
  observeEvent(input$control_gestion, abrir_accion("gestion"))
  observeEvent(input$control_contacto, abrir_accion("contacto"))
  observeEvent(input$control_oportunidad, abrir_accion("oportunidad"))
  observeEvent(input$control_muestra, abrir_accion("muestra"))

  ## Header ----
  observeEvent(input$actualizar, refrescar())

  output$descarga_clientes <- downloadHandler(
    filename = function() paste0("clientes-diferenciados-", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(ruta) {
      clientes <- .clientes_vista(operativo()$clientes, operativo()$pedidos,
                                 .margenes_filtrados(publicaciones(), operativo()$pedidos))
      if (is.null(clientes)) {
        clientes <- .clientes_vista(data.frame(PerCod = character(), PerRazSoc = character()))
      }
      writexl::write_xlsx(list(Clientes = clientes), path = ruta)
    }
  )

  output$identidad_header <- renderUI({
    tags$span(class = "crm-identidad",
              tags$span(class = "badge badge-secondary", identidad),
              if (length(grupos)) tags$span(`aria-hidden` = "true", "|"),
              lapply(grupos, function(grupo) tags$span(class = "badge badge-light text-dark", grupo)))
  })

  ### Indicadores, notas y tareas ----
  vigentes <- .IndicadoresHeaderServer(input, session)
  .NotasTareasHeaderServer(input, session, publicaciones)

  ## Body ----
  navegar <- function(tab) updatebs4TabItems(session, "navegacion", selected = tab)

  ### Generalidades ----
  VistaServer("resumen", .vistas$resumen, publicaciones, operativo, filtros, abrir_accion, cargando, navegar)
  ComparacionIndicadores("comparacion_indicadores",
                         reactive(identical(input$navegacion, "comparacion_indicadores")), vigentes)

  ### Gestión comercial ----
  .PresupuestoCeroServer("presupuesto_cero", operativo, publicaciones)
  for (id in names(.gestion_vistas)) GestionServer(id, publicaciones, operativo, capturar)

  ### Productos ----
  ProductosServer("analisis_productos", publicaciones, filtros, .raiz_crm, function() {
    refrescar_gestion()
    updatebs4TabItems(session, "navegacion", selected = "oportunidades")
  })

  ### Operación y consultas ----
  for (id in names(.operacion_vistas)) OperacionServer(id, publicaciones, operativo, filtros)
  consultas <- setdiff(names(.vistas), c("resumen", names(.operacion_vistas), names(.gestion_vistas)))
  for (id in consultas) {
    VistaServer(id, .vistas[[id]], publicaciones, operativo, filtros, abrir_accion, cargando, navegar)
  }
}
