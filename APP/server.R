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
    if (isTRUE(input$usar_periodo) && (length(periodo) != 2L || anyNA(periodo) || periodo[1L] > periodo[2L])) {
      return(showNotification("Seleccione un intervalo válido de creación.", type = "warning"))
    }
    filtros(list(periodo = if (isTRUE(input$usar_periodo)) periodo,
                 embarque = input$filtro_embarque, creador = input$filtro_creador,
                 cliente = input$filtro_cliente, percod = trimws(input$percod %||% "")))
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
  output$opciones_filtros <- renderUI({
    pedidos <- .tabla_fuente(publicaciones(), "pedidos")
    meses <- sort(unique(na.omit(as.character(pedidos$periodo_cabecera))), decreasing = TRUE)
    creadores <- sort(unique(na.omit(as.character(pedidos$PdcUsu))))
    creadores <- creadores[nzchar(creadores)]
    opciones <- function(id, etiqueta, valores) {
      seleccion <- isolate(input[[id]])
      racafeForms::racafeInputSelect(id, etiqueta, valores, selected = intersect(seleccion, valores),
                                    multiple = TRUE, empty_label = "Todos")
    }
    racafeForms::racafeFormRow(
      opciones("filtro_embarque", "Mes de embarque", setNames(meses, .formato_fecha_crm(meses, mes = TRUE))),
      opciones("filtro_creador", "Usuario de creación del pedido", creadores)
    )
  })
  observeEvent(publicaciones(), {
    clientes <- .tabla_fuente(publicaciones(), "clientes")
    .actualizar_selector_clientes(session, "filtro_cliente", clientes, input$filtro_cliente)
  })
  observeEvent(input$limpiar, {
    filtros(list(percod = ""))
    shinyWidgets::updateMaterialSwitch(session, "usar_periodo", value = FALSE)
    updateTextInput(session, "percod", value = "")
    for (id in c("filtro_embarque", "filtro_creador")) {
      shinyWidgets::updatePickerInput(session, id, selected = character())
    }
    updateSelectizeInput(session, "filtro_cliente", selected = character())
  })

  observeEvent(input$control_tarea, abrir_accion("tarea"))
  observeEvent(input$control_gestion, abrir_accion("gestion"))
  observeEvent(input$control_contacto, abrir_accion("contacto"))
  observeEvent(input$control_oportunidad, abrir_accion("oportunidad"))
  observeEvent(input$control_muestra, abrir_accion("muestra"))

  ## Header ----
  observeEvent(input$actualizar, refrescar())

  output$estado_corte <- renderUI({
    if (cargando()) return(tags$span("Cargando publicación…"))
    publicacion <- publicaciones()$operacional
    corte <- publicacion$manifiesto$procedencia$corte
    texto <- if (inherits(publicacion, "error")) "No se pudo leer la publicación" else if (length(corte)) {
      paste("Corte operacional", .formato_fecha_crm(as.Date(corte)))
    } else "Sin publicación operacional"
    tags$p(texto)
  })

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
    tags$button(id = "usuario", type = "button", class = "action-button crm-identidad",
                title = "Información de sesión",
                tags$span(class = "badge badge-secondary", identidad),
                if (length(grupos)) tags$span(`aria-hidden` = "true", "|"),
                lapply(grupos, function(grupo) tags$span(class = "badge badge-light text-dark", grupo)))
  })
  observeEvent(input$usuario, {
    showModal(modalDialog(title = "Usuario y grupos", tags$p(identidad),
                          tags$p(paste(grupos, collapse = ", ")), uiOutput("estado_corte"),
                          footer = modalButton("Cerrar")))
  })

  observeEvent(input$glosario, showModal(.GlosarioDiferenciadosUI()))

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
