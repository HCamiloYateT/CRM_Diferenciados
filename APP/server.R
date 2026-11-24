# Estado de sesión, publicación y filtros compartidos ----

function(input, output, session) {
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
  session$onFlushed(refrescar, once = TRUE)

  filtros <- reactive(list(
    periodo = if (isTRUE(input$usar_periodo) && length(input$periodo) == 2L && !anyNA(input$periodo)) input$periodo,
    pais = input$filtro_pais, destino = input$filtro_destino,
    creador = input$filtro_creador, cliente = input$filtro_cliente, percod = trimws(input$percod %||% ""), estado = input$filtro_estado
  ))
  operativo <- reactive(.filtrar_operacional(publicaciones(), filtros()))

  output$opciones_filtros <- renderUI({
    clientes <- .tabla_fuente(publicaciones(), "clientes")
    pedidos <- .tabla_fuente(publicaciones(), "pedidos")
    lineas <- .tabla_fuente(publicaciones(), "lineas_pedido")
    opciones <- function(id, etiqueta, valores) {
      valores <- valores[!is.na(valores) & nzchar(valores)]
      if (!length(valores)) return(div(class = "crm-filtro-ausente", strong(etiqueta), " · Sin opciones"))
      seleccion <- isolate(input[[id]])
      selectizeInput(id, etiqueta, choices = valores, selected = intersect(seleccion, valores),
                     multiple = TRUE, options = list(placeholder = "Todos"))
    }
    tagList(opciones("filtro_pais", "País de la empresa", sort(unique(clientes$codigo_pais))),
            opciones("filtro_destino", "País de destino", sort(unique(lineas$PaiCodDtn))),
            opciones("filtro_creador", "Usuario que creó el pedido", sort(unique(pedidos$PdcUsu))),
            opciones("filtro_estado", "Estado de pedido", sort(unique(pedidos$estado_original))))
  })
  observeEvent(publicaciones(), {
    clientes <- .tabla_fuente(publicaciones(), "clientes")
    .actualizar_selector_clientes(session, "filtro_cliente", clientes, input$filtro_cliente)
  })
  observeEvent(input$limpiar, {
    updateCheckboxInput(session, "usar_periodo", value = FALSE)
    updateTextInput(session, "percod", value = "")
    for (id in c("filtro_pais", "filtro_destino", "filtro_cliente", "filtro_estado", "filtro_creador")) {
      updateSelectizeInput(session, id, selected = character())
    }
  })

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
  for (id in names(.acciones)) local({
    accion <- id
    observeEvent(input[[paste0("control_", accion)]], abrir_accion(accion))
  })
  .EncabezadoServer(input, output, session, publicaciones, operativo, cargando, refrescar)

  .PresupuestoCeroServer("presupuesto_cero", operativo, publicaciones)
  ProductosServer("analisis_productos", publicaciones, filtros, .raiz_crm, function() {
    refrescar_gestion()
    updatebs4TabItems(session, "navegacion", selected = "oportunidades")
  })

  for (id in names(.vistas)) {
    if (id %in% names(.operacion_vistas)) OperacionServer(id, publicaciones, operativo, filtros) else
      if (id %in% names(.gestion_vistas)) GestionServer(id, publicaciones, operativo, capturar) else
      VistaServer(id, .vistas[[id]], publicaciones, operativo, filtros, abrir_accion, cargando,
                 function(tab) updatebs4TabItems(session, "navegacion", selected = tab))
  }
}
