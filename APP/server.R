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
  filtros <- Filtros("Filtros", publicaciones)
  operativo <- reactive(.filtrar_operacional(publicaciones(), filtros()))

  usuario <- reactive(toupper(session$user %||% "HCYATE"))
  grupo <- reactive(toupper(session$groups %||% "ANALÍTICA"))

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
  CrearNotas("CrearNota", publicaciones, .raiz_crm, refrescar_gestion, usuario = usuario)
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
              tags$span(class = "badge badge-secondary", usuario()),
              if (length(grupo())) tags$span(`aria-hidden` = "true", "|"),
              lapply(grupo(), function(nombre) tags$span(class = "badge badge-light text-dark", nombre)))
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
  TableroTareas("notas", publicaciones, .raiz_crm, refrescar_gestion, capturar)
  TableroTareas("tareas", publicaciones, .raiz_crm, refrescar_gestion, capturar)
  for (id in names(.gestion_vistas)) GestionServer(id, publicaciones, operativo, capturar)

  ### Productos ----
  ProductosServer("analisis_productos", publicaciones, filtros, .raiz_crm, function() {
    refrescar_gestion()
    updatebs4TabItems(session, "navegacion", selected = "oportunidades")
  })

  ### Operación y consultas ----
  for (id in names(.operacion_vistas)) OperacionServer(id, publicaciones, operativo, filtros)
  consultas <- setdiff(names(.vistas), c("resumen", "notas", "tareas", names(.operacion_vistas), names(.gestion_vistas)))
  for (id in consultas) {
    VistaServer(id, .vistas[[id]], publicaciones, operativo, filtros, abrir_accion, cargando, navegar)
  }
}
