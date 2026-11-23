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
  observeEvent(input$actualizar, refrescar())

  filtros <- reactive(list(
    periodo = if (isTRUE(input$usar_periodo) && length(input$periodo) == 2L && !anyNA(input$periodo)) input$periodo,
    pais = input$filtro_pais, destino = input$filtro_destino,
    cliente = input$filtro_cliente, percod = trimws(input$percod %||% ""), estado = input$filtro_estado
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
    nombres <- if (is.null(clientes)) character() else {
      rotulos <- ifelse(is.na(clientes$PerRazSoc), clientes$PerCod, paste(clientes$PerCod, clientes$PerRazSoc))
      setNames(clientes$PerCod, rotulos)
    }
    tagList(opciones("filtro_pais", "País de la empresa", sort(unique(clientes$codigo_pais))),
            opciones("filtro_destino", "País de destino", sort(unique(lineas$PaiCodDtn))),
            opciones("filtro_cliente", "Cliente", nombres),
            opciones("filtro_estado", "Estado de pedido", sort(unique(pedidos$estado_original))))
  })
  observeEvent(input$limpiar, {
    updateCheckboxInput(session, "usar_periodo", value = FALSE)
    updateTextInput(session, "percod", value = "")
    for (id in c("filtro_pais", "filtro_destino", "filtro_cliente", "filtro_estado")) {
      updateSelectizeInput(session, id, selected = character())
    }
  })

  output$estado_corte <- renderUI({
    if (cargando()) return(tags$span("Cargando publicación…"))
    publicacion <- publicaciones()$operacional
    corte <- publicacion$manifiesto$procedencia$corte
    texto <- if (inherits(publicacion, "error")) "No se pudo leer la publicación" else if (length(corte)) {
      paste("Corte operacional", format(as.Date(corte), "%d/%m/%Y"))
    } else "Sin publicación operacional"
    tags$p(texto)
  })

  abrir_accion <- function(id) showModal(.formulario_accion(id))
  for (id in names(.acciones)) local({
    accion <- id
    observeEvent(input[[paste0("control_", accion)]], abrir_accion(accion))
    observeEvent(input[[paste0("header_", accion)]], abrir_accion(accion))
  })
  output$acciones_clientes <- renderUI({
    clientes <- operativo()$clientes
    descarga <- if (!is.null(clientes) && nrow(clientes)) {
      if (.disponible_api("racafeForms", "racafeButtonDownload")) {
        racafeForms::racafeButtonDownload("descarga_clientes", label = NULL, icon = "file-csv", size = "xxs")
      } else downloadButton("descarga_clientes", "CSV", class = "btn-sm btn-outline-secondary")
    }
    tagList(.boton_header("abrir_clientes", "Directorio de clientes", "users"),
            if (!is.null(descarga)) shiny::tagAppendAttributes(descarga, title = "Descargar clientes",
                                                             `aria-label` = "Descargar clientes"))
  })
  observeEvent(input$abrir_clientes, updatebs4TabItems(session, "navegacion", selected = "directorio"))
  output$descarga_clientes <- downloadHandler(
    filename = function() paste0("clientes-diferenciados-", Sys.Date(), ".csv"),
    content = function(ruta) {
      clientes <- operativo()$clientes
      req(!is.null(clientes), nrow(clientes))
      # Los nombres de origen pueden comenzar por caracteres que una hoja de cálculo ejecutaría.
      clientes[] <- lapply(clientes, function(x) {
        if (!is.character(x)) return(x)
        ifelse(!is.na(x) & grepl("^[[:space:]]*[=+@-]", x), paste0("'", x), x)
      })
      utils::write.csv(clientes, ruta, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )
  observeEvent(input$notificaciones, {
    showModal(modalDialog(title = "Notificaciones", .nota("Pendiente de fuente de alertas.", "muted"),
                          footer = modalButton("Cerrar")))
  })
  output$identidad_header <- renderUI({
    identidad <- session$user %||% "Usuario"
    tags$button(id = "usuario", type = "button", class = "action-button crm-identidad", title = "Información de sesión",
                identidad)
  })
  observeEvent(input$usuario, {
    identidad <- session$user %||% "Sesión sin identidad institucional"
    showModal(modalDialog(title = "Usuario", tags$p(identidad), uiOutput("estado_corte"), tags$p("Línea 20000"),
                          footer = modalButton("Cerrar")))
  })

  observeEvent(input$glosario, showModal(.GlosarioDiferenciadosUI()))

  mercado <- reactiveVal(NULL)
  vigentes <- function(refrescar = FALSE) {
    valor <- isolate(mercado())
    hoy <- .HoyIndicadores()
    if (!refrescar && !is.null(valor) && identical(valor$fecha, hoy)) return(valor$datos)
    datos <- .ObtenerIndicadoresActuales()
    mercado(if (any(is.finite(datos$Valor))) list(fecha = hoy, datos = datos) else NULL)
    datos
  }
  abrir_comparacion <- function() {
    updatebs4TabItems(session, "navegacion", selected = "comparacion_indicadores")
  }
  if (.MenuIndicadoresCorporativo()) {
    racafeModulos::MenuHeaderServer("MenuAccionesCRM", items_r = .acciones_header_ui(), showHeader = FALSE)
    menu <- racafeModulos::MenuHeaderServer("MenuIndicadores", items_r = TablaIndicadoresUI("ind_kpis"),
                                            href = "#", footerText = "Comparación de Indicadores")
    abierto <- reactive(input[["MenuIndicadores-open"]])
    observeEvent(menu$footer_click(), abrir_comparacion())
  } else {
    abierto <- reactive(input$abrir_indicadores)
    observeEvent(input$abrir_indicadores, {
      showModal(modalDialog(title = "Indicadores", TablaIndicadoresUI("ind_kpis"),
                            footer = tagList(.boton("comparar_mercado", "Comparación de Indicadores", "chart-line"),
                                             modalButton("Cerrar"))))
    })
    observeEvent(input$comparar_mercado, {
      removeModal()
      abrir_comparacion()
    })
  }
  TablaIndicadores("ind_kpis", abierto, vigentes)
  ComparacionIndicadores("comparacion_indicadores",
                         reactive(identical(input$navegacion, "comparacion_indicadores")), vigentes)

  .PresupuestoCeroServer("presupuesto_cero", operativo, publicaciones)
  ProductosServer("analisis_productos", publicaciones, filtros, .raiz_crm, function() {
    actualizado <- isolate(publicaciones())
    actualizado$crm <- LeerPublicacionRDS(.raiz_crm, "crm")
    publicaciones(actualizado)
    updatebs4TabItems(session, "navegacion", selected = "oportunidades")
  })

  for (id in names(.vistas)) VistaServer(id, .vistas[[id]], publicaciones, operativo, filtros, abrir_accion, cargando)
}
