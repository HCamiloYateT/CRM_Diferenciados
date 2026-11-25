.MenuIndicadoresCorporativo <- function() {
  .disponible_api("racafeModulos", "MenuHeaderUI") && .disponible_api("racafeModulos", "MenuHeaderServer")
}

# Menú del encabezado ----

TablaIndicadoresUI <- function(id) {
  ns <- shiny::NS(id)
  tabla <- if (.IndicadoresGT("racafeCompactTable")) gt::gt_output(ns("tabla")) else shiny::uiOutput(ns("tabla"))
  contenido <- racafeForms::racafeForm(
    shiny::tags$h6("Indicadores de mercado"), tabla, shiny::uiOutput(ns("estado")),
    racafeForms::racafeFormFooter(
      racafeForms::racafeButton("actualizar", "Actualizar", icon = "sync", variant = "primary", size = "xxs",
                                ns = ns, title = "Actualizar indicadores"),
      align = "right"
    ),
    size = "compact", variant = "plain"
  )
  .CargaIndicadoresUI(ns, contenido)
}

TablaIndicadores <- function(id, abierto, vigentes) {
  shiny::moduleServer(id, function(input, output, session) {
    resultado <- shiny::reactiveVal(NULL)
    cargando <- shiny::reactiveVal(FALSE)
    .EstadoCargaIndicadores(session, output, cargando)
    consultar <- function(forzar = FALSE) {
      if (shiny::isolate(cargando())) return(invisible(NULL))
      cargando(TRUE)
      session$onFlushed(function() {
        on.exit(cargando(FALSE))
        resultado(tryCatch({
          actuales <- vigentes(refrescar = forzar)
          if (!any(is.finite(actuales$Valor))) stop("Sin indicadores vigentes disponibles.", call. = FALSE)
          anterior <- tryCatch(.CargarAnteriorIndicadores(), error = function(e) {
            message("Histórico de indicadores: ", conditionMessage(e))
            NULL
          })
          list(datos = .PrepararTendenciaIndicadores(actuales, anterior), hora = Sys.time(),
               sin_historico = is.null(anterior) || !nrow(anterior))
        }, error = function(e) {
          message("Indicadores vigentes: ", conditionMessage(e))
          e
        }))
      }, once = TRUE)
    }
    shiny::observeEvent(TRUE, consultar(), once = TRUE)
    shiny::observeEvent(abierto(), {
      if (is.null(resultado())) consultar()
    })
    shiny::observeEvent(input$actualizar, consultar(forzar = TRUE))
    output$estado <- shiny::renderUI({
      valor <- resultado()
      if (inherits(valor, "error")) return(.nota("No fue posible consultar los indicadores vigentes.", "error"))
      if (is.null(valor)) return(NULL)
      hora <- .formato_fecha_crm(valor$hora)
      .nota(paste("Consulta:", hora, if (valor$sin_historico) "· Sin referencia histórica del día anterior."))
    })
    presentacion <- shiny::reactive({
      valor <- resultado()
      shiny::req(!is.null(valor), !inherits(valor, "error"))
      datos <- valor$datos[c("Item", "Valor")]
      claves <- names(.IND_NOMBRES_CORTOS)[match(datos$Item, .IND_NOMBRES_CORTOS)]
      datos$Item <- unname(.IND_NOMBRES_VISIBLES[claves])
      colores <- cbind(NA_character_, unname(.IND_COLORES_TENDENCIA[valor$datos$tendencia]))
      list(datos = datos, colores = colores)
    })
    if (.IndicadoresGT("racafeCompactTable")) {
      output$tabla <- gt::render_gt({
        vista <- presentacion()
        tabla <- racafeShiny::racafeCompactTable(vista$datos, backend = "gt", show_header = FALSE,
                                                 font_size = 12, row_padding = 4, align = c("left", "right"))
        .ColorearIndicadoresGT(tabla, vista$datos, vista$colores)
      })
    } else {
      output$tabla <- shiny::renderUI({
        vista <- presentacion()
        .TablaIndicadoresHTML(vista$datos, vista$colores, encabezado = FALSE)
      })
    }
  })
}

# Indicadores vigentes y comparación ----

.IndicadoresHeaderServer <- function(input, session) {
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
  vigentes
}
