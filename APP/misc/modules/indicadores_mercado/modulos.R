# Presentación compartida ----

.IndicadoresGT <- function(funcion) {
  .disponible_api("racafeShiny", funcion) && requireNamespace("gt", quietly = TRUE)
}

.CargaIndicadoresUI <- function(ns, contenido) {
  if (.disponible_api("racafeShiny", "racafeOverlayLoader") &&
      .disponible_api("racafeShiny", "racafeUpdateOverlayLoader")) {
    return(racafeShiny::racafeOverlayLoader("carga", contenido, size = 44, ns = ns, triggers = "actualizar"))
  }
  shiny::div(class = "crm-indicadores-carga", shiny::uiOutput(ns("cargando")), contenido)
}

.EstadoCargaIndicadores <- function(session, output, cargando) {
  if (.disponible_api("racafeShiny", "racafeOverlayLoader") &&
      .disponible_api("racafeShiny", "racafeUpdateOverlayLoader")) {
    shiny::observe(racafeShiny::racafeUpdateOverlayLoader(session, "carga", cargando()))
  } else {
    output$cargando <- shiny::renderUI({
      if (cargando()) .nota("Consultando indicadores…")
    })
  }
}

.TablaIndicadoresHTML <- function(datos, colores, encabezado = TRUE) {
  html <- shiny::tags
  shiny::div(class = "crm-tabla table-responsive",
             html$table(class = "table table-sm",
                        if (encabezado) html$thead(html$tr(lapply(names(datos), html$th))),
                        html$tbody(lapply(seq_len(nrow(datos)), function(i) {
                          html$tr(lapply(seq_len(ncol(datos)), function(j) {
                            html$td(style = if (!is.na(colores[i, j])) paste0("color:", colores[i, j]),
                                    datos[[j]][i])
                          }))
                        }))))
}

.ColorearIndicadoresGT <- function(tabla, datos, colores) {
  for (j in seq_len(ncol(datos))) {
    for (color in unique(na.omit(colores[, j]))) {
      tabla <- gt::tab_style(tabla, gt::cell_text(color = color, weight = "bold"),
                             locations = gt::cells_body(columns = tidyselect::all_of(names(datos)[j]),
                                                        rows = which(colores[, j] == color)))
    }
  }
  tabla
}

# Comparación histórica ----

ComparacionIndicadoresUI <- function(id) {
  ns <- shiny::NS(id)
  fecha <- function(id, etiqueta, valor) {
    if (.disponible_api("racafeForms", "racafeInputDate")) {
      return(racafeForms::racafeInputDate(id, label = etiqueta, value = valor, ns = ns))
    }
    shiny::dateInput(ns(id), etiqueta, value = valor, max = .HoyIndicadores(), language = "es", format = "dd/mm/yyyy")
  }
  tabla <- if (.IndicadoresGT("racafeTableVancouver")) {
    gt::gt_output(ns("tabla"))
  } else shiny::uiOutput(ns("tabla"))
  shiny::div(class = "crm-vista", shiny::h1("Comparación de Indicadores"),
             shiny::div(class = "crm-indicadores-filtros", fecha("fecha_anterior", "Fecha inicial", .HoyIndicadores() - 1),
                        fecha("fecha_actual", "Fecha final", .HoyIndicadores()),
                        .boton(ns("actualizar"), "Actualizar", "rotate")),
             .CargaIndicadoresUI(ns, shiny::div(class = "crm-panel", tabla, shiny::uiOutput(ns("estado")))))
}

.FormatoComparacionIndicadores <- function(datos) {
  porcentaje <- ifelse(is.finite(datos$Variacion_Porcentual),
                        paste0(formatC(100 * datos$Variacion_Porcentual, digits = 2, format = "f"), "%"), "—")
  tabla <- data.frame("Indicador" = datos$Nombre_Indicador,
                      "Valor inicial" = .FormatearIndicadores(datos$Valor_Anterior, datos$Indicador),
                      "Valor final" = .FormatearIndicadores(datos$Valor_Actual, datos$Indicador),
                      "Diferencia" = .FormatearIndicadores(datos$Diferencia_Absoluta, datos$Indicador),
                      "Variación" = porcentaje, check.names = FALSE)
  color <- function(x) ifelse(x > 0, .IND_COLORES_TENDENCIA[["sube"]],
                              ifelse(x < 0, .IND_COLORES_TENDENCIA[["baja"]], NA_character_))
  colores <- cbind(matrix(NA_character_, nrow(tabla), 3L), color(datos$Diferencia_Absoluta),
                  color(datos$Variacion_Porcentual))
  list(datos = tabla, colores = colores)
}

ComparacionIndicadores <- function(id, activo, vigentes) {
  shiny::moduleServer(id, function(input, output, session) {
    resultado <- shiny::reactiveVal(NULL)
    cargando <- shiny::reactiveVal(FALSE)
    .EstadoCargaIndicadores(session, output, cargando)
    consultar <- function(forzar = FALSE) {
      if (shiny::isolate(cargando())) return(invisible(NULL))
      inicio <- shiny::isolate(input$fecha_anterior)
      fin <- shiny::isolate(input$fecha_actual)
      shiny::req(inicio, fin)
      cargando(TRUE)
      session$onFlushed(function() {
        on.exit(cargando(FALSE))
        resultado(tryCatch(.ConsultarComparacionIndicadores(inicio, fin, vigentes = function() {
          vigentes(refrescar = forzar)
        }), error = function(e) {
          message("Comparación de indicadores: ", conditionMessage(e))
          e
        }))
      }, once = TRUE)
    }
    shiny::observeEvent(list(activo(), input$fecha_anterior, input$fecha_actual), {
      if (isTRUE(activo()) && is.null(resultado())) consultar()
    })
    shiny::observeEvent(input$actualizar, consultar(forzar = TRUE))
    output$estado <- shiny::renderUI({
      valor <- resultado()
      if (inherits(valor, "error")) {
        mensaje <- conditionMessage(valor)
        if (!grepl("^(Seleccione fechas válidas|Sin indicadores)", mensaje)) {
          mensaje <- "No fue posible consultar los indicadores para estas fechas."
        }
        return(.nota(mensaje, "error"))
      }
      if (is.null(valor)) return(NULL)
      .nota(paste("Fecha inicial", format(valor$inicio, "%d/%m/%Y"), "· CRMINDICADORES.",
                  "Fecha final", format(valor$fin, "%d/%m/%Y"), "·", paste0(valor$fuente, ".")))
    })
    presentacion <- shiny::reactive({
      valor <- resultado()
      shiny::req(!is.null(valor), !inherits(valor, "error"))
      .FormatoComparacionIndicadores(valor$datos)
    })
    if (.IndicadoresGT("racafeTableVancouver")) {
      output$tabla <- gt::render_gt({
        vista <- presentacion()
        tabla <- racafeShiny::racafeTableVancouver(vista$datos, font_size = 11, row_padding = 3, min_width = "620px")
        .ColorearIndicadoresGT(tabla, vista$datos, vista$colores)
      })
    } else {
      output$tabla <- shiny::renderUI({
        vista <- presentacion()
        .TablaIndicadoresHTML(vista$datos, vista$colores)
      })
    }
  })
}
