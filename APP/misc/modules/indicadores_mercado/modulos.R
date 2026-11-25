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

# La unidad conserva un estilo neutro incluso dentro de una celda con tendencia.
.IndicadorCelda <- function(valor) {
  if (is.na(valor) || !nzchar(valor)) return("")
  separador <- regexpr(" [^ ]+/[^ ]+$", valor)
  if (is.na(separador) || separador < 1L) return(htmltools::htmlEscape(valor))
  as.character(shiny::tagList(substr(valor, 1L, separador - 1L), " ",
                             shiny::tags$strong(class = "crm-indicador-unidad font-weight-bold",
                                                style = "font-size:70%;", substring(valor, separador + 1L))))
}

.TablaIndicadoresHTML <- function(datos, colores, encabezado = TRUE) {
  html <- shiny::tags
  shiny::div(class = "crm-tabla table-responsive",
             html$table(class = "table table-sm",
                        if (encabezado) html$thead(html$tr(lapply(names(datos), function(nombre) {
                          html$th(style = "border-bottom:1px solid #495057;", nombre)
                        }))),
                        html$tbody(lapply(seq_len(nrow(datos)), function(i) {
                          html$tr(lapply(seq_len(ncol(datos)), function(j) {
                            html$td(style = if (!is.na(colores[i, j])) paste0("color:", colores[i, j]),
                                    if (j == 1L) datos[[j]][i] else shiny::HTML(.IndicadorCelda(datos[[j]][i])))
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
  gt::text_transform(tabla, locations = gt::cells_body(columns = -1L), fn = function(x) {
    vapply(x, .IndicadorCelda, character(1))
  })
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
  shiny::tagList(
    shiny::div(class = "d-flex flex-wrap align-items-end",
               style = "gap:12px;margin-bottom:16px;",
               shiny::div(style = "width:220px;max-width:100%;",
                          fecha("fecha_anterior", "Fecha inicial", .HoyIndicadores() - 1)),
               shiny::div(style = "width:220px;max-width:100%;",
                          fecha("fecha_actual", "Fecha final", .HoyIndicadores())),
               shiny::div(class = "d-flex align-items-center", style = "height:38px;",
                          shiny::tagAppendAttributes(
                            racafeForms::racafeButton("actualizar", label = NULL, icon = "arrows-rotate",
                                                      title = "Actualizar indicadores", variant = "primary",
                                                      size = "xs", ns = ns),
                            `aria-label` = "Actualizar indicadores"
                          ))),
    .CargaIndicadoresUI(ns, shiny::tagList(tabla, shiny::uiOutput(ns("estado"))))
  )
}

.MetodologiaComparacionIndicadores <- function(valor, id) {
  inicio <- .formato_fecha_crm(valor$inicio)
  fin <- .formato_fecha_crm(valor$fin)
  referencia <- if (isTRUE(valor$vigente)) {
    sprintf("El valor final del %s corresponde a los datos de mercado disponibles al consultar.", fin)
  } else {
    sprintf("El valor final corresponde a los indicadores registrados el %s.", fin)
  }
  puntos <- c(sprintf("El valor inicial corresponde a los indicadores registrados el %s.", inicio), referencia,
              "La diferencia indica cuánto subió o bajó cada indicador en su propia unidad.",
              "La variación expresa ese cambio como porcentaje del valor inicial.",
              "Las celdas quedan vacías cuando faltan datos para mostrar un valor o calcular el cambio.")
  nota <- racafeForms::racafeNoticeList(puntos, title = NULL, font_size = 11,
                                        text_color = "#495057", accent_color = "#DA291C")
  titulo <- "Metodología de la comparación"
  if (.disponible_api("racafeShiny", "racafeCollapsible")) {
    return(racafeShiny::racafeCollapsible(id, titulo, open = FALSE, nota, color = "#6C757D", class = "crm-seccion"))
  }
  shiny::tags$details(id = id, shiny::tags$summary(titulo), nota)
}

.FormatoComparacionIndicadores <- function(datos, inicio, fin) {
  porcentaje <- ifelse(is.finite(datos$Variacion_Porcentual),
                        paste0(formatC(100 * datos$Variacion_Porcentual, digits = 2, format = "f"), "%"), "")
  tabla <- data.frame("Indicador" = datos$Nombre_Indicador,
                      "Valor inicial" = .FormatearIndicadores(datos$Valor_Anterior, datos$Indicador),
                      "Valor final" = .FormatearIndicadores(datos$Valor_Actual, datos$Indicador),
                      "Diferencia" = .FormatearIndicadores(datos$Diferencia_Absoluta, datos$Indicador),
                      "Variación" = porcentaje, check.names = FALSE)
  names(tabla)[2:3] <- paste(c("Valor inicial", "Valor final"), .formato_fecha_crm(c(inicio, fin)), sep = " · ")
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
      .MetodologiaComparacionIndicadores(valor, session$ns("metodologia"))
    })
    presentacion <- shiny::reactive({
      valor <- resultado()
      shiny::req(!is.null(valor), !inherits(valor, "error"))
      .FormatoComparacionIndicadores(valor$datos, valor$inicio, valor$fin)
    })
    if (.IndicadoresGT("racafeTableVancouver")) {
      output$tabla <- gt::render_gt({
        vista <- presentacion()
        tabla <- racafeShiny::racafeTableVancouver(vista$datos, header_color = "#495057", font_size = 11, row_padding = 3,
                                                   align = c("left", rep("right", 4L)), min_width = "620px")
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
