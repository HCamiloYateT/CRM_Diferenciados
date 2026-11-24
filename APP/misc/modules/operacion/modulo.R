# Visualización de hechos de Diferenciados ----

OperacionUI <- function(id) {
  ns <- shiny::NS(id)
  tipo <- unname(.operacion_vistas[id])
  contenido <- switch(tipo,
    Trazabilidad = tagList(.selector_clientes(ns("pedido"), "Buscar pedido por compañía o número"),
      uiOutput(ns("cabecera")), bs4Dash::tabsetPanel(
        tabPanel("Resumen", .tabla_ui(ns("resumen"))), tabPanel("Líneas", .tabla_ui(ns("lineas"))),
        tabPanel("Compromisos", .tabla_ui(ns("referencias"))), tabPanel("Lotes y facturación", .tabla_ui(ns("lotes"))),
        tabPanel("Margen", .tabla_ui(ns("margen"))), tabPanel("Cronología", .tabla_ui(ns("cronologia"))))),
    Mapa = tagList(selectInput(ns("medida"), "Medida", c("Pedidos" = "pedidos", "Clientes" = "clientes", "Sacos" = "sacos")),
      if (requireNamespace("plotly", quietly = TRUE)) plotly::plotlyOutput(ns("mapa"), height = "520px"),
      uiOutput(ns("banderas")), .tabla_ui(ns("tabla"))),
    Cohortes = tagList(dateRangeInput(ns("periodo"), "Período de primera compra", start = .HoyProductos() - 365,
      end = .HoyProductos(), format = "dd/mm/yyyy", language = "es"), uiOutput(ns("cohorte_estado")),
      bs4Dash::tabsetPanel(tabPanel("Clientes nuevos", .tabla_ui(ns("nuevos"))),
                          tabPanel("Retención, ventas y frecuencia", .tabla_ui(ns("retencion"))))))
  div(class = "crm-vista", h1(.vistas[[id]]$titulo), contenido)
}

OperacionServer <- function(id, publicaciones, operativo, filtros) {
  moduleServer(id, function(input, output, session) {
    tipo <- unname(.operacion_vistas[id])
    if (tipo == "Trazabilidad") {
      observeEvent(operativo()$pedidos, {
        d <- operativo()$pedidos
        etiquetas <- if (is.null(d) || !nrow(d)) character() else stats::setNames(
          d$pedido_id, paste(d$CiaCod, d$PdcCod, sep = " · "))
        updateSelectizeInput(session, "pedido", choices = etiquetas,
                             selected = intersect(input$pedido, etiquetas), server = TRUE)
      }, ignoreNULL = FALSE)
      filtrar <- function(d) {
        if (is.null(d) || !nzchar(input$pedido %||% "")) return(NULL)
        d[which(d$pedido_id == input$pedido), , drop = FALSE]
      }
      resumen <- reactive(filtrar(operativo()$pedidos))
      lotes <- reactive(filtrar(publicaciones()$operacional$tablas$margenes))
      .tabla_server("resumen", reactive(.etiquetar_clientes(resumen(), operativo()$clientes)), output)
      .tabla_server("lineas", reactive(filtrar(operativo()$lineas_pedido)), output)
      .tabla_server("referencias", reactive(filtrar(publicaciones()$operacional$tablas$referencias_embarque)), output)
      .tabla_server("lotes", lotes, output)
      .tabla_server("margen", reactive({
        d <- lotes()
        if (!is.null(d) && nrow(d)) data.frame(MNFCC_COP = .dp_suma(d$MNFCC))
      }), output)
      .tabla_server("cronologia", reactive({
        d <- resumen()
        if (is.null(d) || !nrow(d)) return(NULL)
        eventos <- data.frame(Evento = "Creación de pedido", Fecha = d$fecha_creacion)
        facturas <- lotes()
        if (!is.null(facturas) && nrow(facturas)) {
          eventos <- rbind(eventos, unique(data.frame(Evento = paste("Factura", facturas$factura),
                                                     Fecha = facturas$fecha)))
        }
        eventos[order(eventos$Fecha), , drop = FALSE]
      }), output)
      output$cabecera <- renderUI({
        d <- resumen()
        if (is.null(d) || !nrow(d)) return(.nota("Seleccione un pedido disponible.", "muted"))
        .nota(paste("Pedido", d$CiaCod, d$PdcCod, "·", d$PerCod,
                    "· La facturación y FCC dependen del vínculo disponible con PyG por lote."))
      })
    }
    if (tipo == "Mapa") {
      destinos <- reactive({
        datos <- .destinos_operacion(operativo()$lineas_pedido, operativo()$pedidos)
        if (is.null(datos)) return(NULL)
        paises <- publicaciones()$operacional$tablas$paises
        if (!is.null(paises)) datos <- merge(datos, paises, by = "iso2", all.x = TRUE, sort = FALSE)
        if (!"nombre" %in% names(datos)) datos$nombre <- datos$iso2
        datos$nombre[is.na(datos$nombre)] <- datos$iso2[is.na(datos$nombre)]
        datos
      })
      .tabla_server("tabla", destinos, output)
      output$banderas <- renderUI({
        d <- destinos()
        if (is.null(d) || !nrow(d)) return(.nota("Se requiere un corte con destinos ISO homologados.", "muted"))
        div(class = "crm-controles-registro", lapply(seq_len(nrow(d)), function(i) {
          div(.bandera_pais(d$iso2[i]), tags$span(d$nombre[i] %||% d$iso2[i]))
        }))
      })
      if (requireNamespace("plotly", quietly = TRUE)) output$mapa <- plotly::renderPlotly({
        d <- destinos()
        shiny::validate(shiny::need(!is.null(d) && nrow(d), "Sin destinos homologados para los filtros."))
        d$iso3 <- unname(.iso3_operacion[d$iso2])
        d <- d[!is.na(d$iso3), ]
        shiny::validate(shiny::need(nrow(d), "Sin correspondencias para trazar el mapa."))
        d$medida <- d[[input$medida]]
        plotly::plot_ly(d, type = "choropleth", locations = ~iso3, locationmode = "ISO-3", z = ~medida,
                       text = ~nombre, hoverinfo = "text+z", colorscale = "Reds")
      })
    }
    if (tipo == "Cohortes") {
      historico <- reactive({
        seleccion <- filtros()
        seleccion$periodo <- NULL
        .filtrar_operacional(publicaciones(), seleccion)
      })
      primeros <- reactive({
        d <- .tabla_fuente(publicaciones(), "primer_pedido_cliente")
        rango <- input$periodo
        req(length(rango) == 2L, !anyNA(rango), rango[1L] <= rango[2L])
        if (is.null(d)) return(NULL)
        elegibles <- historico()$clientes
        d[which(d$PerCod %in% elegibles$PerCod & d$fecha_creacion >= rango[1L] &
                  d$fecha_creacion <= rango[2L]), , drop = FALSE]
      })
      .tabla_server("nuevos", reactive(.etiquetar_clientes(primeros(), operativo()$clientes)), output)
      .tabla_server("retencion", reactive(.cohortes_operacion(primeros(), historico()$pedidos)), output)
      output$cohorte_estado <- renderUI({
        d <- primeros()
        if (is.null(d)) return(.nota("Pendiente de cobertura de primeras compras.", "muted"))
        .nota(paste(nrow(d), "clientes nuevos por fecha de primer pedido observado.",
                    "Retención observada en los meses con pedidos."))
      })
    }
  })
}
