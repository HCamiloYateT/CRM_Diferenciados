# Guía de inicio y presupuesto provisional ----

.InicioDiferenciadosUI <- function() {
  secciones <- c(
    "Generalidades" = "Resumen ejecutivo de la línea 20000 y comparación de referencias del mercado.",
    "Gestión comercial" = paste("Presupuesto provisional, oportunidades, actividades, tareas y notas.",
                                "La meta de prueba es cero y no produce un porcentaje de cumplimiento."),
    "Prospección" = paste("Seguimiento por Kanban, contactos, prospectos, leads, clientes y descartados.",
                           "La conversión se observa en el primer pedido; los flujos de gestión requieren datos."),
    "Cohortes de clientes" = paste("Identifica PerCod nuevos por la fecha exacta de primera compra observada.",
                                   "Una cobertura histórica incompleta limita esta identificación."),
    "Productos" = paste("Marcas y nombres comerciales independientes, ventas cruzadas y calidad.",
                        "El volumen sin atribución se conserva y las asociaciones requieren revisión comercial."),
    "Muestras" = "Solicitudes, envíos y evaluaciones. El circuito y sus responsables siguen por validar.",
    "Clientes" = "Directorio y consulta por PerCod. Cada PerCod corresponde a una cuenta individual.",
    "Pedidos y embarques" = "Creación, compromisos y ejecución efectiva se muestran con sus calendarios propios.",
    "Rentabilidad" = "Margen anticipado y FCC requieren fuentes, unidades y asignaciones verificables.",
    "Potencial" = "El potencial y la penetración requieren denominadores comparables.",
    "Mercados internacionales" = "País del cliente y destinos se conservan separados; los mapas dependen de ISO.",
    "Administración" = "Catálogos, fuentes, auditoría y homologaciones, según el acceso y los datos disponibles."
  )
  iconos <- c("clipboard-list", "briefcase", "filter", "chart-gantt", "boxes-stacked", "flask",
              "users", "ship", "chart-line", "calculator", "globe", "gear")
  alcance <- paste("El CRM analiza la línea original 20000. Actualizar vuelve a leer el corte publicado.",
                   "Los indicadores de mercado se consultan desde su menú en el encabezado.")
  if (!.disponible_api("racafeForms", "racafeInfoGrid")) {
    return(div(class = "crm-vista", shiny::h1(.titulo), .nota(alcance),
               div(class = "crm-form-grid", lapply(seq_along(secciones), function(i) {
                 div(class = "crm-panel", shiny::h2(names(secciones)[i]), shiny::tags$p(secciones[i]))
               }))))
  }

  shiny::div(class = "crm-vista",
    racafeForms::racafeForm(id = "inicio_diferenciados", variant = "plain",
      racafeForms::racafePanel(title = "CRM Diferenciados · Información para la gestión comercial",
        variant = "highlighted", padding = "comfortable", shiny::tags$p(alcance)),
      racafeForms::racafePanel(title = "Consulta y actualización de la información", variant = "muted",
        racafeForms::racafeInfoGrid(columns = 2,
          racafeForms::racafeInfoItem("ALCANCE DE LA CONSULTA",
            "La barra lateral derecha reúne los filtros. Cada vista conserva su período y grano de análisis."),
          racafeForms::racafeInfoItem("DISPONIBILIDAD DE LOS DATOS",
            "La sesión usa el corte disponible. Una nueva publicación se incorpora con Actualizar."))),
      shiny::tags$h3("Secciones de la plataforma"),
      racafeForms::racafeInfoGrid(columns = 2, lapply(seq_along(secciones), function(i) {
        simbolo <- shiny::tagAppendAttributes(shiny::icon(iconos[i]), class = "text-danger")
        racafeForms::racafeCard(title = toupper(names(secciones)[i]), variant = "default", padding = "normal",
                                icon = simbolo, shiny::tags$p(secciones[i]))
      })),
      racafeForms::racafePanel(title = "Criterios para interpretar los resultados",
        racafeForms::racafeInfoGrid(columns = 2,
          racafeForms::racafeInfoItem("PEDIDOS Y EJECUCIÓN",
            "La fecha de creación y la fecha de embarque efectivo corresponden a hechos distintos."),
          racafeForms::racafeInfoItem("COBERTURA Y MEDIDAS",
            "Cada cifra depende del corte, la cobertura histórica y la unidad de medida indicada.")))))
}

.PresupuestoCeroUI <- function(id) {
  ns <- shiny::NS(id)
  div(class = "crm-vista", shiny::h1("Presupuesto"),
      .nota(paste("Ppto provisional de cero para sacos, margen US¢/lb y MNFCC.",
                  "La vista acumulada suma los meses con datos. Una meta de cero no genera porcentaje de cumplimiento.")),
      div(class = "crm-indicadores-filtros",
          shiny::numericInput(ns("anio"), "Año", as.integer(format(.HoyProductos(), "%Y")), min = 1900, max = 9999),
          shiny::checkboxInput(ns("acumulado"), "Vista acumulada", value = TRUE)),
      shiny::uiOutput(ns("resumen")),
      bs4Dash::tabsetPanel(tabPanel("Evolución mensual", .tabla_ui(ns("mensual")),
                                   if (requireNamespace("plotly", quietly = TRUE)) fluidRow(
                                     column(6, plotly::plotlyOutput(ns("serie_sacos"))),
                                     column(6, plotly::plotlyOutput(ns("serie_mnfcc"))))),
                          tabPanel("Presupuesto por cliente", .tabla_ui(ns("clientes")))))
}

.PresupuestoCeroServer <- function(id, operativo, publicaciones) {
  shiny::moduleServer(id, function(input, output, session) {
    mensual <- shiny::reactive({
      shiny::req(length(input$anio) == 1L, is.finite(input$anio), input$anio >= 1900, input$anio <= 9999)
      meses <- sprintf("%04d-%02d", as.integer(input$anio), seq_len(12L))
      pedidos <- operativo()$pedidos
      real <- if (is.null(pedidos)) rep(NA_real_, 12L) else {
        periodo <- format(pedidos$fecha_creacion, "%Y-%m")
        agrupado <- .dp_agregar(periodo, pedidos$sacos, meses)
        corte <- publicaciones()$operacional$manifiesto$procedencia$corte
        if (length(corte)) agrupado[!meses %in% periodo & meses <= format(as.Date(corte), "%Y-%m")] <- 0
        agrupado
      }
      margen <- .margenes_filtrados(publicaciones(), pedidos)
      mnfcc <- if (is.null(margen)) rep(NA_real_, 12L) else {
        .dp_agregar(format(margen$fecha, "%Y-%m"), margen$MNFCC, meses)
      }
      data.frame(Mes = meses, PptoL = 0, SacosPedidos = real, DiferenciaSacos = real,
                 PptoMargenUScentLb = 0, PptoMNFCC = 0, MNFCC_COP = mnfcc,
                 CumplimientoSacos = NA_real_, CumplimientoMNFCC = NA_real_)
    })
    .tabla_server("mensual", mensual, output)
    .tabla_server("clientes", shiny::reactive({
      clientes <- operativo()$clientes
      shiny::req(!is.null(clientes))
      pedidos <- operativo()$pedidos
      if (!is.null(pedidos)) pedidos <- pedidos[which(format(pedidos$fecha_creacion, "%Y") == input$anio), ]
      datos <- .clientes_vista(clientes, pedidos, .margenes_filtrados(publicaciones(), pedidos))
      datos$PptoL <- rep(0, nrow(datos))
      datos$PptoMNFCC <- rep(0, nrow(datos))
      datos$PptoMargenUScentLb <- rep(0, nrow(datos))
      datos$DiferenciaSacos <- datos$Sacos
      datos$DiferenciaMNFCC <- datos$MNFCC_COP
      datos$Cumplimiento <- rep(NA_real_, nrow(datos))
      datos
    }), output)
    if (requireNamespace("plotly", quietly = TRUE)) {
      serie <- function(campo, titulo) {
        datos <- mensual()
        valores <- datos[[campo]]
        if (isTRUE(input$acumulado)) {
          observados <- which(is.finite(valores))
          valores[observados] <- cumsum(valores[observados])
        }
        p <- plotly::plot_ly(x = datos$Mes, y = valores, type = "scatter", mode = "lines+markers",
                             name = "Real", line = list(color = "#DA291C"))
        p <- plotly::add_trace(p, x = datos$Mes, y = rep(0, nrow(datos)), name = "Ppto")
        plotly::layout(p, title = titulo, xaxis = list(title = ""), yaxis = list(title = ""))
      }
      output$serie_sacos <- plotly::renderPlotly(serie("SacosPedidos", "Presupuesto de sacos"))
      output$serie_mnfcc <- plotly::renderPlotly(serie("MNFCC_COP", "Presupuesto de MNFCC (COP)"))
    }
    output$resumen <- shiny::renderUI({
      corte <- publicaciones()$operacional$manifiesto$procedencia$corte
      datos <- mensual()
      meses_al_corte <- if (length(corte)) datos$Mes <= format(as.Date(corte), "%Y-%m") else rep(FALSE, 12L)
      total <- if (any(meses_al_corte)) .dp_suma(datos$SacosPedidos[meses_al_corte]) else NA_real_
      valores <- c("Ppto de sacos" = "0", "Sacos pedidos" = .formato_cantidad(total),
                   "Ppto MNFCC (COP)" = "0", "Cumplimiento con meta cero" = "No disponible")
      div(class = "crm-kpis", lapply(seq_along(valores), function(i) {
        div(class = "crm-kpi", span(names(valores)[i]), strong(valores[i]))
      }))
    })
  })
}
