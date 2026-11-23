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
    "Clientes" = "Directorio y consulta por PerCod. Cuentas padre, tiers y productos homologados están pendientes.",
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

.GlosarioDiferenciadosUI <- function() {
  definiciones <- c(
    PerCod = "Identificador institucional del cliente.",
    Cohorte = "Período de creación del primer pedido observado del PerCod en la línea 20000.",
    "Compromiso de embarque" = "Fecha o período previsto asociado al pedido.",
    "Embarque efectivo" = "Fecha de ejecución verificada para evaluar cumplimiento.",
    FCC = "Margen financiero asociado a una operación conciliada.",
    "Market Basket" = "Análisis de ítems comprados juntos que orienta posibles ventas cruzadas."
  )
  contenido <- if (.disponible_api("racafeForms", "racafeInfoGrid")) {
    racafeForms::racafeInfoGrid(columns = 2, lapply(seq_along(definiciones), function(i) {
      racafeForms::racafeInfoItem(names(definiciones)[i], definiciones[i])
    }))
  } else {
    shiny::tags$dl(lapply(seq_along(definiciones), function(i) {
      shiny::tagList(shiny::tags$dt(names(definiciones)[i]), shiny::tags$dd(definiciones[i]))
    }))
  }
  shiny::modalDialog(title = "Glosario comercial", contenido, easyClose = TRUE, footer = shiny::modalButton("Cerrar"))
}

.PresupuestoCeroUI <- function(id) {
  ns <- shiny::NS(id)
  div(class = "crm-vista", shiny::h1("Presupuesto"),
      .nota(paste("Ppto provisional de cero para sacos, margen US¢/lb y MNFCC.",
                  "El cumplimiento queda sin calcular mientras la meta sea cero.")),
      shiny::numericInput(ns("anio"), "Año", as.integer(format(.HoyProductos(), "%Y")), min = 1900, max = 9999),
      shiny::uiOutput(ns("resumen")),
      div(class = "crm-panel", shiny::h2("Ejecución por creación de pedido"), .tabla_ui(ns("mensual"))))
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
      data.frame(Mes = meses, PptoL = 0, SacosPedidos = real, DiferenciaSacos = real,
                 PptoMargenUScentLb = 0, PptoMNFCC = 0, Cumplimiento = NA_real_)
    })
    .tabla_server("mensual", mensual, output)
    output$resumen <- shiny::renderUI({
      corte <- publicaciones()$operacional$manifiesto$procedencia$corte
      datos <- mensual()
      meses_al_corte <- if (length(corte)) datos$Mes <= format(as.Date(corte), "%Y-%m") else rep(FALSE, 12L)
      total <- if (any(meses_al_corte)) .dp_suma(datos$SacosPedidos[meses_al_corte]) else NA_real_
      valores <- c("Ppto de sacos" = "0", "Sacos pedidos" = .formato_cantidad(total),
                   "Cumplimiento" = "No disponible")
      div(class = "crm-kpis", lapply(seq_along(valores), function(i) {
        div(class = "crm-kpi", span(names(valores)[i]), strong(valores[i]))
      }))
    })
  })
}
