# Botones del encabezado ----

.boton_header <- function(id, titulo, simbolo) {
  boton <- if (.disponible_api("racafeForms", "racafeButton")) {
    racafeForms::racafeButton(id, NULL, icon = simbolo, title = titulo, size = "xxs", variant = "secondary")
  } else {
    shiny::actionButton(id, NULL, icon = shiny::icon(simbolo), title = titulo, class = "btn-sm btn-secondary")
  }
  shiny::tagAppendAttributes(boton, `aria-label` = titulo)
}

# Glosario comercial ----

.GlosarioDiferenciadosUI <- function() {
  secciones <- list(
    "Clientes y gestión comercial" = c(
      "PerCod" = "Código institucional de la persona o empresa en SysCafe. Cada cuenta se consulta por su PerCod.",
      "Cliente" = "Cuenta con al menos un pedido válido observado en Diferenciados.",
      "Prospecto" = "Entidad identificada con la que todavía se explora una relación comercial.",
      "Oportunidad" = "Negocio potencial en seguimiento, con producto, volumen y próxima acción.",
      "Oferta" = "Propuesta comercial que registra precio o diferencial, volumen y condiciones.",
      "Muestra comercial" = "Producto enviado para evaluación antes de una posible compra.",
      "Segmento" = "Clasificación comercial de la cuenta. Su filtro está pendiente de homologación.",
      "Asesor" = "Responsable comercial de la cuenta. Su filtro está pendiente de fuente."
    ),
    "Pedidos y embarques" = c(
      "Pedido" = "Registro comercial en SysCafe, identificado por compañía y número. El CRM usa la línea 20000.",
      "Fecha de creación" = "Día en que se registró el pedido en SysCafe. El creador es el usuario que lo registró.",
      "Mes de embarque" = "Mes previsto en la cabecera del pedido. Se usa para filtrar compromisos.",
      "Embarque efectivo" = "Fecha de ejecución verificada. Su fuente está pendiente de integración.",
      "País de destino" = "País al que se dirige el café según la línea del pedido.",
      "Sacos" = "Cantidad de la operación expresada en sacos según la unidad de origen homologada."
    ),
    "Indicadores y análisis" = c(
      "Cohorte" = "Grupo de clientes cuyo primer pedido observado cae en el mismo período.",
      "Retención" = "Proporción de clientes de una cohorte con pedidos en un mes posterior.",
      "MNFCC" = "Margen financiero en pesos colombianos procedente del PyG por lote vinculado al pedido.",
      "Presupuesto" = "Meta comercial del período. En Diferenciados el valor provisional por cliente es cero.",
      "Market Basket" = "Análisis de artículos comprados juntos para identificar oportunidades de venta cruzada.",
      "TRM" = "Tasa representativa del mercado expresada en pesos colombianos por dólar estadounidense.",
      "Diferencial de compra" = "Prima o descuento frente a una referencia de precio. Consulte la unidad indicada.",
      "Corte operacional" = "Fecha de la publicación de SysCafe disponible en la sesión."
    )
  )
  contenido <- lapply(names(secciones), function(titulo) {
    definiciones <- secciones[[titulo]]
    div(class = "mb-3",
        tags$h5(titulo),
        if (.disponible_api("racafeForms", "racafeInfoGrid")) {
          racafeForms::racafeInfoGrid(columns = 2, lapply(names(definiciones), function(termino) {
            racafeForms::racafeInfoItem(termino, unname(definiciones[[termino]]))
          }))
        } else tags$dl(lapply(names(definiciones), function(termino) {
          tagList(tags$dt(termino), tags$dd(definiciones[[termino]]))
        })))
  })
  shiny::modalDialog(
    title = "Glosario comercial de Diferenciados", size = "l", easyClose = TRUE,
    tags$p("Definiciones para interpretar las cuentas, los pedidos y los indicadores del CRM."),
    contenido, footer = shiny::modalButton("Cerrar")
  )
}
