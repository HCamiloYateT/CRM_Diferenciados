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
