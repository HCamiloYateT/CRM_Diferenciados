# Identidad, descarga y glosario ----

.EncabezadoServer <- function(input, output, session, publicaciones, operativo, cargando, refrescar) {
  observeEvent(input$actualizar, refrescar())

  output$estado_corte <- renderUI({
    if (cargando()) return(tags$span("Cargando publicación…"))
    publicacion <- publicaciones()$operacional
    corte <- publicacion$manifiesto$procedencia$corte
    texto <- if (inherits(publicacion, "error")) "No se pudo leer la publicación" else if (length(corte)) {
      paste("Corte operacional", format(as.Date(corte), "%d/%m/%Y"))
    } else "Sin publicación operacional"
    tags$p(texto)
  })

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

  identidad <- toupper(session$user %||% "HCYATE")
  grupos <- paste(toupper(session$groups %||% "ANALÍTICA"), collapse = ", ")
  output$identidad_header <- renderUI({
    contenido <- tagList(tags$span(identidad), tags$small(paste("·", grupos)))
    tags$button(id = "usuario", type = "button", class = "action-button crm-identidad", title = "Información de sesión",
                contenido)
  })
  observeEvent(input$usuario, {
    showModal(modalDialog(title = "Usuario y grupos", tags$p(identidad), tags$p(grupos), uiOutput("estado_corte"),
                          footer = modalButton("Cerrar")))
  })

  observeEvent(input$glosario, showModal(.GlosarioDiferenciadosUI()))

  .IndicadoresHeaderServer(input, session)
  .NotasTareasHeaderServer(input, session, publicaciones)
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
