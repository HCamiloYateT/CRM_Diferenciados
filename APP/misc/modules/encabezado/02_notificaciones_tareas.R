# Notas y tareas del encabezado ----

.SeguimientoHeaderUI <- function() {
  if (.MenuIndicadoresCorporativo()) {
    return(racafeModulos::MenuHeaderUI("MenuTareas", icon = shiny::icon("bars-progress"), min_width = "390px",
                                      title = "Notas y tareas", keep_open_on_select = TRUE))
  }
  boton <- .boton_header("MenuTareas_abrir", "Notas y tareas", "bars-progress")
  tags$li(class = "nav-item dropdown position-relative",
          boton, uiOutput("contador_notas_header", inline = TRUE))
}

.notas_header_registros <- function(tablas, identidad) {
  d <- .tablero_notas_datos(tablas, identidad$usuario %||% character(), identidad$grupos, identidad$local)
  d[!d$Archivado & !d$Cumplido, , drop = FALSE]
}

.nota_header_item <- function(fila, datos_actividades, id) {
  tarea <- fila$Tipo == "list"
  tipo <- if (tarea) "Tarea" else "Nota"
  fecha <- if (is.na(fila$seguimiento)) "Sin fecha" else .formato_fecha_crm(fila$seguimiento)
  metadata <- c(Vencimiento = fecha)
  if (tarea) {
    actividades <- .actividades_nota_lista(datos_actividades, fila$tarea_id)
    total <- length(actividades$textos)
    metadata <- c(metadata, Actividades = if (total) {
      paste0(sum(actividades$cumplidas), "/", total, " actividades")
    } else "Sin actividades")
  }
  titulo <- .gestion_texto(fila$asunto)
  if (is.na(titulo)) titulo <- "Sin título"
  responsable <- .gestion_texto(fila$responsable)
  if (is.na(responsable)) responsable <- "Sin responsable"
  prioridad <- tools::toTitleCase(tolower(fila$Prioridad))
  variante <- c(URGENTE = "danger", IMPORTANTE = "warning", MEDIA = "info", BAJA = "neutral")
  badges <- tagList(racafeForms::racafeStatusBadge(prioridad, unname(variante[fila$Prioridad]), size = "xs"),
                    racafeForms::racafeStatusBadge("Pendiente", "warning", size = "xs"))
  subtitulo <- paste(tipo, "·", responsable)
  if (.disponible_api("racafeShiny", "racafeListItem")) {
    return(racafeShiny::racafeListItem(titulo, subtitle = subtitulo, metadata = metadata, badges = badges,
                                      id = id, value = fila$tarea_id))
  }
  tags$li(class = "dropdown-item text-wrap", strong(titulo), tags$div(subtitulo), badges,
          tags$small(class = "d-block text-muted", paste(names(metadata), metadata, collapse = " · ")))
}

.NotasTareasHeaderServer <- function(input, output, session, publicaciones, editar) {
  tablas <- reactive(publicaciones()$crm$tablas)
  registros <- reactive(.notas_header_registros(tablas(), .crm_identidad_notas(session)))
  contenido <- reactive({
    d <- registros()
    actividades <- .gestion_filas(tablas(), "tarea_actividades")
    items <- lapply(seq_len(nrow(d)), function(i) {
      .nota_header_item(d[i, , drop = FALSE], actividades, "header_abrir_nota")
    })
    if (.disponible_api("racafeShiny", "racafeListGroup")) {
      return(do.call(racafeShiny::racafeListGroup, c(items, list(empty_message = "No hay notas ni tareas pendientes."))))
    }
    if (!length(items)) return(.nota("No hay notas ni tareas pendientes.", "empty"))
    tags$ul(class = "list-unstyled mb-0", items)
  })
  observeEvent(input$header_abrir_nota, {
    d <- registros()
    fila <- d[d$tarea_id %in% input$header_abrir_nota, , drop = FALSE]
    req(nrow(fila) == 1L)
    editar(fila)
  })
  if (.MenuIndicadoresCorporativo()) {
    racafeModulos::MenuHeaderServer("MenuTareas", items_r = contenido, showHeader = FALSE,
                                    badgeStatus_r = "danger", badge_n_r = reactive(nrow(registros())))
  } else {
    output$contador_notas_header <- renderUI({
      total <- nrow(registros())
      if (total) tags$span(class = "badge badge-danger navbar-badge", total)
    })
    observeEvent(input$MenuTareas_abrir, {
      showModal(racafeForms::racafeModalDetail(title = "Notas y tareas", contenido(), size = "subventana3"))
    })
  }
}
