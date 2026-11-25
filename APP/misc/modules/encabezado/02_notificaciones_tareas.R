# Notas propias y tareas pendientes asignadas ----

.SeguimientoHeaderUI <- function() {
  if (.MenuIndicadoresCorporativo()) {
    return(racafeModulos::MenuHeaderUI("MenuTareas", icon = shiny::icon("bars-progress"), min_width = "390px",
                                      title = "Notas y tareas", keep_open_on_select = TRUE))
  }
  tags$li(class = "nav-item dropdown", .boton_header("MenuTareas_abrir", "Notas y tareas", "bars-progress"))
}

.NotasTareasHeaderServer <- function(input, session, publicaciones) {
  registros <- reactive({
    tablas <- publicaciones()$crm$tablas
    d <- .gestion_filas(tablas, "tareas")
    archivadas <- .gestion_filas(tablas, "tarea_archivo")$tarea_id
    d <- d[!d$tarea_id %in% archivadas & d$estado != "CANCELADA", , drop = FALSE]
    identidad <- .crm_identidad_notas(session)
    if (identidad$local) d <- d[is.na(d$PerCod), , drop = FALSE]
    usuario <- toupper(identidad$usuario %||% character())
    .gestion_agenda(d[which(toupper(d$responsable) %in% usuario), , drop = FALSE])
  })
  tareas <- reactive({
    d <- registros()
    d[which(d$tipo != "NOTA" & d$estado == "PENDIENTE"), , drop = FALSE]
  })
  lista <- function(d, fechas, mensaje) {
    if (!nrow(d)) return(.nota(mensaje, "empty"))
    tags$ul(class = "list-unstyled mb-0", lapply(seq_len(min(8L, nrow(d))), function(i) {
      tags$li(class = "py-1", tags$strong(d$asunto[i]),
              if (!is.na(fechas[i])) tags$small(class = "ml-2 text-muted", .formato_fecha_crm(fechas[i])))
    }))
  }
  contenido <- reactive({
    d <- registros()
    notas <- d[which(d$tipo == "NOTA"), , drop = FALSE]
    notas <- notas[order(notas$fecha, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    pendientes <- tareas()
    div(class = "px-3 py-2",
        tags$h6("Notas"), lista(notas, notas$fecha, "Sin notas propias."),
        .boton("header_ir_notas", "Abrir notas", "arrow-right"), tags$hr(),
        tags$h6("Tareas pendientes"),
        lista(pendientes, pendientes$seguimiento, "Sin tareas pendientes asignadas."))
  })
  navegar <- function(vista) {
    if (!.MenuIndicadoresCorporativo()) removeModal()
    updatebs4TabItems(session, "navegacion", selected = vista)
  }
  observeEvent(input$header_ir_notas, navegar("notas"))
  if (.MenuIndicadoresCorporativo()) {
    menu <- racafeModulos::MenuHeaderServer("MenuTareas", items_r = contenido, href = "#",
                                            footerText = "Abrir agenda", badgeStatus_r = "danger",
                                            badge_n_r = reactive(nrow(tareas())))
    observeEvent(menu$footer_click(), navegar("tareas"))
  } else {
    observeEvent(input$MenuTareas_abrir, {
      showModal(modalDialog(title = "Notas y tareas", contenido(),
                            footer = tagList(.boton("header_ir_tareas", "Abrir agenda", "arrow-right"),
                                             modalButton("Cerrar"))))
    })
    observeEvent(input$header_ir_tareas, navegar("tareas"))
  }
}
