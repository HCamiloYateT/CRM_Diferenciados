# Notas propias y tareas pendientes asignadas ----

.menus_seguimiento_header <- list(
  MenuNotas = list(titulo = "Notas", icono = "note-sticky", vista = "notas", enlace = "Abrir notas"),
  MenuTareas = list(titulo = "Tareas pendientes", icono = "bars-progress", vista = "tareas", enlace = "Abrir agenda")
)

.SeguimientoHeaderUI <- function(id, menu) {
  if (.MenuIndicadoresCorporativo()) {
    return(racafeModulos::MenuHeaderUI(id, icon = shiny::icon(menu$icono), min_width = "390px",
                                      title = menu$titulo, keep_open_on_select = TRUE))
  }
  tags$li(class = "nav-item dropdown", .boton_header(paste0(id, "_abrir"), menu$titulo, menu$icono))
}

.NotasTareasHeaderServer <- function(input, session, publicaciones) {
  registros <- reactive({
    d <- .gestion_filas(publicaciones()$crm$tablas, "tareas")
    usuario <- toupper(session$user %||% character())
    .gestion_agenda(d[which(toupper(d$responsable) %in% usuario), , drop = FALSE])
  })
  for (id in names(.menus_seguimiento_header)) local({
    clave <- id
    menu <- .menus_seguimiento_header[[clave]]
    notas <- menu$vista == "notas"
    filas_menu <- reactive({
      d <- registros()
      filas <- if (notas) which(d$tipo == "NOTA") else which(d$tipo != "NOTA" & d$estado == "PENDIENTE")
      d[filas, , drop = FALSE]
    })
    contenido <- reactive({
      d <- filas_menu()
      if (!nrow(d)) {
        mensaje <- if (notas) "Sin notas propias." else "Sin tareas pendientes asignadas."
        return(.nota(mensaje, "empty"))
      }
      if (notas) d <- d[order(d$fecha, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
      fechas <- if (notas) d$fecha else d$seguimiento
      tags$ul(class = "list-unstyled px-3 mb-0", lapply(seq_len(min(8L, nrow(d))), function(i) {
        tags$li(class = "py-1", tags$strong(d$asunto[i]),
                if (!is.na(fechas[i])) tags$small(class = "ml-2 text-muted", format(fechas[i], "%d/%m/%Y")))
      }))
    })
    navegar <- function() updatebs4TabItems(session, "navegacion", selected = menu$vista)
    if (.MenuIndicadoresCorporativo()) {
      desplegable <- racafeModulos::MenuHeaderServer(clave, items_r = contenido, href = "#", footerText = menu$enlace,
                                                     badgeStatus_r = "danger", badge_n_r = reactive(nrow(filas_menu())))
      observeEvent(desplegable$footer_click(), navegar())
    } else {
      ir <- paste0(clave, "_ir")
      observeEvent(input[[paste0(clave, "_abrir")]], {
        showModal(modalDialog(title = menu$titulo, contenido(),
                              footer = tagList(.boton(ir, menu$enlace, "arrow-right"), modalButton("Cerrar"))))
      })
      observeEvent(input[[ir]], {
        removeModal()
        navegar()
      })
    }
  })
}
