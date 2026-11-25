# Edición del formulario de notas y tareas ----

.comentarios_nota_ui <- function(datos, id) {
  d <- datos[datos$tarea_id %in% id, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  tags$ul(class = "list-unstyled", lapply(seq_len(nrow(d)), function(i) {
    tags$li(tags$strong(d$autor[i]), " · ", .formato_fecha_crm(d$fecha[i]), tags$p(d$comentario[i]))
  }))
}

.guardar_edicion_nota <- function(raiz, id, comentario, actividades, originales, identidad) {
  usuario <- identidad$usuario
  if (!.crm_usuario_autenticado(usuario)) stop("La sesión no tiene usuario.", call. = FALSE)
  .gestion_actualizar(raiz, function(tablas) {
    registros <- .tablero_notas_datos(tablas, usuario, identidad$grupos, identidad$local)
    fila <- registros[registros$tarea_id %in% id, , drop = FALSE]
    if (nrow(fila) != 1L) stop("El registro ya no está disponible.", call. = FALSE)
    if (fila$Tipo == "list" && !is.null(actividades)) {
      d <- tablas$tarea_actividades
      propias <- d$tarea_id %in% id
      if (!identical(.actividades_nota_lista(d, id), originales)) {
        stop("La lista cambió en otra sesión. Vuelva a abrir el registro.", call. = FALSE)
      }
      nuevas <- d[FALSE, , drop = FALSE]
      if (nrow(actividades)) {
        nuevas <- data.frame(tarea_id = id, PerCod = fila$PerCod, orden = seq_len(nrow(actividades)),
                              actividad = actividades$texto, cumplida = actividades$cumplida)
      }
      tablas$tarea_actividades <- rbind(d[!propias, , drop = FALSE], nuevas)
    }
    if (!is.na(comentario)) {
      tablas$tarea_comentarios <- rbind(tablas$tarea_comentarios,
        data.frame(comentario_id = basename(tempfile("comentario-")), tarea_id = id, autor = usuario,
                   fecha = Sys.time(), comentario = comentario))
    }
    tablas$tareas$actualizada[tablas$tareas$tarea_id %in% id] <- Sys.time()
    tablas
  }, usuario, "Editar comentarios y actividades", id)
}

.EditarNotas <- function(id, publicaciones, raiz, identidad, refrescar) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    registro <- reactiveVal(NULL)
    filas <- reactiveVal(data.frame(id = integer(), texto = character(), cumplida = logical()))
    secuencia <- 0L
    estados <- reactiveValues()
    observadores <- list()
    leer <- function() {
      d <- filas()
      for (i in seq_len(nrow(d))) {
        clave <- d$id[i]
        d$texto[i] <- input[[paste0("actividad_", clave)]] %||% d$texto[i]
        d$cumplida[i] <- isTRUE(estados[[as.character(clave)]])
      }
      d
    }
    limpiar_fila <- function(indice) {
      clave <- as.character(indice)
      for (observador in observadores[[clave]]) observador$destroy()
      observadores[[clave]] <<- NULL
      estados[[clave]] <- NULL
      output[[paste0("estado_actividad_", indice)]] <- NULL
    }
    observar_fila <- function(indice, cumplida = FALSE) {
      force(indice)
      clave <- as.character(indice)
      boton <- paste0("cumplida_", indice)
      estados[[clave]] <- cumplida
      output[[paste0("estado_actividad_", indice)]] <- renderUI({
        .boton_estado_actividad(ns, boton, isTRUE(estados[[clave]]))
      })
      observadores[[clave]] <<- list(
        observeEvent(input[[boton]], estados[[clave]] <- !isTRUE(estados[[clave]])),
        observeEvent(input[[paste0("quitar_", indice)]], {
          d <- leer()
          filas(d[d$id != indice, , drop = FALSE])
          limpiar_fila(indice)
        })
      )
    }
    cerrar <- function() {
      for (indice in filas()$id) limpiar_fila(indice)
      filas(filas()[FALSE, , drop = FALSE])
      registro(NULL)
      removeModal()
    }
    output$actividades <- renderUI({
      d <- filas()
      lapply(seq_len(nrow(d)), function(i) .actividad_nota_ui(ns, d$id[i], i, d$texto[i]))
    })
    observeEvent(input$agregar, {
      secuencia <<- secuencia + 1L
      filas(rbind(leer(), data.frame(id = secuencia, texto = "", cumplida = FALSE)))
      observar_fila(secuencia)
    })
    observeEvent(input$cancelar, cerrar())
    observeEvent(input$guardar, {
      ctx <- registro()
      req(!is.null(ctx))
      comentario <- .gestion_texto(input$comentario)
      d <- if (ctx$tarea) leer()
      if (!is.null(d) && any(!nzchar(trimws(d$texto)))) {
        return(racafeForms::racafeNotify(session, "Complete o quite las actividades vacías", type = "warning"))
      }
      if (!is.null(d)) d$texto <- trimws(d$texto)
      tryCatch({
        .guardar_edicion_nota(raiz, ctx$id, comentario, d, ctx$originales, identidad)
        cerrar()
        refrescar()
        racafeForms::racafeNotify(session, "Registro actualizado", type = "success")
      }, error = function(e) {
        racafeForms::racafeNotify(session, "No se pudo actualizar", conditionMessage(e), type = "error")
      })
    })
    function(fila) {
      tablas <- publicaciones()$crm$tablas
      originales <- .actividades_nota_lista(.gestion_filas(tablas, "tarea_actividades"), fila$tarea_id)
      comentarios <- .gestion_filas(tablas, "tarea_comentarios")
      for (indice in filas()$id) limpiar_fila(indice)
      ids <- secuencia + seq_along(originales$textos)
      secuencia <<- secuencia + length(ids)
      filas(data.frame(id = ids, texto = originales$textos, cumplida = originales$cumplidas))
      for (i in seq_along(ids)) observar_fila(ids[i], originales$cumplidas[i])
      tarea <- fila$Tipo == "list"
      registro(list(id = fila$tarea_id, tarea = tarea, originales = originales))
      showModal(racafeForms::racafeModalDetail(
        title = if (tarea) "Editar tarea" else "Editar nota", size = "subventana3",
        racafeForms::racafeForm(
          racafeForms::racafeInfoItem("Título", fila$asunto),
          racafeForms::racafeInfoItem("Descripción", fila$resultado),
          racafeForms::racafeInfoItem("Comentarios", .comentarios_nota_ui(comentarios, fila$tarea_id),
                                     empty = "Sin comentarios"),
          racafeForms::racafeInputTextArea("comentario", "Añadir comentario", rows = 3, ns = ns),
          if (tarea) div(class = "nota-actividades-contenedor",
                         div(class = "nota-actividades-header",
                             racafeForms::racafeButton("agregar", "Añadir", icon = "plus", size = "xxs", ns = ns)),
                         uiOutput(ns("actividades"))),
          variant = "plain"
        ),
        footer = racafeForms::racafeModalFooter(
          racafeForms::racafeButtonCancel("cancelar", ns = ns),
          racafeForms::racafeButton("guardar", "Guardar", icon = "check", variant = "primary", size = "xs", ns = ns)
        )
      ))
    }
  })
}
