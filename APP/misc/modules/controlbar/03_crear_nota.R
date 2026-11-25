# Captura de notas y tareas ----

.etiquetas_notas_corporativas <- function() {
  .disponible_api("racafeModulos", "ListaDesplegableAgregableUI") &&
    .disponible_api("racafeModulos", "ListaDesplegableAgregable")
}

.actividad_nota_ui <- function(ns, id, numero = id) {
  div(id = ns(paste0("fila_", id)), class = "nota-actividad-row",
      div(class = "nota-actividad-texto",
          racafeForms::racafeInputText(paste0("actividad_", id), label = NULL,
                                       placeholder = paste("Actividad", numero), ns = ns)),
      div(class = "nota-actividad-accion", uiOutput(ns(paste0("estado_actividad_", id)))),
      div(class = "nota-actividad-accion",
          racafeForms::racafeButton(paste0("quitar_", id), label = NULL, icon = "trash", title = "Eliminar actividad",
                                    variant = "danger", size = "xxs", ns = ns)))
}

CrearNotasUI <- function(id) {
  ns <- NS(id)
  racafeForms::racafePanel(
    title = NULL, padding = "compact", bordered = FALSE,
    racafeForms::racafeForm(
      racafeForms::racafeFormGroup(
        racafeForms::racafeFormRow(
          racafeForms::racafeInputSelect("tipo", "Tipo", c("Nota" = "NOTA", "Tarea" = "SEGUIMIENTO"),
                                         selected = "NOTA", empty = FALSE, search = FALSE, required = TRUE, ns = ns),
          racafeForms::racafeInputText("titulo", "Título", required = TRUE, ns = ns),
          responsive = "fixed"
        )
      ),
      racafeForms::racafeFormGroup(
        racafeForms::racafeInputTextArea("descripcion", "Descripción", rows = 4, ns = ns)
      ),
      conditionalPanel(
        "input.tipo === 'SEGUIMIENTO'", ns = ns,
        racafeForms::racafeFormGroup(
          racafeForms::racafeInputDate("seguimiento", "Fecha de cumplimiento", value = .HoyProductos() + 1L,
                                       min = .HoyProductos(), required = TRUE, ns = ns)
        )
      ),
      racafeForms::racafeFormGroup(uiOutput(ns("asignacion")), divider = FALSE),
      racafeForms::racafeFormGroup(
        if (.etiquetas_notas_corporativas()) {
          racafeModulos::ListaDesplegableAgregableUI(ns("etiquetas"))
        } else div(class = "crm-nota-selectize",
                   selectizeInput(ns("etiquetas"), "Etiquetas", choices = NULL, multiple = TRUE,
                                  options = list(create = TRUE, persist = FALSE,
                                                 placeholder = "Elegir o crear etiquetas")))
      ),
      conditionalPanel(
        "input.tipo === 'SEGUIMIENTO'", ns = ns,
        racafeForms::racafeFormGroup(
          racafeForms::racafeInputSwitch("incluir_actividades", "Incluir lista de actividades", ns = ns),
          conditionalPanel(
            "input.incluir_actividades", ns = ns,
            div(class = "nota-actividades-contenedor",
                div(class = "nota-actividades-header",
                    racafeForms::racafeButton("agregar_actividad", "Añadir", icon = "plus",
                                              variant = "primary", size = "xxs", ns = ns)),
                div(id = ns("lista_actividades"), .actividad_nota_ui(ns, 1L)))
          )
        )
      ),
      racafeForms::racafeFormFooter(
        racafeForms::racafeButton("crear", "Crear nota o tarea", icon = "note-sticky",
                                  variant = "primary", size = "xs", ns = ns),
        align = "right"
      ),
      size = "compact", variant = "plain"
    )
  )
}

# Confirmación de captura ----

.confirmacion_nota_ui <- function(campos, responsable, ns) {
  tarea <- identical(campos$tipo, "SEGUIMIENTO")
  descripcion <- .gestion_texto(campos$resultado)
  detalle <- list(
    racafeForms::racafeInfoItem("TIPO", if (tarea) "TAREA" else "NOTA"),
    racafeForms::racafeInfoItem("TÍTULO", campos$asunto),
    racafeForms::racafeInfoItem("DESCRIPCIÓN", if (is.na(descripcion)) "Sin descripción" else descripcion),
    racafeForms::racafeInfoItem("ETIQUETAS", .etiquetas_crm_ui(campos$etiquetas), empty = "Sin etiquetas"),
    racafeForms::racafeInfoItem("PRIORIDAD", campos$prioridad)
  )
  if (tarea) {
    detalle <- c(detalle, list(
      racafeForms::racafeInfoItem("FECHA DE CUMPLIMIENTO", .formato_fecha_crm(campos$seguimiento)),
      racafeForms::racafeInfoItem("RESPONSABLE", responsable)
    ))
  }
  contenido <- tagList(
    tags$p("Revise la información antes de crear el registro."),
    do.call(racafeForms::racafeInfoGrid, c(detalle, list(columns = 2))),
    if (tarea && length(campos$actividades)) {
      tags$div(tags$strong("ACTIVIDADES"), tags$ul(lapply(campos$actividades, function(actividad) {
        estado <- if (actividad %in% campos$actividades_cumplidas) "Completada" else "Pendiente"
        tags$li(paste0(actividad, " (", estado, ")"))
      })))
    }
  )
  if (.disponible_api("racafeShiny", "racafeOverlayLoader")) {
    contenido <- racafeShiny::racafeOverlayLoader("guardar", contenido, triggers = "confirmar", ns = ns)
  }
  racafeForms::racafeModalDetail(
    title = if (tarea) "Confirmar tarea" else "Confirmar nota", size = "subventana3", contenido,
    footer = racafeForms::racafeModalFooter(
      racafeForms::racafeButtonCancel("cancelar", "Cancelar", size = "xs", ns = ns),
      racafeForms::racafeButton("confirmar", "Crear", icon = "note-sticky", variant = "primary", size = "xs", ns = ns)
    )
  )
}

CrearNotas <- function(id, publicaciones, raiz, al_guardar, usuario = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    identidad <- .crm_identidad_notas(session)
    if (is.null(usuario)) usuario <- reactive(toupper(identidad$usuario %||% "HCYATE"))
    pendiente <- reactiveVal(NULL)
    actividades <- reactiveVal(1L)
    estados_actividad <- reactiveValues()
    siguiente_actividad <- 1L
    numero_actividad <- 1L
    observadores <- list()
    limpiar_etiquetas <- reactiveVal(FALSE)

    etiquetas_modulo <- if (.etiquetas_notas_corporativas()) {
      racafeModulos::ListaDesplegableAgregable("etiquetas", label = "Etiquetas", choices = character(),
                                               selected = character(), multiple = TRUE, fem = TRUE)
    }
    etiquetas_elegidas <- if (is.null(etiquetas_modulo)) reactive(input$etiquetas) else etiquetas_modulo$seleccion
    observeEvent(publicaciones(), {
      catalogo <- .gestion_filas(publicaciones()$crm$tablas, "tarea_etiquetas")$etiqueta
      seleccion <- if (limpiar_etiquetas()) character() else etiquetas_elegidas() %||% character()
      limpiar_etiquetas(FALSE)
      opciones <- sort(unique(c(catalogo, seleccion)))
      if (is.null(etiquetas_modulo)) {
        updateSelectizeInput(session, "etiquetas", choices = opciones, selected = seleccion)
      } else etiquetas_modulo$actualizar(opciones, seleccion)
    })

    output$asignacion <- renderUI({
      racafeForms::racafeFormRow(
        racafeForms::racafeInputSelect("prioridad", "Prioridad", names(.colores_prioridad_crm),
                                       selected = "MEDIA", empty = FALSE, search = FALSE, required = TRUE, ns = ns),
        racafeForms::racafeInputText("responsable", "Responsable", value = usuario(),
                                     disabled = TRUE, ns = ns),
        responsive = "fixed"
      )
    })

    quitar_actividad <- function(indice) {
      if (indice == 1L) {
        updateTextInput(session, paste0("actividad_", indice), value = "")
        estados_actividad[[as.character(indice)]] <- FALSE
        return(invisible(NULL))
      }
      removeUI(selector = paste0("#", ns(paste0("fila_", indice))), immediate = TRUE)
      actividades(setdiff(actividades(), indice))
      clave <- as.character(indice)
      for (observador in observadores[[clave]]) observador$destroy()
      estados_actividad[[clave]] <- NULL
      output[[paste0("estado_actividad_", indice)]] <- NULL
      observadores[[clave]] <<- NULL
    }
    observar_actividad <- function(indice) {
      force(indice)
      clave <- as.character(indice)
      boton <- paste0("cumplida_", indice)
      output[[paste0("estado_actividad_", indice)]] <- renderUI({
        cumplida <- isTRUE(estados_actividad[[clave]])
        titulo <- if (cumplida) "Marcar como pendiente" else "Marcar como cumplida"
        tagAppendAttributes(
          racafeForms::racafeButton(boton, label = NULL, icon = if (cumplida) "rotate-left" else "check",
                                    title = titulo, variant = "primary", size = "xxs", ns = ns),
          `aria-label` = titulo, `aria-pressed` = if (cumplida) "true" else "false"
        )
      })
      observadores[[clave]] <<- list(
        observeEvent(input[[paste0("quitar_", indice)]], quitar_actividad(indice)),
        observeEvent(input[[boton]], {
          estados_actividad[[clave]] <- !isTRUE(estados_actividad[[clave]])
        })
      )
    }
    observar_actividad(1L)
    observeEvent(input$agregar_actividad, {
      siguiente_actividad <<- siguiente_actividad + 1L
      numero_actividad <<- numero_actividad + 1L
      indice <- siguiente_actividad
      insertUI(selector = paste0("#", ns("lista_actividades")), where = "beforeEnd",
               ui = .actividad_nota_ui(ns, indice, numero_actividad), immediate = TRUE)
      actividades(c(actividades(), indice))
      observar_actividad(indice)
    })

    observeEvent(input$crear, {
      pendiente(NULL)
      if (!.crm_usuario_autenticado(identidad$usuario)) {
        return(racafeForms::racafeNotify(session, "Sesión sin usuario autenticado",
                                        "Abra el CRM desde Analítica o desde su sesión personal de RStudio Server.", type = "warning"))
      }
      tipo <- input$tipo %||% ""
      tarea <- identical(tipo, "SEGUIMIENTO")
      lista <- character()
      cumplidas <- logical()
      campos <- list(tipo = list(label = "Tipo", required = TRUE, kind = "select"),
                     titulo = list(label = "Título", required = TRUE, kind = "text"),
                     prioridad = list(label = "Prioridad", required = TRUE, kind = "select"))
      valores <- list(tipo = tipo, titulo = .gestion_texto(input$titulo), prioridad = input$prioridad %||% "")
      reglas <- list(tipo = function(x, valores) {
        if (length(x) == 1L && x %in% c("NOTA", "SEGUIMIENTO")) TRUE else "Seleccione nota o tarea."
      })
      reglas$prioridad <- function(x, valores) {
        if (length(x) == 1L && x %in% names(.colores_prioridad_crm)) TRUE else "Seleccione una prioridad válida."
      }
      if (tarea) {
        campos$seguimiento <- list(label = "Fecha de cumplimiento", required = TRUE, kind = "date")
        valores$seguimiento <- .dp_fecha(as.character(input$seguimiento %||% NA_character_))
        reglas$seguimiento <- function(x, valores) {
          if (x >= .HoyProductos()) TRUE else "La fecha no puede ser anterior a hoy."
        }
      }
      if (tarea && isTRUE(input$incluir_actividades)) {
        ids <- paste0("actividad_", actividades())
        lista <- vapply(ids, function(id) .gestion_texto(input[[id]]), character(1))
        cumplidas <- vapply(actividades(), function(i) isTRUE(estados_actividad[[as.character(i)]]), logical(1))
        campos[ids] <- lapply(seq_along(ids), function(i) {
          list(label = paste("Actividad", i), required = TRUE, kind = "text")
        })
        valores[ids] <- as.list(lista)
      }
      validacion <- racafeForms::racafeValidateForm(valores, campos, rules = reglas, ns = ns)
      racafeForms::racafeShowValidation(session, validacion, mode = "inline")
      if (!validacion$valid) {
        return(racafeForms::racafeNotify(session, "Campos pendientes",
                                        "Revise los campos señalados antes de continuar.", type = "warning"))
      }
      titulo <- valores$titulo
      pendiente(list(tipo = tipo, asunto = titulo, resultado = input$descripcion,
                     fecha = .HoyProductos(), seguimiento = valores$seguimiento,
                     estado = if (tarea) "PENDIENTE" else "REALIZADA",
                     prioridad = valores$prioridad,
                     motivo = if (tarea) "Creación de tarea desde el panel lateral" else
                       "Creación de nota desde el panel lateral",
                     etiquetas = etiquetas_elegidas(), actividades = lista,
                     actividades_cumplidas = lista[cumplidas]))
      showModal(.confirmacion_nota_ui(pendiente(), usuario(), ns))
    })

    observeEvent(input$cancelar, {
      pendiente(NULL)
      removeModal()
    })

    limpiar_formulario <- function() {
      racafeForms::racafeUpdateSelect(session, "tipo", selected = "NOTA")
      updateTextInput(session, "titulo", value = "")
      updateTextAreaInput(session, "descripcion", value = "")
      racafeForms::racafeUpdateDate(session, "seguimiento", value = .HoyProductos() + 1L)
      racafeForms::racafeUpdateSelect(session, "prioridad", selected = "MEDIA")
      racafeForms::racafeUpdateSwitch(session, "incluir_actividades", value = FALSE)
      for (indice in actividades()) quitar_actividad(indice)
      numero_actividad <<- 1L
      limpiar_etiquetas(TRUE)
      if (is.null(etiquetas_modulo)) {
        updateSelectizeInput(session, "etiquetas", selected = character())
      } else etiquetas_modulo$actualizar(etiquetas_modulo$choices(), character())
    }

    observeEvent(input$confirmar, {
      campos <- pendiente()
      req(!is.null(campos))
      pendiente(NULL)
      on.exit({
        if (.disponible_api("racafeShiny", "racafeUpdateOverlayLoader")) {
          racafeShiny::racafeUpdateOverlayLoader(session, "guardar", FALSE)
        }
        removeModal()
      }, add = TRUE)
      guardado <- tryCatch({
        .gestion_guardar(raiz, "tarea", campos, origen = NULL, usuario = identidad$usuario,
                         clientes = NULL, pedidos = NULL, grupos = identidad$grupos, sesion_local = identidad$local)
        TRUE
      }, error = function(e) {
        racafeForms::racafeNotify(session, "No se pudo guardar el registro", conditionMessage(e), type = "error")
        FALSE
      })
      if (!guardado) return(invisible(NULL))
      limpiar_formulario()
      actualizado <- tryCatch({
        al_guardar()
        TRUE
      }, error = function(e) {
        racafeForms::racafeNotify(session, "Registro guardado",
                                  "No se pudo refrescar el tablero de notas y tareas.", type = "warning")
        FALSE
      })
      if (actualizado) racafeForms::racafeNotify(session, "Registro guardado", type = "success")
    })
  })
}
