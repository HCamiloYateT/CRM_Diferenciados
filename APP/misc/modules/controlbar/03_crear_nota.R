# Captura de notas y tareas ----

.etiquetas_notas_corporativas <- function() {
  .disponible_api("racafeModulos", "ListaDesplegableAgregableUI") &&
    .disponible_api("racafeModulos", "ListaDesplegableAgregable")
}

.actividad_nota_ui <- function(ns, id, numero = id, valor = "", detalle = NULL) {
  div(id = ns(paste0("fila_", id)), class = "nota-actividad-row",
      div(class = "nota-actividad-texto",
          racafeForms::racafeInputText(paste0("actividad_", id), label = NULL, value = valor,
                                       placeholder = paste("Actividad", numero), ns = ns), detalle),
      div(class = "nota-actividad-accion", uiOutput(ns(paste0("estado_actividad_", id)))),
      div(class = "nota-actividad-accion",
          racafeForms::racafeButton(paste0("quitar_", id), label = NULL, icon = "trash", title = "Eliminar actividad",
                                    variant = "danger", size = "xxs", ns = ns)))
}

.boton_estado_actividad <- function(ns, id, cumplida) {
  titulo <- if (cumplida) "Marcar como pendiente" else "Marcar como cumplida"
  tagAppendAttributes(
    racafeForms::racafeButton(id, label = NULL, icon = if (cumplida) "rotate-left" else "check",
                              title = titulo, variant = "primary", size = "xxs", ns = ns),
    `aria-label` = titulo, `aria-pressed` = if (cumplida) "true" else "false"
  )
}

CrearNotasUI <- function(id, en_modal = FALSE) {
  ns <- NS(id)
  formulario <- racafeForms::racafeForm(
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
  if (en_modal) return(div(class = "w-100", formulario))
  racafeForms::racafePanel(title = NULL, formulario, padding = "compact", bordered = FALSE)
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

.enviar_asignacion_nota <- function(campos, creador, responsable) {
  usuario <- sub("@racafe[.]com$", "", tolower(trimws(responsable)))
  if (!grepl("^[a-z0-9._-]+$", usuario)) stop("El responsable no tiene un usuario de correo válido.")
  para <- paste0(usuario, "@racafe.com")
  tarea <- identical(campos$tipo, "SEGUIMIENTO")
  fecha <- format(campos$fecha, "%d/%m/%y")
  descripcion <- .gestion_texto(campos$resultado)
  cabecera <- if (tarea) "Se asignó una tarea en CRM Diferenciados." else "Se asignó una nota en CRM Diferenciados."
  mensaje <- paste(c(cabecera,
                   paste0("Título: ", campos$asunto), paste0("Creado por: ", creador),
                   paste0("Fecha de creación: ", fecha), paste0("Asignado a: ", responsable),
                   if (tarea) paste0("Vencimiento: ", .formato_fecha_crm(campos$seguimiento)),
                   if (!is.na(descripcion)) paste0("Descripción: ", descripcion)), collapse = " · ")
  if (!requireNamespace("racafeMail", quietly = TRUE)) stop("El paquete racafeMail no está instalado.")
  config <- racafeMail::racafeMailConfig()
  if (!nzchar(config$remitente) || !nzchar(config$credenciales) || !file.exists(config$credenciales) ||
      dir.exists(config$credenciales) || file.access(config$credenciales, 4L) != 0L) {
    stop("Falta configurar el remitente o las credenciales SMTP de racafeMail.")
  }
  racafeMail::racafeMailNotificacion(para = para, nombre = responsable, mensaje = mensaje, config = config)
}

CrearNotas <- function(id, publicaciones, raiz, al_guardar, usuario = NULL, abrir = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    identidad <- .crm_identidad_notas(session)
    en_modal <- !is.null(abrir)
    editar <- if (!en_modal) .EditarNotas("edicion", publicaciones, raiz, identidad, al_guardar)
    if (en_modal) {
      observeEvent(abrir(), {
        req(abrir() > 0L)
        showModal(racafeForms::racafeModalDetail(
          title = "Nueva tarea o nota", size = "subventana3", CrearNotasUI(id, en_modal = TRUE),
          footer = racafeForms::racafeModalFooter(racafeForms::racafeButtonCancel("cerrar", ns = ns))
        ))
      }, priority = 10)
    }
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
    observeEvent(list(publicaciones(), if (en_modal) abrir()), {
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
        .boton_estado_actividad(ns, boton, isTRUE(estados_actividad[[clave]]))
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
                     motivo = if (tarea) "Creación de tarea" else "Creación de nota",
                     etiquetas = etiquetas_elegidas(), actividades = lista,
                     actividades_cumplidas = lista[cumplidas]))
      if (en_modal) {
        titulo_confirmacion <- if (tarea) "¿Crear esta tarea?" else "¿Crear esta nota?"
        racafeForms::racafeConfirm(session, "confirmar_modal", titulo_confirmacion, titulo, ns = ns)
      } else showModal(.confirmacion_nota_ui(pendiente(), usuario(), ns))
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

    observeEvent(input$cerrar, {
      pendiente(NULL)
      limpiar_formulario()
      removeModal()
    })

    observeEvent(if (en_modal) input$confirmar_modal else input$confirmar, {
      campos <- pendiente()
      pendiente(NULL)
      req(!is.null(campos), !en_modal || isTRUE(input$confirmar_modal))
      on.exit({
        if (.disponible_api("racafeShiny", "racafeUpdateOverlayLoader")) {
          racafeShiny::racafeUpdateOverlayLoader(session, "guardar", FALSE)
        }
        if (!en_modal) removeModal()
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
      if (en_modal) removeModal()
      actualizado <- tryCatch({
        al_guardar()
        TRUE
      }, error = function(e) {
        racafeForms::racafeNotify(session, "Registro guardado",
                                  "No se pudo refrescar el tablero de notas y tareas.", type = "warning")
        FALSE
      })
      if (actualizado) racafeForms::racafeNotify(session, "Registro guardado", type = "success")
      tryCatch({
        responsable <- identidad$usuario
        .enviar_asignacion_nota(campos, identidad$usuario, responsable)
      }, error = function(e) {
        detalle <- conditionMessage(e)
        if (!startsWith(detalle, "Falta configurar") && !startsWith(detalle, "El paquete racafeMail")) {
          detalle <- "Verifique el servicio SMTP y el correo del responsable."
        }
        racafeForms::racafeNotify(session, "Registro guardado. Correo sin enviar", detalle, type = "warning")
      })
    })
    editar
  })
}
