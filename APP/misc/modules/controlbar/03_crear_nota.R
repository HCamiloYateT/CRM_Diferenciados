# Captura de notas y tareas ----

.etiquetas_notas_corporativas <- function() {
  .disponible_api("racafeModulos", "ListaDesplegableAgregableUI") &&
    .disponible_api("racafeModulos", "ListaDesplegableAgregable")
}

.actividad_nota_ui <- function(ns, id) {
  div(id = ns(paste0("fila_", id)), class = "crm-nota-actividad",
      checkboxInput(ns(paste0("cumplida_", id)), "Cumplida"),
      racafeForms::racafeInputText(paste0("actividad_", id), "Actividad", placeholder = "Describa la actividad",
                                   ns = ns),
      racafeForms::racafeButton(paste0("quitar_", id), label = NULL, icon = "trash", title = "Eliminar actividad",
                                variant = "danger", size = "xxs", ns = ns))
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
        ),
        conditionalPanel("input.tipo === 'SEGUIMIENTO'", ns = ns,
                         div(class = "crm-nota-selectize", .selector_clientes(ns("cliente"), "Cliente")))
      ),
      racafeForms::racafeFormGroup(
        racafeForms::racafeInputTextArea("descripcion", "Descripción", rows = 4, ns = ns)
      ),
      conditionalPanel(
        "input.tipo === 'SEGUIMIENTO'", ns = ns,
        racafeForms::racafeFormGroup(
          racafeForms::racafeInputDate("seguimiento", "Fecha de cumplimiento", value = .HoyProductos() + 1L,
                                       min = .HoyProductos(), required = TRUE, ns = ns),
          uiOutput(ns("asignacion"))
        )
      ),
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
            div(class = "d-flex justify-content-end",
                racafeForms::racafeButton("agregar_actividad", "Añadir", icon = "plus", size = "xxs", ns = ns)),
            div(id = ns("lista_actividades"), .actividad_nota_ui(ns, 1L))
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

CrearNotas <- function(id, publicaciones, raiz, al_guardar) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pendiente <- reactiveVal(NULL)
    actividades <- reactiveVal(1L)
    siguiente_actividad <- 1L
    observadores <- list()
    limpiar_etiquetas <- reactiveVal(FALSE)
    clientes <- reactive(.tabla_fuente(publicaciones(), "clientes"))

    observeEvent(clientes(), {
      .actualizar_selector_clientes(session, "cliente", clientes(), input$cliente)
    }, ignoreNULL = FALSE)

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
                                       selected = "MEDIA", empty = FALSE, search = FALSE, ns = ns),
        racafeForms::racafeInputText("responsable", "Responsable", value = toupper(session$user %||% "HCYATE"),
                                     disabled = TRUE, ns = ns),
        responsive = "fixed"
      )
    })

    quitar_actividad <- function(indice) {
      if (length(actividades()) == 1L) {
        updateTextInput(session, paste0("actividad_", indice), value = "")
        updateCheckboxInput(session, paste0("cumplida_", indice), value = FALSE)
        return(invisible(NULL))
      }
      removeUI(selector = paste0("#", ns(paste0("fila_", indice))), immediate = TRUE)
      actividades(setdiff(actividades(), indice))
      clave <- as.character(indice)
      observadores[[clave]]$destroy()
      observadores[[clave]] <<- NULL
    }
    observar_actividad <- function(indice) {
      force(indice)
      observadores[[as.character(indice)]] <<- observeEvent(input[[paste0("quitar_", indice)]],
                                                            quitar_actividad(indice), ignoreNULL = TRUE)
    }
    observar_actividad(1L)
    observeEvent(input$agregar_actividad, {
      siguiente_actividad <<- siguiente_actividad + 1L
      indice <- siguiente_actividad
      insertUI(selector = paste0("#", ns("lista_actividades")), where = "beforeEnd",
               ui = .actividad_nota_ui(ns, indice), immediate = TRUE)
      actividades(c(actividades(), indice))
      observar_actividad(indice)
    })

    observeEvent(input$crear, {
      pendiente(NULL)
      if (!.prod_autorizado(session$user, Sys.getenv("CRM_GESTION_USUARIOS"))) {
        return(showNotification("La captura requiere una cuenta autorizada para gestión.", type = "warning"))
      }
      titulo <- .gestion_texto(input$titulo)
      tipo <- input$tipo %||% ""
      tarea <- identical(tipo, "SEGUIMIENTO")
      cliente <- if (tarea) .gestion_texto(input$cliente) else NA_character_
      seguimiento <- if (tarea) .dp_fecha(as.character(input$seguimiento %||% NA_character_)) else as.Date(NA)
      condiciones <- c(
        "Seleccione un cliente del corte operacional." = tarea && !cliente %in% clientes()$PerCod,
        "Ingrese el título." = is.na(titulo),
        "Seleccione nota o tarea." = !tipo %in% c("NOTA", "SEGUIMIENTO"),
        "Seleccione una fecha de cumplimiento válida." = tarea && (is.na(seguimiento) || seguimiento < .HoyProductos())
      )
      if (any(condiciones)) {
        for (mensaje in names(condiciones)[condiciones]) showNotification(mensaje, type = "warning")
        return(invisible(NULL))
      }
      lista <- character()
      cumplidas <- logical()
      if (tarea && isTRUE(input$incluir_actividades)) {
        lista <- vapply(actividades(), function(i) .gestion_texto(input[[paste0("actividad_", i)]]), character(1))
        cumplidas <- vapply(actividades(), function(i) isTRUE(input[[paste0("cumplida_", i)]]), logical(1))
      }
      pendiente(list(PerCod = cliente, tipo = tipo, asunto = titulo, resultado = input$descripcion,
                     fecha = .HoyProductos(), seguimiento = seguimiento,
                     estado = if (tarea) "PENDIENTE" else "REALIZADA",
                     prioridad = if (tarea) input$prioridad else "MEDIA",
                     motivo = if (tarea) "Creación de tarea desde el panel lateral" else
                       "Creación de nota desde el panel lateral",
                     etiquetas = etiquetas_elegidas(), actividades = lista,
                     actividades_cumplidas = lista[cumplidas]))
      showModal(modalDialog(
        title = if (tarea) "Confirmar tarea" else "Confirmar nota", tags$p(titulo),
        footer = tagList(modalButton("Cancelar"), .boton(ns("confirmar"), "Guardar", "check"))
      ))
    })

    observeEvent(input$confirmar, {
      campos <- pendiente()
      req(!is.null(campos))
      pendiente(NULL)
      removeModal()
      tryCatch({
        pedidos <- .tabla_fuente(publicaciones(), "pedidos")
        .gestion_guardar(raiz, "tarea", campos, origen = NULL, usuario = session$user,
                         clientes = clientes(), pedidos = pedidos)
        for (indice in actividades()) quitar_actividad(indice)
        shinyWidgets::updateMaterialSwitch(session, "incluir_actividades", value = FALSE)
        limpiar_etiquetas(TRUE)
        al_guardar()
        updateTextInput(session, "titulo", value = "")
        updateTextAreaInput(session, "descripcion", value = "")
        showNotification("Registro guardado.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
  })
}
