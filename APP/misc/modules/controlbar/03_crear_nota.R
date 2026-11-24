# Captura de notas y tareas ----

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
        .selector_clientes(ns("cliente"), "Cliente")
      ),
      racafeForms::racafeFormGroup(
        racafeForms::racafeInputTextArea("descripcion", "Descripción", rows = 4, ns = ns)
      ),
      conditionalPanel(
        "input.tipo === 'SEGUIMIENTO'", ns = ns,
        racafeForms::racafeFormGroup(
          racafeForms::racafeInputDate("seguimiento", "Fecha de cumplimiento", value = .HoyProductos() + 1L,
                                       min = .HoyProductos(), required = TRUE, ns = ns),
          racafeForms::racafeFormRow(
            racafeForms::racafeInputSelect("prioridad", "Prioridad", names(.colores_prioridad_crm),
                                           selected = "MEDIA", empty = FALSE, search = FALSE, ns = ns),
            uiOutput(ns("responsable")),
            responsive = "fixed"
          )
        )
      ),
      racafeForms::racafeFormGroup(
        racafeForms::racafeInputText("motivo", "Motivo del registro", required = TRUE, ns = ns)
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
    clientes <- reactive(.tabla_fuente(publicaciones(), "clientes"))

    observeEvent(clientes(), {
      .actualizar_selector_clientes(session, "cliente", clientes(), input$cliente)
    }, ignoreNULL = FALSE)

    output$responsable <- renderUI({
      racafeForms::racafeInputReadOnly("responsable", "Responsable", session$user %||% "", ns = ns)
    })

    observeEvent(input$crear, {
      pendiente(NULL)
      if (!.prod_autorizado(session$user, Sys.getenv("CRM_GESTION_USUARIOS"))) {
        return(showNotification("La captura requiere una cuenta autorizada para gestión.", type = "warning"))
      }
      cliente <- .gestion_texto(input$cliente)
      titulo <- .gestion_texto(input$titulo)
      motivo <- .gestion_texto(input$motivo)
      tipo <- input$tipo %||% ""
      tarea <- identical(tipo, "SEGUIMIENTO")
      seguimiento <- if (tarea) .dp_fecha(as.character(input$seguimiento %||% NA_character_)) else as.Date(NA)
      condiciones <- c(
        "Seleccione un cliente del corte operacional." = !cliente %in% clientes()$PerCod,
        "Ingrese el título." = is.na(titulo),
        "Ingrese el motivo del registro." = is.na(motivo),
        "Seleccione nota o tarea." = !tipo %in% c("NOTA", "SEGUIMIENTO"),
        "Seleccione una fecha de cumplimiento válida." = tarea && (is.na(seguimiento) || seguimiento < .HoyProductos())
      )
      if (any(condiciones)) {
        for (mensaje in names(condiciones)[condiciones]) showNotification(mensaje, type = "warning")
        return(invisible(NULL))
      }
      pendiente(list(PerCod = cliente, tipo = tipo, asunto = titulo, resultado = input$descripcion,
                     fecha = .HoyProductos(), seguimiento = seguimiento,
                     estado = if (tarea) "PENDIENTE" else "REALIZADA",
                     prioridad = if (tarea) input$prioridad else "MEDIA", motivo = motivo))
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
        al_guardar()
        updateTextInput(session, "titulo", value = "")
        updateTextAreaInput(session, "descripcion", value = "")
        updateTextInput(session, "motivo", value = "")
        showNotification("Registro guardado.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
  })
}
