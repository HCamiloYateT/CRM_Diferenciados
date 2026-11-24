# Agenda, oportunidades, ofertas, muestras y contactos ----

.gestion_vistas <- c(contactos = "contacto", actividades = "tarea", tareas = "tarea", notas = "tarea",
                     oportunidades = "oportunidad", negociaciones = "oferta", muestras = "muestra",
                     solicitudes = "muestra", envios = "muestra", evaluaciones = "muestra", seguimiento = "muestra",
                     indicadores = "muestra")

.gestion_tablas <- c(contacto = "contactos", tarea = "tareas", oportunidad = "oportunidades",
                     oferta = "ofertas", muestra = "muestras")

GestionUI <- function(id, titulo) {
  ns <- shiny::NS(id)
  div(class = "crm-vista", h1(titulo),
      div(class = "crm-indicadores-filtros",
          .selector_clientes(ns("cliente"), "Filtrar cliente"),
          .boton(ns("nuevo"), "Registrar", "plus"), .boton(ns("editar"), "Actualizar registro", "pen"),
          if (unname(.gestion_vistas[id]) %in% c("oportunidad", "muestra")) {
            .boton(ns("proxima"), "Próxima acción", "calendar-check")
          }),
      uiOutput(ns("seleccion")), uiOutput(ns("estado")),
      if (id %in% c("notas", "tareas")) uiOutput(ns("kanban")) else .tabla_ui(ns("tabla")))
}

.gestion_agenda <- function(tareas, hoy = .HoyProductos()) {
  tareas$vencida <- tareas$estado == "PENDIENTE" & !is.na(tareas$seguimiento) & tareas$seguimiento < hoy
  tareas[order(tareas$estado != "PENDIENTE", tareas$seguimiento, -as.numeric(tareas$fecha), na.last = TRUE), ,
         drop = FALSE]
}

GestionServer <- function(id, publicaciones, operativo, abrir) {
  shiny::moduleServer(id, function(input, output, session) {
    tipo <- unname(.gestion_vistas[id])
    tabla <- unname(.gestion_tablas[tipo])
    datos <- shiny::reactive({
      t <- publicaciones()$crm$tablas
      d <- if (tipo == "oportunidad") {
        salida <- .gestion_oportunidades(t)
        agenda <- .gestion_agenda(.gestion_filas(t, "tareas"))
        agenda <- agenda[agenda$estado == "PENDIENTE" & !is.na(agenda$oportunidad_id), , drop = FALSE]
        pos <- match(salida$oportunidad_id, agenda$oportunidad_id)
        salida$proxima_accion <- agenda$asunto[pos]
        salida$fecha_proxima_accion <- agenda$seguimiento[pos]
        salida
      } else .gestion_filas(t, tabla)
      clientes <- operativo()$clientes
      if (!is.null(clientes)) d <- d[d$PerCod %in% clientes$PerCod, , drop = FALSE]
      if (nzchar(input$cliente %||% "")) d <- d[d$PerCod == input$cliente, , drop = FALSE]
      if (tipo == "tarea") {
        d <- .gestion_agenda(d)
        prioridades <- t$tarea_prioridad
        d$prioridad <- prioridades$prioridad[match(d$tarea_id, prioridades$tarea_id)]
        if (is.null(d$prioridad)) d$prioridad <- rep(NA_character_, nrow(d))
        d$prioridad[is.na(d$prioridad)] <- "MEDIA"
        if (id == "notas") d <- d[d$tipo == "NOTA", , drop = FALSE]
        if (id == "actividades") d <- d[d$estado == "REALIZADA", , drop = FALSE]
        if (id == "tareas") d <- d[d$estado == "PENDIENTE" & d$tipo != "NOTA", , drop = FALSE]
      }
      if (id == "solicitudes") d <- d[d$estado %in% c("SOLICITADA", "EN_PREPARACION"), , drop = FALSE]
      if (id == "envios") d <- d[d$estado %in% c("ENVIADA", "PENDIENTE_RESPUESTA"), , drop = FALSE]
      if (id == "evaluaciones") d <- d[d$estado %in% c("APROBADA", "RECHAZADA"), , drop = FALSE]
      if (id == "seguimiento") d <- d[d$estado == "APROBADA" & is.na(d$pedido_id), , drop = FALSE]
      d
    })
    observeEvent(operativo()$clientes, {
      .actualizar_selector_clientes(session, "cliente", operativo()$clientes, input$cliente)
    }, ignoreNULL = FALSE)
    llave <- .gestion_contratos()[[tabla]]$clave
    output$seleccion <- renderUI({
      d <- datos()
      if (!nrow(d)) return(NULL)
      etiqueta <- intersect(c("asunto", "producto", "nombre"), names(d))[1L]
      d <- .etiquetar_clientes(d, operativo()$clientes)
      opciones <- stats::setNames(d[[llave]], paste(d$PerCod, d[[etiqueta]], d[[llave]], sep = " · "))
      selectInput(session$ns("registro"), "Registro", c("Seleccione…" = "", opciones),
                  selected = isolate(input$registro))
    })
    output$estado <- renderUI({
      if (inherits(publicaciones()$crm, "error")) return(.nota("No se pudo leer la gestión comercial.", "error"))
      if (id == "seguimiento") return(.nota("Muestras aprobadas sin pedido vinculado."))
      if (!nrow(datos())) return(.nota("Sin registros para los filtros activos.", "empty"))
      NULL
    })
    if (id %in% c("notas", "tareas")) {
      output$kanban <- renderUI({
        d <- .etiquetar_clientes(datos(), operativo()$clientes)
        if (!nrow(d)) return(NULL)
        prioridades <- c("ALTA", "MEDIA", "BAJA")
        columnas <- lapply(prioridades, function(prioridad) {
          filas <- d[which(d$prioridad == prioridad), , drop = FALSE]
          tarjetas <- lapply(seq_len(nrow(filas)), function(i) {
            cuerpo <- div(class = "crm-panel", tags$p(filas$PerCod[i]), tags$p(filas$resultado[i]),
                           tags$p(as.character(filas$seguimiento[i])))
            if (.disponible_api("racafeModulos", "TarjetaKanbanUI")) {
              return(racafeModulos::TarjetaKanbanUI(filas$asunto[i], cuerpo = cuerpo))
            }
            div(class = "crm-panel", strong(filas$asunto[i]), cuerpo)
          })
          if (.disponible_api("racafeModulos", "ColumnaKanbanUI")) {
            return(racafeModulos::ColumnaKanbanUI(prioridad, tarjetas, nrow(filas)))
          }
          div(class = "crm-panel", h2(prioridad), tarjetas)
        })
        if (.disponible_api("racafeModulos", "TableroKanbanUI")) {
          return(do.call(racafeModulos::TableroKanbanUI, columnas))
        }
        div(class = "crm-kpis", columnas)
      })
    } else .tabla_server("tabla", reactive(.etiquetar_clientes(datos(), operativo()$clientes)), output)
    observeEvent(input$nuevo, abrir(tipo, cliente = input$cliente, vista = id))
    observeEvent(input$proxima, {
      d <- datos()
      fila <- d[which(d[[llave]] == input$registro), , drop = FALSE]
      if (nrow(fila) != 1L) return(showNotification("Seleccione un registro.", type = "warning"))
      vinculo <- if (tipo == "oportunidad") "oportunidad_id" else "muestra_id"
      abrir("tarea", cliente = fila$PerCod, vinculos = stats::setNames(list(input$registro), vinculo))
    })
    observeEvent(input$editar, {
      if (!nzchar(input$registro %||% "")) {
        return(showNotification("Seleccione un registro para actualizar.", type = "warning"))
      }
      req(input$registro %in% datos()[[llave]])
      abrir(tipo, origen = input$registro, vista = id)
    })
  })
}

# Formulario compartido por los accesos de gestión ----

.GestionCapturaServer <- function(publicaciones, raiz, al_guardar) {
  session <- shiny::getDefaultReactiveDomain()
  input <- session$input
  contexto <- shiny::reactiveVal(NULL)
  cliente_elegido <- function() input$gestion_cliente %||% ""
  opciones <- function(tabla, clave, etiqueta, cliente) {
    tabla <- tabla[which(tabla$PerCod == cliente), , drop = FALSE]
    stats::setNames(tabla[[clave]], paste(tabla[[clave]], tabla[[etiqueta]], sep = " · "))
  }
  actualizar_vinculos <- function(cliente, valores = list()) {
    t <- publicaciones()$crm$tablas
    pedidos <- publicaciones()$operacional$tablas$pedidos
    cargar <- function(id, choices, seleccionado) {
      updateSelectizeInput(session, id, choices = c("Sin vínculo" = "", choices),
                            selected = if (is.na(seleccionado %||% NA_character_)) "" else seleccionado, server = TRUE)
    }
    cargar("gestion_oportunidad_id", opciones(.gestion_oportunidades(t), "oportunidad_id", "asunto", cliente),
            valores$oportunidad_id)
    cargar("gestion_muestra_id", opciones(.gestion_filas(t, "muestras"), "muestra_id", "producto", cliente),
            valores$muestra_id)
    if (!is.null(pedidos)) {
      cargar("gestion_pedido_id", opciones(pedidos, "pedido_id", "PdcCod", cliente), valores$pedido_id)
    }
  }
  observeEvent(input$gestion_cliente, {
    ctx <- contexto()
    if (!is.null(ctx)) {
      valores <- if (identical(cliente_elegido(), ctx$cliente)) ctx$valores else list()
      actualizar_vinculos(cliente_elegido(), valores)
    }
  })
  observeEvent(input$gestion_guardar, {
    ctx <- contexto()
    req(!is.null(ctx))
    campos <- c("estado", "pedido_id", "motivo", "oportunidad_id", "muestra_id", "tipo", "asunto", "producto",
                "sacos", "embarque", "valor", "unidad", "condiciones", "fecha", "calidad", "resultado",
                "seguimiento", "correo", "telefono", "prioridad")
    valores <- stats::setNames(lapply(campos, function(x) input[[paste0("gestion_", x)]]), campos)
    valores$PerCod <- cliente_elegido()
    if (!is.null(ctx$origen) && !identical(valores$PerCod, ctx$cliente)) {
      return(showNotification("El PerCod del registro no puede cambiar.", type = "error"))
    }
    tryCatch({
      t <- publicaciones()$operacional$tablas
      .gestion_guardar(raiz, ctx$tipo, valores, ctx$origen, session$user, t$clientes, t$pedidos, ctx$esperado)
      contexto(NULL)
      removeModal()
      al_guardar()
      showNotification("Gestión guardada.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })

  function(tipo, origen = NULL, cliente = NULL, vista = NULL, vinculos = list()) {
    if (!.prod_autorizado(session$user, Sys.getenv("CRM_GESTION_USUARIOS"))) {
      showNotification("La captura requiere una cuenta autorizada para gestión.", type = "warning")
      return(invisible(NULL))
    }
    clientes <- .tabla_fuente(publicaciones(), "clientes")
    if (is.null(clientes) || !nrow(clientes)) {
      showNotification("Publique un corte con clientes para registrar gestión.", type = "warning")
      return(invisible(NULL))
    }
    valores <- vinculos
    esperado <- NULL
    if (!is.null(origen)) {
      t <- publicaciones()$crm$tablas
      nombre <- unname(.gestion_tablas[tipo])
      tabla <- .gestion_filas(t, nombre)
      llave <- .gestion_contratos()[[nombre]]$clave
      fila <- tabla[which(tabla[[llave]] == origen), , drop = FALSE]
      req(nrow(fila) == 1L)
      cliente <- fila$PerCod
      valores <- as.list(fila)
      if (tipo == "tarea") {
        prioridad <- t$tarea_prioridad
        valores$prioridad <- prioridad$prioridad[match(origen, prioridad$tarea_id)]
      }
      if (tipo == "contacto") {
        valores$asunto <- fila$nombre
        valores$calidad <- fila$preferencia_calidad
      }
      detalle <- .gestion_filas(t, "oportunidad_detalle")
      detalle <- detalle[detalle$oportunidad_id == origen, , drop = FALSE]
      if (tipo == "oportunidad") {
        datos <- .gestion_oportunidades(t)
        valores <- as.list(datos[datos$oportunidad_id == origen, , drop = FALSE])
      }
      esperado <- list(fila = fila, detalle = detalle)
    }
    campo <- function(nombre) {
      valor <- valores[[nombre]]
      if (is.null(valor) || is.na(valor)) "" else valor
    }
    texto <- function(id, label) textInput(paste0("gestion_", id), label, value = campo(id))
    numero <- function(id, label) {
      valor <- campo(id)
      numericInput(paste0("gestion_", id), label, value = if (identical(valor, "")) NA_real_ else valor)
    }
    fecha <- function(id, label, inicial = NULL, origen = id) {
      id <- paste0("gestion_", id)
      valor <- campo(origen)
      if (identical(valor, "")) valor <- inicial
      textInput(id, label, value = as.character(valor %||% ""), placeholder = "AAAA-MM-DD")
    }
    elegir <- function(id, label, choices, inicial = choices[[1L]]) {
      valor <- campo(id)
      selectInput(paste0("gestion_", id), label, choices,
                  selected = if (identical(valor, "")) inicial else valor, selectize = FALSE)
    }
    vinculo <- function(id, label) selectizeInput(paste0("gestion_", id), label, choices = NULL)
    hoy <- .HoyProductos()
    controles <- switch(tipo,
      contacto = tagList(texto("asunto", "Nombre del contacto"), texto("correo", "Correo"),
                         texto("telefono", "Teléfono"), texto("calidad", "Preferencia de calidad")),
      oportunidad = tagList(elegir("tipo", "Tipo", c("ADQUISICION", "CROSS_SELLING", "UP_SELLING")),
        texto("asunto", "Asunto"), texto("producto", "Producto"), numero("sacos", "Volumen potencial (sacos)"),
        fecha("embarque", "Embarque estimado", origen = "embarque_estimado"), vinculo("pedido_id", "Pedido vinculado")),
      oferta = tagList(vinculo("oportunidad_id", "Oportunidad"), texto("producto", "Producto"),
        numero("sacos", "Sacos ofrecidos"), numero("valor", "Precio o diferencial"), texto("unidad", "Moneda y unidad"),
        texto("condiciones", "Condiciones comerciales"), fecha("fecha", "Fecha de oferta", hoy),
        fecha("embarque", "Embarque estimado", origen = "embarque_estimado")),
      muestra = tagList(texto("producto", "Producto"), texto("calidad", "Calidad"),
        fecha("fecha", "Fecha de solicitud", hoy), texto("resultado", "Respuesta o resultado"),
        vinculo("pedido_id", "Pedido vinculado")),
      tarea = tagList(texto("asunto", "Asunto"),
        elegir("tipo", "Tipo", c("LLAMADA", "REUNION", "COMPROMISO", "SEGUIMIENTO", "NOTA"),
               if (identical(vista, "notas")) "NOTA" else "LLAMADA"),
        elegir("prioridad", "Prioridad", c("ALTA", "MEDIA", "BAJA"), "MEDIA"),
        fecha("fecha", "Fecha de actividad", hoy), fecha("seguimiento", "Próximo seguimiento"),
        texto("resultado", "Resultado"), vinculo("oportunidad_id", "Oportunidad"), vinculo("muestra_id", "Muestra")))
    estado <- .gestion_estados[[tipo]]
    cliente <- cliente %||% ""
    contexto(list(tipo = tipo, origen = origen, cliente = cliente, valores = valores, esperado = esperado))
    cuerpo <- div(class = "crm-form-grid", .selector_clientes("gestion_cliente", "Cliente"), controles,
                 if (length(estado)) elegir("estado", "Estado", estado,
                                           if (isTRUE(vista %in% c("actividades", "notas"))) "REALIZADA" else estado[[1L]]),
                 textInput("gestion_motivo", "Motivo del registro"))
    if (.disponible_api("racafeForms", "racafeForm")) cuerpo <- racafeForms::racafeForm(cuerpo, variant = "plain")
    showModal(modalDialog(title = if (is.null(origen)) "Registrar gestión" else "Actualizar gestión",
                          cuerpo, footer = tagList(modalButton("Cancelar"),
                                                    .boton("gestion_guardar", "Confirmar registro", "check"))))
    session$onFlushed(function() {
      .actualizar_selector_clientes(session, "gestion_cliente", clientes, cliente)
      actualizar_vinculos(cliente, valores)
    }, once = TRUE)
  }
}
