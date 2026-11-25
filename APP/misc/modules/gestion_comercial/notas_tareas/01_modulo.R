# Tablero de notas y tareas ----

TableroTareasUI <- function(id) {
  ns <- NS(id)
  div(class = "crm-vista crm-tareas-form", h1("Notas y tareas"),
      racafeForms::racafeForm(
        variant = "plain",
        racafeForms::racafeActionBar(left = tagAppendAttributes(
          racafeForms::racafeButton("crear", "Nueva tarea o nota", icon = "plus", ns = ns),
          `data-rf-controlbar-tab` = "Tareas y notas"
        )),
        racafeForms::racafeFormRow(
          racafeForms::racafeInputText("buscar", "Buscar", placeholder = "Título, descripción o actividad", ns = ns),
          uiOutput(ns("filtro_etiquetas")), responsive = "auto"
        ),
        div(class = "crm-tareas-switches",
            racafeForms::racafeInputSwitch("archivadas", "Incluir archivadas", ns = ns),
            racafeForms::racafeInputSwitch("cumplidas", "Incluir cumplidas", ns = ns)),
        uiOutput(ns("tablero"))
      ))
}

.tablero_notas_datos <- function(tablas, usuario, grupos = character(), sesion_local = FALSE) {
  d <- .gestion_filas(tablas, "tareas")
  propios <- toupper(d$responsable) %in% toupper(usuario)
  if (sesion_local) propios <- propios & is.na(d$PerCod)
  admin <- !sesion_local && .crm_es_admin(usuario, grupos)
  d <- d[which((admin | propios) &
                d$tipo %in% c("NOTA", "SEGUIMIENTO")), ,
         drop = FALSE]
  prioridades <- .gestion_filas(tablas, "tarea_prioridad")
  archivo <- .gestion_filas(tablas, "tarea_archivo")
  d$Prioridad <- prioridades$prioridad[match(d$tarea_id, prioridades$tarea_id)]
  d$Prioridad[d$Prioridad %in% "ALTA"] <- "IMPORTANTE"
  d$Prioridad[!d$Prioridad %in% names(.colores_prioridad_crm)] <- "MEDIA"
  d$Tipo <- ifelse(d$tipo == "NOTA", "text", "list")
  d$Cumplido <- d$Tipo == "list" & d$estado == "REALIZADA"
  d$Archivado <- d$tarea_id %in% archivo$tarea_id | d$estado == "CANCELADA"
  d[order(d$seguimiento, -as.numeric(d$fecha), na.last = TRUE), , drop = FALSE]
}

.tablero_notas_actuar <- function(raiz, id, accion, usuario, grupos = character(), sesion_local = FALSE) {
  if (!.crm_usuario_autenticado(usuario)) {
    stop("La cuenta no tiene permiso para modificar gestión.", call. = FALSE)
  }
  .gestion_actualizar(raiz, function(t) {
    d <- .tablero_notas_datos(t, usuario, grupos, sesion_local)
    fila <- d[d$tarea_id %in% id, , drop = FALSE]
    if (nrow(fila) != 1L) stop("El registro ya no está disponible.", call. = FALSE)
    posicion <- match(id, t$tareas$tarea_id)
    if (accion == "archivar" && !fila$Archivado) {
      t$tarea_archivo <- .gestion_upsert(t$tarea_archivo, data.frame(tarea_id = id, estado = fila$estado), "tarea_id")
    }
    if (accion == "desarchivar" && fila$Archivado) {
      t$tarea_archivo <- t$tarea_archivo[t$tarea_archivo$tarea_id != id, , drop = FALSE]
      if (fila$estado == "CANCELADA") t$tareas$estado[posicion] <- if (fila$tipo == "NOTA") "REALIZADA" else "PENDIENTE"
    }
    if (accion == "cumplir" && fila$Tipo == "list" && !fila$Cumplido && !fila$Archivado) {
      t$tareas$estado[posicion] <- "REALIZADA"
    }
    if (accion == "eliminar") {
      for (tabla in c("tarea_prioridad", "tarea_etiquetas", "tarea_actividades", "tarea_archivo", "tareas")) {
        t[[tabla]] <- t[[tabla]][t[[tabla]]$tarea_id != id, , drop = FALSE]
      }
    } else t$tareas$actualizada[posicion] <- Sys.time()
    t
  }, usuario, paste("Tablero de notas y tareas", accion), id)
}

.actividades_nota_lista <- function(datos, id) {
  filas <- datos[datos$tarea_id %in% id, , drop = FALSE]
  filas <- filas[order(filas$orden), , drop = FALSE]
  list(textos = filas$actividad, cumplidas = filas$cumplida)
}

.detalle_nota_ui <- function(fila, tablas) {
  actividades <- .actividades_nota_lista(.gestion_filas(tablas, "tarea_actividades"), fila$tarea_id)
  etiquetas <- .gestion_filas(tablas, "tarea_etiquetas")
  racafeForms::racafeModalDetail(
    title = fila$asunto, size = "subventana3",
    tags$p(fila$resultado), tags$p(fila$responsable),
    tags$p(.formato_fecha_crm(fila$seguimiento)),
    racafeForms::racafeInfoItem("ETIQUETAS", .etiquetas_crm_ui(etiquetas$etiqueta[etiquetas$tarea_id %in% fila$tarea_id]),
                               empty = "Sin etiquetas"),
    if (length(actividades$textos)) tags$ul(lapply(seq_along(actividades$textos), function(i) {
      texto <- actividades$textos[i]
      tags$li(if (actividades$cumplidas[i]) tags$del(texto) else texto)
    }))
  )
}

TableroTareas <- function(id, publicaciones, raiz, refrescar, editar) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pendiente <- reactiveVal(NULL)
    identidad <- .crm_identidad_notas(session)
    usuario <- identidad$usuario %||% character()
    grupos <- identidad$grupos
    tablas <- reactive(publicaciones()$crm$tablas)
    notas <- reactive(.tablero_notas_datos(tablas(), usuario, grupos, identidad$local))
    etiquetas <- reactive(.gestion_filas(tablas(), "tarea_etiquetas"))
    lista <- reactive(.gestion_filas(tablas(), "tarea_actividades"))
    actividades <- function(id) .actividades_nota_lista(lista(), id)
    output$filtro_etiquetas <- renderUI({
      opciones <- sort(unique(etiquetas()$etiqueta[etiquetas()$tarea_id %in% notas()$tarea_id]))
      racafeForms::racafeInputSelect("etiquetas", "Etiquetas", c("Sin etiqueta" = "__sin__", opciones),
                                     selected = isolate(input$etiquetas), multiple = TRUE, empty_label = "Todas",
                                     ns = ns)
    })
    seleccion <- reactive({
      d <- notas()
      if (!isTRUE(input$archivadas)) d <- d[!d$Archivado, , drop = FALSE]
      if (!isTRUE(input$cumplidas)) d <- d[!d$Cumplido, , drop = FALSE]
      if (length(input$etiquetas)) {
        e <- etiquetas()
        sin_etiqueta <- if ("__sin__" %in% input$etiquetas) setdiff(d$tarea_id, e$tarea_id) else character()
        d <- d[d$tarea_id %in% c(sin_etiqueta, e$tarea_id[e$etiqueta %in% input$etiquetas]), , drop = FALSE]
      }
      buscar <- trimws(input$buscar %||% "")
      if (nzchar(buscar)) {
        texto <- vapply(d$tarea_id, function(id) paste(actividades(id)$textos, collapse = " "), character(1))
        d <- d[grepl(tolower(buscar), tolower(paste(d$asunto, d$resultado, texto)), fixed = TRUE), , drop = FALSE]
      }
      d
    })
    acciones <- function(fila) {
      disponibles <- c(abrir = "Abrir detalle")
      if (!.crm_usuario_autenticado(usuario)) return(disponibles)
      editar_propio <- is.na(fila$PerCod) || .gestion_autorizada(usuario, grupos)
      c(disponibles, if (editar_propio) c(editar = "Editar"),
        if (fila$Archivado) c(desarchivar = "Desarchivar") else c(archivar = "Archivar"),
        if (fila$Tipo == "list" && !fila$Cumplido && !fila$Archivado) c(cumplir = "Marcar cumplida"),
        eliminar = "Eliminar")
    }
    output$tablero <- renderUI({
      d <- seleccion()
      colores <- .colores_prioridad_crm
      if (.disponible_api("racafeModulos", "TableroTareasTarjetas") &&
          "acciones_fn" %in% names(formals(racafeModulos::TableroTareasTarjetas))) {
        return(racafeModulos::TableroTareasTarjetas(
          d, names(colores), actividades_fn = function(fila) actividades(fila$tarea_id), ns = ns,
          campos = c(id = "tarea_id", titulo = "asunto", descripcion = "resultado", prioridad = "Prioridad",
                     responsable = "responsable", tipo = "Tipo", cumplido = "Cumplido", archivado = "Archivado",
                     vencimiento = "seguimiento"),
          acciones_fn = acciones, colores = colores, nota_fn = .nota, arrastrable = FALSE
        ))
      }
      div(
        if (nrow(d)) racafeForms::racafeFormRow(
          selectInput(ns("registro"), "Registro", setNames(d$tarea_id, d$asunto)),
          uiOutput(ns("operaciones")), responsive = "fixed"
        ),
        div(class = "crm-kpis", lapply(names(colores), function(prioridad) {
        filas <- d[d$Prioridad == prioridad, , drop = FALSE]
        div(class = "crm-panel", h2(prioridad), lapply(seq_len(nrow(filas)), function(i) {
          div(class = "mb-2", strong(filas$asunto[i]),
              tags$p(filas$resultado[i]), tags$small(if (filas$tipo[i] == "NOTA") "Nota" else "Tarea"))
        }))
        }))
      )
    })
    output$operaciones <- renderUI({
      d <- seleccion()
      fila <- d[d$tarea_id %in% input$registro, , drop = FALSE]
      if (!nrow(fila)) return(NULL)
      opciones <- acciones(fila)
      tagList(selectInput(ns("operacion"), "Acción", setNames(names(opciones), opciones)),
              .boton(ns("ejecutar"), "Abrir acción", "arrow-right"))
    })
    actuar <- function(evento) {
      d <- seleccion()
      fila <- d[d$tarea_id %in% evento$codigo, , drop = FALSE]
      req(nrow(fila) == 1L, evento$accion %in% names(acciones(fila)))
      if (evento$accion == "editar") return(editar("tarea", origen = fila$tarea_id))
      if (evento$accion == "abrir") return(showModal(.detalle_nota_ui(fila, tablas())))
      pendiente(list(id = fila$tarea_id, accion = evento$accion))
      showModal(modalDialog(title = acciones(fila)[[evento$accion]], fila$asunto,
                            footer = tagList(modalButton("Cancelar"), .boton(ns("confirmar"), "Confirmar", "check"))))
    }
    observeEvent(input$AccionTarea, actuar(input$AccionTarea))
    observeEvent(input$ejecutar, actuar(list(codigo = input$registro, accion = input$operacion)))
    observeEvent(input$confirmar, {
      evento <- pendiente()
      req(!is.null(evento))
      pendiente(NULL)
      removeModal()
      tryCatch({
        .tablero_notas_actuar(raiz, evento$id, evento$accion, usuario, grupos, identidad$local)
        refrescar()
        racafeForms::racafeNotify(session, "Registro actualizado", type = "success")
      }, error = function(e) {
        racafeForms::racafeNotify(session, "No se pudo actualizar el registro", conditionMessage(e), type = "error")
      })
    })
  })
}
