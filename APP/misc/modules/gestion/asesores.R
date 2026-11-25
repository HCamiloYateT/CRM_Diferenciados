# Catálogo de asesores ----

.asesor_usuario <- function(x) toupper(trimws(as.character(x)))

.asesor_correo <- function(usuario) if (length(usuario)) paste0(tolower(usuario), "@racafe.com") else character()

# Asignación provisional de cartera ----
.asesor_provisional <- "LMGRANADOS"

.asesores_catalogo <- function(tablas, incluir_retirados = FALSE) {
  guardados <- .gestion_filas(tablas, "asesores")
  nombres <- c(LMGRANADOS = "Lina Granados", JFCASTRO = "Juan Felipe Castro", PCAICEDOP = "Pablo Caicedo",
               TRESTREPO = "Tomás Restrepo", WMUNOZS = "Wilmar Muñoz", IRADICS = "Ildikó Radics",
               HVERGARA = "Hernán Vergara")
  claves <- setdiff(names(nombres), guardados$usuario)
  base <- data.frame(usuario = claves, nombre = unname(nombres[claves]),
                     correo = .asesor_correo(claves), vigente = rep(TRUE, length(claves)))
  orden <- .gestion_filas(tablas, "asesores_orden")
  eliminado <- orden$usuario[orden$eliminado]
  d <- rbind(guardados, base)
  if (incluir_retirados) return(d)
  d[!d$usuario %in% eliminado, , drop = FALSE]
}

.asesor_nombre <- function(usuario, catalogo) {
  nombre <- catalogo$nombre[match(.asesor_usuario(usuario), catalogo$usuario)]
  nombre[is.na(nombre)] <- usuario[is.na(nombre)]
  nombre
}

.asesor_asignado <- function(tablas, creador, seleccionado = NULL) {
  if (is.null(seleccionado)) return(creador)
  elegido <- .asesor_usuario(.gestion_texto(seleccionado))
  catalogo <- .asesores_catalogo(tablas)
  encontrado <- match(elegido, catalogo$usuario)
  if (!is.na(encontrado) && isTRUE(catalogo$vigente[encontrado])) return(elegido)
  orden <- .gestion_filas(tablas, "asesores_orden")
  if (is.na(encontrado) && identical(elegido, .asesor_usuario(creador)) &&
      !elegido %in% orden$usuario[orden$eliminado]) return(elegido)
  stop("Seleccione un responsable vigente.", call. = FALSE)
}

.asesores_guardar <- function(raiz, clave, nombre, correo, vigente, identidad, origen) {
  if (!.crm_es_admin(identidad$usuario, identidad$grupos)) {
    stop("Solo un administrador puede editar el catálogo.", call. = FALSE)
  }
  clave <- .asesor_usuario(.gestion_campo(clave, "usuario"))
  nombre <- .gestion_campo(nombre, "nombre del asesor")
  correo <- tolower(.gestion_campo(correo, "correo"))
  if (!grepl("^[A-Z0-9._-]+$", clave) || !grepl("^[a-z0-9._%+-]+@racafe[.]com$", correo)) {
    stop("Indique un usuario válido y un correo del dominio @racafe.com.", call. = FALSE)
  }
  .gestion_actualizar(raiz, function(tablas) {
    a <- tablas$asesores
    actual <- a[a$usuario == clave, , drop = FALSE]
    rownames(actual) <- NULL
    rownames(origen) <- NULL
    if (!identical(actual, origen)) stop("El asesor cambió en otra sesión. Vuelva a seleccionarlo.", call. = FALSE)
    fila <- data.frame(usuario = clave, nombre = nombre, correo = correo, vigente = isTRUE(vigente))
    tablas$asesores <- .gestion_upsert(a, fila, "usuario")
    i <- match(clave, tablas$asesores_orden$usuario)
    if (!is.na(i)) tablas$asesores_orden$eliminado[i] <- FALSE
    tablas
  }, identidad$usuario, "Actualizar catálogo de asesores", clave)
}

.asesores_filas <- function(pedidos, catalogo, orden = .gestion_filas(NULL, "asesores_orden")) {
  creadores <- .asesor_usuario(pedidos$PdcUsu)
  claves <- unique(c(creadores, catalogo$usuario))
  claves <- claves[!is.na(claves) & grepl("^[A-Z0-9._-]+$", claves)]
  claves <- setdiff(claves, orden$usuario[orden$eliminado])
  i <- match(claves, catalogo$usuario)
  fechas <- pedidos$fecha_creacion %||% rep(as.Date(NA), length(creadores))
  recientes <- order(fechas, decreasing = TRUE, na.last = NA)
  ultima <- fechas[recientes[match(claves, creadores[recientes])]]
  filas <- data.frame(usuario = claves, nombre = .asesor_nombre(claves, catalogo),
                      correo = ifelse(is.na(i), .asesor_correo(claves), catalogo$correo[i]),
                      vigente = !is.na(i) & catalogo$vigente[i], ultima_creacion = ultima)
  manual <- orden$orden[match(filas$usuario, orden$usuario)]
  manual[is.na(manual)] <- Inf
  filas[order(manual, -as.numeric(filas$ultima_creacion), filas$usuario, na.last = TRUE), , drop = FALSE]
}

.AsesoresUI <- function(id) {
  ns <- NS(id)
  div(class = "row",
      div(class = "col-12 col-xl-9 mb-3",
          div(class = "font-weight-bold mb-1", "Personas"),
          racafeModulos::TablaVancouverUI(ns("asesores"), descargable = FALSE),
          uiOutput(ns("alta"))))
}

.AsesoresServer <- function(id, publicaciones, raiz, refrescar) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    identidad <- .crm_identidad_notas(session)
    autorizado <- .crm_es_admin(identidad$usuario, identidad$grupos)
    origen <- reactiveVal(NULL)
    eliminar <- reactiveVal(NULL)
    output$alta <- renderUI({
      req(autorizado)
      tagList(div(class = "small text-muted mt-1", "Catálogo independiente, sin jerarquía"),
              racafeForms::racafeFormFooter(
                racafeForms::racafeButton("anadir", "Nueva opción", icon = "plus",
                                          variant = "secondary", size = "xs", ns = ns),
                align = "right"
              ))
    })
    catalogo <- reactive(.asesores_catalogo(publicaciones()$crm$tablas))
    filas <- reactive({
      req(autorizado)
      d <- .asesores_filas(.tabla_fuente(publicaciones(), "pedidos"), catalogo(),
                           .gestion_filas(publicaciones()$crm$tablas, "asesores_orden"))
      d$numero <- seq_len(nrow(d))
      d$acciones <- d$usuario
      d
    })
    boton_accion <- function(clave, accion, titulo, icono) {
      evento <- jsonlite::toJSON(ns("accion"), auto_unbox = TRUE)
      valor <- jsonlite::toJSON(list(usuario = clave, tipo = accion), auto_unbox = TRUE)
      variante <- if (accion == "eliminar") "danger" else "secondary"
      tagAppendAttributes(
        racafeForms::racafeButton(paste("fila", accion, clave, sep = "_"), NULL, icon = icono,
                                  variant = variante, size = "xxs", title = titulo, ns = ns),
        `aria-label` = titulo, onclick = sprintf("Shiny.setInputValue(%s,%s,{priority:'event'});", evento, valor),
        style = paste0("height:19px;min-height:19px;width:19px;min-width:19px;padding:0;",
                       "font-size:10px;line-height:17px;margin:0;border-radius:50%;")
      )
    }
    tabla <- racafeModulos::TablaVancouverServer(
      "asesores", data = filas, id_col = "usuario", modo_seleccion = "fila", descargable = FALSE, sortable = FALSE,
      tabla_fn = function(datos, ...) {
        tabla <- racafeShiny::gt_minimal_style(gt::gt(datos))
        tabla <- gt::cols_move_to_start(tabla, columns = "numero")
        gt::tab_options(tabla, table.width = gt::pct(100), table.font.size = gt::px(11),
                        data_row.padding = gt::px(1), column_labels.padding = gt::px(2),
                        table_body.hlines.style = "none", row.striping.include_table_body = FALSE)
      },
      col_specs = list(
        numero = list(name = "#", align = "center", minWidth = 28,
                      format = list(digits = 0, separators = FALSE)),
        usuario = list(name = "Usuario"),
        nombre = list(name = "Nombre de la persona"),
        correo = list(name = "Correo"),
        vigente = list(name = "Activo", cell = function(valor) {
          racafeForms::racafeStatusBadge(if (valor) "Sí" else "No",
                                        variant = if (valor) "success" else "neutral")
        }),
        ultima_creacion = list(name = "Última creación de pedido", cell = function(fecha) {
          if (is.na(fecha)) "—" else .formato_fecha_crm(fecha)
        }),
        acciones = list(name = "Acciones", cell = function(clave, indice) {
          tags$span(class = "d-inline-flex align-items-center", style = "gap:3px;white-space:nowrap;",
                    if (indice > 1L) boton_accion(clave, "subir", "Subir", "arrow-up"),
                    if (indice < nrow(filas())) boton_accion(clave, "bajar", "Bajar", "arrow-down"),
                    if (!identical(clave, .asesor_provisional)) {
                      boton_accion(clave, "eliminar", "Eliminar", "trash")
                    })
        })
      )
    )
    editar <- function(fila = NULL) {
      anterior <- .gestion_filas(publicaciones()$crm$tablas, "asesores")
      clave <- if (is.null(fila)) NULL else fila$usuario
      origen(list(clave = clave, fila = anterior[anterior$usuario %in% clave, , drop = FALSE]))
      showModal(racafeForms::racafeModalDetail(
        title = if (is.null(clave)) "Añadir asesor" else paste("Editar asesor", clave), size = "subventana3",
        racafeForms::racafeForm(
          if (is.null(clave)) racafeForms::racafeInputText("usuario_nuevo", "Usuario", required = TRUE, ns = ns),
          racafeForms::racafeInputText("nombre", "Nombre de la persona", value = fila$nombre %||% "", ns = ns),
          racafeForms::racafeInputText("correo", "Correo", value = fila$correo %||% "", ns = ns),
          racafeForms::racafeInputSwitch("vigente", "Activo", value = fila$vigente %||% TRUE, ns = ns),
          variant = "plain"
        ),
        footer = racafeForms::racafeModalFooter(
          modalButton("Cancelar"),
          racafeForms::racafeButton("guardar", "Guardar", icon = "check", ns = ns)
        )
      ))
    }
    observeEvent(tabla$seleccion(), {
      req(autorizado)
      editar(tabla$seleccion()$fila)
      tabla$limpiar()
    })
    observeEvent(input$anadir, {
      req(autorizado)
      editar()
    })
    observeEvent(input$usuario_nuevo, {
      req(autorizado, !is.null(origen()), is.null(origen()$clave))
      if (!nzchar(input$correo %||% "")) updateTextInput(session, "correo",
                                                        value = .asesor_correo(.asesor_usuario(input$usuario_nuevo)))
    })
    observeEvent(input$guardar, {
      registro <- origen()
      req(autorizado, !is.null(registro))
      tryCatch({
        clave <- registro$clave %||% .asesor_usuario(input$usuario_nuevo)
        if (is.null(registro$clave) && clave %in% filas()$usuario) stop("El usuario ya está en el catálogo.")
        correo <- if (nzchar(input$correo %||% "")) input$correo else .asesor_correo(clave)
        .asesores_guardar(raiz, clave, input$nombre, correo, input$vigente, identidad, registro$fila)
        origen(NULL)
        removeModal()
        refrescar()
        racafeForms::racafeNotify(session, "Asesor guardado", type = "success")
      }, error = function(e) racafeForms::racafeNotify(session, "No se pudo guardar", conditionMessage(e), type = "error"))
    })
    aplicar_orden <- function(clave, paso = NULL) {
      req(autorizado, clave %in% filas()$usuario)
      pedidos <- .tabla_fuente(publicaciones(), "pedidos")
      tryCatch({
        .gestion_actualizar(raiz, function(tablas) {
          orden <- tablas$asesores_orden
          claves <- .asesores_filas(pedidos, .asesores_catalogo(tablas), orden)$usuario
          i <- match(clave, claves)
          if (is.na(i)) stop("El asesor ya no está disponible.")
          if (is.null(paso)) {
            if (identical(clave, .asesor_provisional)) stop("Lina Granados está asignada a la cartera provisional.")
            fila <- data.frame(usuario = clave, orden = NA_integer_, eliminado = TRUE)
            tablas$asesores_orden <- .gestion_upsert(orden, fila, "usuario")
          } else if ((i + paso) %in% seq_along(claves)) {
            claves[c(i, i + paso)] <- claves[c(i + paso, i)]
            filas_orden <- data.frame(usuario = claves, orden = seq_along(claves), eliminado = FALSE)
            tablas$asesores_orden <- rbind(orden[orden$eliminado, , drop = FALSE], filas_orden)
          }
          tablas
        }, identidad$usuario, if (is.null(paso)) "Retirar asesor del catálogo" else "Reordenar asesores", clave)
        removeModal()
        refrescar()
      }, error = function(e) racafeForms::racafeNotify(session, "No se pudo guardar", conditionMessage(e), type = "error"))
    }
    observeEvent(input$accion, {
      clave <- input$accion$usuario
      req(autorizado, is.character(clave), length(clave) == 1L, clave %in% filas()$usuario)
      switch(input$accion$tipo,
             subir = aplicar_orden(clave, -1L),
             bajar = aplicar_orden(clave, 1L),
             eliminar = {
               eliminar(clave)
               racafeForms::racafeConfirm(session, "confirmar_eliminar", "¿Eliminar del catálogo?", clave, ns = ns)
             })
    })
    observeEvent(input$confirmar_eliminar, {
      clave <- eliminar()
      eliminar(NULL)
      if (isTRUE(input$confirmar_eliminar)) aplicar_orden(clave)
    })
  })
}
