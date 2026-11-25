# Catálogo de asesores ----

.asesor_usuario <- function(x) toupper(trimws(as.character(x)))

.asesor_correo <- function(usuario) paste0(tolower(usuario), "@racafe.com")

.asesores_catalogo <- function(tablas) .gestion_filas(tablas, "asesores")

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
  if (is.na(encontrado) && identical(elegido, .asesor_usuario(creador))) return(elegido)
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
    tablas
  }, identidad$usuario, "Actualizar catálogo de asesores", clave)
}

.AsesoresUI <- function(id) {
  racafeForms::racafePanel(title = "Asesores", reactable::reactableOutput(NS(id)("asesores")))
}

.AsesoresServer <- function(id, publicaciones, raiz, refrescar) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    identidad <- .crm_identidad_notas(session)
    autorizado <- .crm_es_admin(identidad$usuario, identidad$grupos)
    origen <- reactiveVal(NULL)
    catalogo <- reactive(.asesores_catalogo(publicaciones()$crm$tablas))
    filas <- reactive({
      pedidos <- .tabla_fuente(publicaciones(), "pedidos")
      d <- catalogo()
      claves <- .asesor_usuario(c(pedidos$PdcUsu, d$usuario))
      claves <- sort(unique(claves[!is.na(claves) & grepl("^[A-Z0-9._-]+$", claves)]))
      i <- match(claves, d$usuario)
      data.frame(usuario = claves, nombre = .asesor_nombre(claves, d),
                 correo = ifelse(is.na(i), .asesor_correo(claves), d$correo[i]),
                 vigente = is.na(i) | d$vigente[i])
    })
    output$asesores <- reactable::renderReactable({
      req(autorizado)
      reactable::reactable(
        filas(), searchable = TRUE, compact = TRUE, highlight = TRUE, defaultPageSize = 15,
        selection = "single", onClick = "select",
        columns = list(.selection = reactable::colDef(show = FALSE),
                       usuario = reactable::colDef(name = "Usuario"),
                       nombre = reactable::colDef(name = "Nombre de la persona"),
                       correo = reactable::colDef(name = "Correo"),
                       vigente = reactable::colDef(name = "Activo", cell = function(value) {
                         racafeForms::racafeStatusBadge(if (value) "Sí" else "No",
                                                       variant = if (value) "success" else "neutral")
                       }))
      )
    })
    observeEvent(reactable::getReactableState("asesores", "selected"), {
      req(autorizado)
      seleccion <- reactable::getReactableState("asesores", "selected")
      d <- filas()
      req(length(seleccion) == 1L, seleccion %in% seq_len(nrow(d)))
      fila <- d[seleccion, , drop = FALSE]
      anterior <- catalogo()
      origen(list(clave = fila$usuario, fila = anterior[anterior$usuario == fila$usuario, , drop = FALSE]))
      reactable::updateReactable("asesores", selected = NA)
      showModal(racafeForms::racafeModalDetail(
        title = paste("Editar asesor", fila$usuario), size = "subventana3",
        racafeForms::racafeForm(
          racafeForms::racafeInputText("nombre", "Nombre de la persona", value = fila$nombre, ns = ns),
          racafeForms::racafeInputText("correo", "Correo", value = fila$correo, ns = ns),
          racafeForms::racafeInputSwitch("vigente", "Activo", value = fila$vigente, ns = ns),
          variant = "plain"
        ),
        footer = racafeForms::racafeModalFooter(
          modalButton("Cancelar"),
          racafeForms::racafeButton("guardar", "Guardar", icon = "check", ns = ns)
        )
      ))
    })
    observeEvent(input$guardar, {
      registro <- origen()
      req(autorizado, !is.null(registro))
      tryCatch({
        .asesores_guardar(raiz, registro$clave, input$nombre, input$correo, input$vigente, identidad, registro$fila)
        origen(NULL)
        removeModal()
        refrescar()
        racafeForms::racafeNotify(session, "Asesor guardado", type = "success")
      }, error = function(e) racafeForms::racafeNotify(session, "No se pudo guardar", conditionMessage(e), type = "error"))
    })
  })
}
