# Disponibilidad y filtros de lectura ----

.leer_dominios <- function(raiz) {
  leer <- function(dominio) {
    tryCatch(LeerPublicacionRDS(raiz, dominio), error = function(e) {
      message("CRM Diferenciados / ", dominio, ": ", conditionMessage(e))
      e
    })
  }
  list(operacional = leer("operacional"), crm = leer("crm"))
}

.estado_fuente <- function(publicaciones, nombre) {
  fuente <- .fuentes[[nombre]]
  pub <- publicaciones[[fuente$dominio]]
  if (inherits(pub, "esquema_publicacion")) return("Esquema incompatible")
  if (inherits(pub, "error")) return("Error de lectura")
  tabla <- pub$tablas[[nombre]]
  if (is.null(tabla)) return(fuente$pendiente)
  if (!is.data.frame(tabla) || !all(fuente$campos %in% names(tabla))) return("Esquema incompatible")
  if (anyNA(tabla[fuente$clave]) || anyDuplicated(tabla[fuente$clave])) return("Esquema incompatible")
  if (nombre == "lineas_pedido" && (anyNA(tabla$LinNegCod) || any(tabla$LinNegCod != "20000"))) {
    return("Esquema incompatible")
  }
  if (nombre %in% c("pedidos", "primer_pedido_cliente", "clientes_comerciales") &&
      !inherits(tabla$fecha_creacion, "Date")) return("Esquema incompatible")
  if (!nrow(tabla)) {
    return(if (fuente$pendiente == "Pendiente de homologación") fuente$pendiente else "Sin información")
  }
  "Disponible"
}

.tabla_fuente <- function(publicaciones, nombre) {
  estado <- .estado_fuente(publicaciones, nombre)
  if (!estado %in% c("Disponible", "Sin información")) return(NULL)
  publicaciones[[.fuentes[[nombre]]$dominio]]$tablas[[nombre]]
}

.registro_fuentes <- function(publicaciones) {
  filas <- lapply(names(.fuentes), function(nombre) {
    fuente <- .fuentes[[nombre]]
    pub <- publicaciones[[fuente$dominio]]
    corte <- pub$manifiesto$procedencia$corte
    version <- pub$manifiesto$versiones[nombre]
    calidad <- pub$tablas$calidad
    consumidores <- names(Filter(function(vista) vista$fuente == nombre, .vistas))
    dependencias <- names(Filter(function(x) nombre %in% x, .dependencias_vista))
    consumidores <- unique(c(consumidores, dependencias))
    if (nombre %in% .ficha) consumidores <- unique(c(consumidores, "individual"))
    data.frame(Fuente = nombre, Dominio = fuente$dominio, Archivo = paste0(nombre, ".rds"),
               Esquema = paste(fuente$campos, collapse = ", "), Clave = paste(fuente$clave, collapse = " + "),
               Estado = .estado_fuente(publicaciones, nombre),
               Corte = if (length(corte)) .formato_fecha_crm(as.Date(corte)) else "—",
               Versión = if (length(version) && !is.na(version)) unname(version) else "—",
               Calidad = if (is.data.frame(calidad)) paste(nrow(calidad), "incidencias del corte") else "—",
               Vistas = paste(consumidores, collapse = ", "))
  })
  do.call(rbind, filas)
}

.filtrar_operacional <- function(publicaciones, filtros) {
  clientes <- .tabla_fuente(publicaciones, "clientes")
  pedidos <- .tabla_fuente(publicaciones, "pedidos")
  lineas <- .tabla_fuente(publicaciones, "lineas_pedido")
  catalogo <- .asesores_catalogo(publicaciones$crm$tablas, incluir_retirados = TRUE)
  asesor <- .asesor_nombre(.asesor_provisional, catalogo)
  if (!is.null(clientes)) {
    clientes$Asesor <- rep(asesor, nrow(clientes))
    if (length(filtros$pais)) clientes <- clientes[clientes$codigo_pais %in% filtros$pais, , drop = FALSE]
    if (length(filtros$cliente)) clientes <- clientes[clientes$PerCod %in% filtros$cliente, , drop = FALSE]
    if (nzchar(filtros$percod)) clientes <- clientes[clientes$PerCod == filtros$percod, , drop = FALSE]
  }
  seleccion_pedido <- length(filtros$periodo) == 2L || length(filtros$estado) ||
    length(filtros$destino) || length(filtros$creador) || length(filtros$embarque)
  if (!is.null(pedidos)) {
    if (!is.null(clientes)) pedidos <- pedidos[pedidos$PerCod %in% clientes$PerCod, , drop = FALSE]
    if (length(filtros$periodo) == 2L) {
      fechas <- pedidos$fecha_creacion
      pedidos <- pedidos[which(fechas >= filtros$periodo[1L] & fechas <= filtros$periodo[2L]), , drop = FALSE]
    }
    if (length(filtros$embarque)) pedidos <- pedidos[pedidos$periodo_cabecera %in% filtros$embarque, , drop = FALSE]
    if ("PdcUsu" %in% names(pedidos)) {
      clave <- .asesor_usuario(pedidos$PdcUsu)
      if (length(filtros$creador)) pedidos <- pedidos[clave %in% filtros$creador, , drop = FALSE]
      pedidos$UsuarioCreacion <- .asesor_nombre(pedidos$PdcUsu, catalogo)
      pedidos$PdcUsu <- NULL
    }
    pedidos$Asesor <- rep(asesor, nrow(pedidos))
    if (length(filtros$estado)) pedidos <- pedidos[pedidos$estado_original %in% filtros$estado, , drop = FALSE]
    if (length(filtros$destino)) {
      destino <- lineas$pedido_id[lineas$PaiCodDtn %in% filtros$destino]
      pedidos <- pedidos[pedidos$pedido_id %in% destino, , drop = FALSE]
    }
  }
  if (!is.null(lineas) && !is.null(pedidos)) lineas <- lineas[lineas$pedido_id %in% pedidos$pedido_id, , drop = FALSE]
  if (length(filtros$destino) && !is.null(lineas)) {
    lineas <- lineas[lineas$PaiCodDtn %in% filtros$destino, , drop = FALSE]
  }
  if (length(filtros$destino) && !is.null(pedidos) && !is.null(lineas)) {
    pedidos$sacos <- .dp_agregar(lineas$pedido_id, lineas$sacos, pedidos$pedido_id)
    for (i in seq_len(nrow(pedidos))) {
      detalle <- lineas[lineas$pedido_id == pedidos$pedido_id[i], , drop = FALSE]
      monedas <- unique(detalle$moneda)
      comparable <- length(monedas) == 1L && !is.na(monedas)
      pedidos$valor[i] <- if (comparable) .dp_suma(detalle$valor) else NA_real_
      pedidos$moneda[i] <- if (comparable) monedas else NA_character_
    }
  }
  if (seleccion_pedido && !is.null(clientes) && !is.null(pedidos)) {
    clientes <- clientes[clientes$PerCod %in% pedidos$PerCod, , drop = FALSE]
  }
  list(clientes = clientes, pedidos = pedidos, lineas_pedido = lineas)
}

.vista_datos <- function(publicaciones, operativo, nombre, filtros) {
  if (nombre == "registro") return(.registro_fuentes(publicaciones))
  if (nombre %in% names(operativo)) return(operativo[[nombre]])
  if (nombre == "margenes") return(.margenes_filtrados(publicaciones, operativo$pedidos))
  datos <- .tabla_fuente(publicaciones, nombre)
  if (is.null(datos)) return(NULL)
  if ("PerCod" %in% names(datos) && !is.null(operativo$clientes)) {
    datos <- datos[datos$PerCod %in% operativo$clientes$PerCod, , drop = FALSE]
  }
  if (nombre == "referencias_embarque" && !is.null(operativo$pedidos)) {
    datos <- datos[datos$pedido_id %in% operativo$pedidos$pedido_id, , drop = FALSE]
  }
  if (nombre %in% c("primer_pedido_cliente", "cohortes_clientes") && length(filtros$periodo) == 2L) {
    if ("fecha_creacion" %in% names(datos)) {
      fechas <- datos$fecha_creacion
      datos <- datos[which(fechas >= filtros$periodo[1L] & fechas <= filtros$periodo[2L]), , drop = FALSE]
    } else {
      meses <- format(filtros$periodo, "%Y-%m")
      datos <- datos[which(datos$cohorte >= meses[1L] & datos$cohorte <= meses[2L]), , drop = FALSE]
    }
  }
  datos
}

.margenes_filtrados <- function(publicaciones, pedidos) {
  d <- .tabla_fuente(publicaciones, "margenes")
  if (is.null(d) || is.null(pedidos)) return(NULL)
  d[d$pedido_id %in% pedidos$pedido_id, , drop = FALSE]
}
