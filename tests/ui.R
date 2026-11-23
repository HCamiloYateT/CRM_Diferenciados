# Contratos visuales y filtros con un fixture sintético ----

source("APP/global.R")
pagina <- htmltools::renderTags(source("APP/ui.R", local = TRUE)$value)$html
for (id in names(.vistas)) stopifnot(grepl(paste0('id="shiny-tab-', id, '"'), pagina, fixed = TRUE))
for (id in c("actualizar", "header_contacto", "control_contacto", "crm-carga", "estado_corte")) {
  stopifnot(grepl(paste0('id="', id, '"'), pagina, fixed = TRUE))
}
identificadores <- regmatches(pagina, gregexpr('(?<![\\w-])id="[^"]+"', pagina, perl = TRUE))[[1L]]
stopifnot(!anyDuplicated(identificadores), !anyDuplicated(names(.vistas)))
stopifnot(all(vapply(.vistas, function(x) x$fuente %in% c(names(.fuentes), "registro"), logical(1))))

publicaciones <- .leer_dominios(tempdir())
stopifnot(.estado_fuente(publicaciones, "pedidos") == "Pendiente de fuente",
          .estado_fuente(publicaciones, "jerarquia_cuentas") == "Pendiente de homologación")
ficha <- htmltools::renderTags(.formulario_accion("muestra"))$html
stopifnot(grepl("disabled", ficha), grepl("Courier", ficha), grepl("Resultado de evaluación", ficha))
clientes <- data.frame(PerCod = c("01", "02"), codigo_pais = c("CO", "DE"), PerRazSoc = c("Sintético A", "Sintético B"))
pedidos <- data.frame(pedido_id = c("A", "B"), CiaCod = "01", PdcCod = c("1", "2"), PerCod = c("01", "02"),
                      fecha_creacion = as.Date(c("2025-01-01", "2025-02-01")), estado_original = "Vigente",
                      sacos = c(4, 3), valor = c(8, 6), moneda = "USD")
lineas <- data.frame(pedido_id = c("A", "A", "B"), PdcLin = c("1", "2", "1"), LinNegCod = "20000",
                     cantidad = c(1, 3, 3), UMeCod = "S", sacos = c(1, 3, 3), valor = c(2, 6, 6), moneda = "USD",
                     PaiCodDtn = c("CA", "US", "US"), PaiCodOri = "CO")
publicaciones$operacional <- list(tablas = list(clientes = clientes, pedidos = pedidos, lineas_pedido = lineas),
                                  manifiesto = list(versiones = c(pedidos = "v1")))
estado <- .registro_fuentes(publicaciones)
stopifnot(estado$Estado[estado$Fuente == "pedidos"] == "Disponible")
seleccion <- .filtrar_operacional(publicaciones, list(destino = "CA", percod = ""))
stopifnot(nrow(seleccion$pedidos) == 1L, seleccion$pedidos$sacos == 1, seleccion$pedidos$valor == 2,
          nrow(seleccion$lineas_pedido) == 1L, seleccion$clientes$PerCod == "01")
stopifnot(.datos_grafico("destino", lineas, seleccion)$total[2L] == 2L)
publicaciones$operacional$tablas$lineas_pedido$LinNegCod[1L] <- "10000"
stopifnot(.estado_fuente(publicaciones, "lineas_pedido") == "Esquema incompatible")
