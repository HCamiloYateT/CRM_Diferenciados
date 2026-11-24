# Geografía, cohortes y pedido ----

.bandera_pais <- function(iso) {
  if (length(iso) != 1L || is.na(iso) || !grepl("^[A-Z]{2}$", iso)) return(NULL)
  tags$img(src = paste0("https://flagsapi.com/", iso, "/flat/32.png"), alt = iso,
           width = 24, height = 24, loading = "lazy", referrerpolicy = "no-referrer")
}

.destinos_operacion <- function(lineas, pedidos) {
  if (is.null(lineas) || is.null(pedidos) || !"ISO2Dtn" %in% names(lineas)) return(NULL)
  d <- lineas[!is.na(lineas$ISO2Dtn) & grepl("^[A-Z]{2}$", lineas$ISO2Dtn), , drop = FALSE]
  filas <- lapply(split(d, d$ISO2Dtn), function(x) {
    ids <- unique(x$pedido_id)
    data.frame(iso2 = x$ISO2Dtn[1L], pedidos = length(ids),
               clientes = length(unique(pedidos$PerCod[match(ids, pedidos$pedido_id)])),
               sacos = .dp_suma(x$sacos))
  })
  do.call(rbind, filas)
}

.cohortes_operacion <- function(primeros, pedidos) {
  if (is.null(primeros) || is.null(pedidos) || !nrow(primeros)) return(NULL)
  d <- pedidos[which(!is.na(pedidos$fecha_creacion) & pedidos$PerCod %in% primeros$PerCod), , drop = FALSE]
  pos <- match(d$PerCod, primeros$PerCod)
  d$cohorte <- primeros$cohorte[pos]
  d$mes <- format(d$fecha_creacion, "%Y-%m")
  d <- d[which(d$mes >= d$cohorte), , drop = FALSE]
  denominador <- table(primeros$cohorte)
  filas <- lapply(split(d, paste(d$cohorte, d$mes)), function(x) {
    clientes <- length(unique(x$PerCod))
    data.frame(Cohorte = x$cohorte[1L], Mes = x$mes[1L], Clientes = clientes,
               RetencionPct = 100 * clientes / denominador[[x$cohorte[1L]]], Pedidos = nrow(x),
               Frecuencia = nrow(x) / clientes, Sacos = .dp_suma(x$sacos))
  })
  do.call(rbind, filas)
}
