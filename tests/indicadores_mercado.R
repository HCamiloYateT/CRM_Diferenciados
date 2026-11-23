# Fuentes sintéticas. No se consulta CRMINDICADORES ni sistemas corporativos en la prueba.
source("APP/global.R")

campos <- .IND_CLAVES_CANON
stopifnot(length(campos) == 16L, !anyDuplicated(campos), identical(names(.IND_UNIDADES), campos))
consultas <- character()
consulta <- function(bd, query) {
  consultas <<- c(consultas, query)
  stopifnot(!grepl("LinNegCod|CRMNAL", query))
  if (grepl("EXPMECAF", query)) return(data.frame(TRM = 4000))
  if (grepl("INFFUT1", query)) return(data.frame(NY = 200))
  if (grepl("HAVING", query)) return(data.frame(HTFec = "2026-11-20", PreTo = 130000000, KilTo = 100))
  if (bd == "cafesys") {
    return(data.frame(Producto = c("CONSUMO", "CONSUMO", "MOLIDOS", "SOLUBLE", "RIPIO", "ROBUSTA"),
                      TotKls = c(1, 3, 1, 1, 1, 1), ResVlrNeg = c(5000, 27000, 1, 2, 3, 4)))
  }
  data.frame(HTPreRip = 45, HTPrePas = 55, HTPreCon = 65, Diferencial = -0.1)
}
vigentes <- function() {
  .ObtenerIndicadoresActuales(fnc = function() list(precio = 1400000, bolsa = 215),
                             sistema = function() .GetSystemData(consulta))
}
datos <- vigentes()
canon <- .NormalizarIndicadoresActuales(datos)
stopifnot(length(consultas) == 5L, identical(names(canon), campos), canon$TRM == 4000,
          canon$COMConsumo == 8000, canon$UGCRacafe == (1300000 / 125) / (70 / 96.89),
          canon$UGCFNC == (1400000 / 125) / (70 / 96.89))

fechas <- as.Date(c("2026-11-20", "2026-11-21", "2026-11-23"))
historico <- rbind(canon, canon)
historico$FechaActualizacion <- fechas[1:2]
historico$TRM <- c(0, 3950)
historico$Diferencial <- c(-0.2, -0.1)
lecturas <- 0L
leer <- function() {
  lecturas <<- lecturas + 1L
  historico
}
historica <- .ConsultarComparacionIndicadores(fechas[1L], fechas[2L], fechas[3L], leer,
                                              vigentes = function() stop("No se espera consulta vigente"))
stopifnot(lecturas == 1L, historica$fuente == "CRMINDICADORES", nrow(historica$datos) == 16L,
          is.na(historica$datos$Variacion_Porcentual[1L]),
          historica$datos$Variacion_Porcentual[4L] == 0.5)
actual <- .ConsultarComparacionIndicadores(fechas[2L], fechas[3L], fechas[3L], leer, vigentes)
stopifnot(lecturas == 2L, length(consultas) == 10L, actual$datos$Diferencia_Absoluta[1L] == 50)
tendencia <- .PrepararTendenciaIndicadores(datos, .CargarAnteriorIndicadores(fechas[2L], historico))
stopifnot(tendencia$tendencia[1L] == "sube", grepl("COP/USD", tendencia$Valor[1L], fixed = TRUE))
tabla <- .FormatoComparacionIndicadores(actual$datos)$datos
for (i in seq_along(campos)) stopifnot(endsWith(tabla[["Valor final"]][i], .IND_UNIDADES[[campos[i]]]))
sin_fecha <- try(.ConsultarComparacionIndicadores(fechas[1L] - 1, fechas[2L], fechas[3L], leer), silent = TRUE)
stopifnot(inherits(sin_fecha, "try-error"))

pagina <- htmltools::renderTags(source("APP/ui.R", local = TRUE)$value)$html
stopifnot(grepl('id="shiny-tab-comparacion_indicadores"', pagina, fixed = TRUE),
          grepl("Generalidades", pagina, fixed = TRUE),
          grepl("Indicadores", pagina, fixed = TRUE))
