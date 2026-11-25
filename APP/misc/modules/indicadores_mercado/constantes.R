# Indicadores ----
.IND_CLAVES_CANON <- c(
  "TRM", "PrecioNY", "PrecioCarga", "Diferencial", "UGCRacafe",
  "PrecioBolsa", "PrecioFNC", "UGCFNC",
  "CALConsumo", "COMConsumo", "CALPasilla", "COMMolidos",
  "COMSoluble", "CALRipio", "COMRipio", "COMRobusta"
)

.IND_NOMBRES_CORTOS <- c(
  "TRM" = "TRM (Hoja de trabajo)",
  "PrecioNY" = "Precio NYC (HT)",
  "PrecioCarga" = "Precio Carga (Promedio de últimas entradas del día)",
  "Diferencial" = "Diferencial de Compra (HT)",
  "UGCRacafe" = "Costo UGQ Racafé",
  "PrecioBolsa" = "Precio Bolsa (FNC)",
  "PrecioFNC" = "Precio Carga (FNC)",
  "UGCFNC" = "Costo UGQ (FNC)",
  "CALConsumo" = "Precio Consumo (Calculadora)",
  "COMConsumo" = "Precio Consumo (Compras)",
  "CALPasilla" = "Precio Pasilla (Calculadora)",
  "COMMolidos" = "Precio Molidos (Compras)",
  "COMSoluble" = "Precio Soluble (Compras)",
  "CALRipio" = "Precio Ripio (Calculadora)",
  "COMRipio" = "Precio Ripio (Compras)",
  "COMRobusta" = "Precio Robusta (Compras)"
)[.IND_CLAVES_CANON]

# Rótulos para la comparación y el menú. Los nombres cortos se usan para empatar las fuentes.
.IND_NOMBRES_VISIBLES <- c(
  TRM = "Tasa representativa del mercado (hoja de trabajo)",
  PrecioNY = "Cotización del café en Nueva York (hoja de trabajo)",
  PrecioCarga = "Precio por carga · promedio de las últimas entradas del día",
  Diferencial = "Diferencial de compra de café (hoja de trabajo)",
  UGCRacafe = "Costo estimado por kilo de UGQ (Racafé)",
  PrecioBolsa = "Cotización del café en bolsa publicada por la FNC",
  PrecioFNC = "Precio de referencia por carga publicado por la FNC",
  UGCFNC = "Costo estimado por kilo de UGQ a partir del precio FNC",
  CALConsumo = "Precio de café consumo (calculadora)",
  COMConsumo = "Precio de compra de café consumo",
  CALPasilla = "Precio de café pasilla (calculadora)",
  COMMolidos = "Precio de compra de café para molidos",
  COMSoluble = "Precio de compra de café para soluble",
  CALRipio = "Precio de café ripio (calculadora)",
  COMRipio = "Precio de compra de café ripio",
  COMRobusta = "Precio de compra de café robusta"
)[.IND_CLAVES_CANON]

.IND_COLORES_TENDENCIA <- c(
  sube = "#1F7A55",
  baja = "#9F0712",
  igual = "#374151"
)

.IND_FLECHAS_TENDENCIA <- c(
  sube = "▲",
  baja = "▼",
  igual = "▶"
)

.IND_UNIDADES <- stats::setNames(rep("COP/kg", length(.IND_CLAVES_CANON)), .IND_CLAVES_CANON)
.IND_UNIDADES[c("TRM", "PrecioNY", "PrecioCarga", "Diferencial", "PrecioBolsa", "PrecioFNC")] <-
  c("COP/USD", "¢USD/lb", "COP/carga", "USD/lb", "¢USD/lb", "COP/carga")
.IND_DECIMALES <- stats::setNames(rep(2L, length(.IND_CLAVES_CANON)), .IND_CLAVES_CANON)
.IND_DECIMALES[c("PrecioCarga", "PrecioFNC", "Diferencial")] <- c(0L, 0L, 4L)
