# Catálogo de navegación y dependencias ----

.vista <- function(titulo, fuente, nota = NULL, grafico = NULL, filtros = NULL) {
  list(titulo = titulo, fuente = fuente, nota = nota, grafico = grafico, filtros = filtros)
}

.grupos <- list(
  "Generalidades" = list(resumen = .vista("Resumen ejecutivo", "pedidos", grafico = "pedidos")),
  "Gestión comercial" = list(
    oportunidades = .vista("Oportunidades", "oportunidades",
      "Adquisición, cross-selling y up-selling. Las sugerencias requieren aceptación autorizada."),
    negociaciones = .vista("Negociaciones y ofertas", "ofertas"),
    actividades = .vista("Actividades", "tareas"), tareas = .vista("Agenda y pendientes", "tareas"),
    notas = .vista("Notas y seguimiento", "notas")
  ),
  "Prospección" = list(
    kanban = .vista("Kanban", "empresas"), contactos = .vista("Contactos", "contactos"),
    prospectos = .vista("Prospectos", "empresas"), leads = .vista("Leads", "empresas"),
    prospectos_clientes = .vista("Clientes", "clientes_comerciales"),
    descartados = .vista("Descartados", "descartados"),
    conversiones = .vista("Conversión", "primer_pedido_cliente",
                           "Primera creación observada de un pedido 20000.", "cohortes"),
    embudo = .vista("Embudo comercial", "empresas")
  ),
  "Cohortes de clientes" = list(
    cohortes = .vista("Clientes nuevos por período", "primer_pedido_cliente",
      "Un PerCod cuenta en el período exacto de su primer pedido observado. La cobertura histórica limita la cohorte.",
      "cohortes")
  ),
  "Muestras" = list(
    muestras = .vista("Directorio de muestras", "muestras"), solicitudes = .vista("Solicitudes", "muestras"),
    envios = .vista("Envíos de muestras", "remisiones"), evaluaciones = .vista("Evaluaciones", "evaluaciones"),
    seguimiento = .vista("Aprobadas sin pedido", "muestras"), indicadores = .vista("Indicadores de muestras", "muestras")
  ),
  "Clientes" = list(
    directorio = .vista("Directorio comercial", "clientes_comerciales"),
    individual = .vista("Consulta individual", "clientes_comerciales"),
    segmentacion = .vista("Segmentación", "tiers", "La consulta se realiza por cuenta individual.")
  ),
  "Pedidos y embarques" = list(
    pedidos = .vista("Pedidos", "pedidos", "Volumen y valor derivados de líneas elegibles.", "pedidos"),
    detalle = .vista("Detalle de pedidos", "lineas_pedido"),
    compromisos = .vista("Compromisos de embarque", "referencias_embarque",
      "Referencias candidatas de cabecera y línea. Su precedencia y cantidades comprometidas requieren validación."),
    ejecucion = .vista("Ejecución y cumplimiento", "embarques",
      "El cumplimiento se calcula a partir del embarque efectivo conciliado con el compromiso.")
  ),
  "Rentabilidad" = list(
    margenes = .vista("Márgenes", "margenes", "Margen anticipado y MNFCC se presentan en sus unidades respectivas."),
    margen_cliente = .vista("Rentabilidad por cliente", "margenes"),
    margen_pais = .vista("Rentabilidad por país", "margenes"),
    atributos = .vista("Atributos comerciales", "lineas_pedido",
      "Se conservan los códigos de origen. Categorías y productos homologados siguen pendientes.")
  ),
  "Potencial" = list(potencial = .vista("Potencial y penetración", "potencial")),
  "Mercados internacionales" = list(
    panorama = .vista("Panorama geográfico", "paises", "El mapa requiere equivalencias ISO verificadas."),
    paises = .vista("Países", "lineas_pedido", "País de destino y de origen según sus códigos fuente.", "destino"),
    evolucion = .vista("Evolución por mercado", "lineas_pedido",
      "Pedidos distintos por mes de creación y código de destino.", "evolucion")
  ),
  "Administración" = list(
    catalogos = .vista("Catálogos", "unidades"), usuarios = .vista("Usuarios y comerciales", "responsables"),
    tipologias = .vista("Tipologías", "tipologias"), catalogo_paises = .vista("Catálogo de países", "paises_syscafe"),
    productos = .vista("Homologación de productos", "productos"),
    parametros = .vista("Parámetros comerciales", "parametros"),
    tier_admin = .vista("Administración de segmentación", "tiers"),
    estado = .vista("Estado de datos", "registro"), auditoria = .vista("Auditoría", "auditoria")
  )
)
.vistas <- do.call(c, unname(.grupos))
.iconos <- c("clipboard-list", "briefcase", "filter", "chart-gantt", "flask", "users", "ship",
             "chart-line", "calculator", "globe", "gear")

.fuente <- function(dominio, clave, campos = clave, pendiente = "Pendiente de fuente") {
  list(dominio = dominio, clave = clave, campos = campos, pendiente = pendiente)
}

.fuentes <- list(
  clientes = .fuente("operacional", "PerCod", c("PerCod", "PerRazSoc", "codigo_pais")),
  clientes_comerciales = .fuente("operacional", "PerCod",
    c("PerCod", "PerRazSoc", "codigo_pais", "fecha_creacion", "tiene_pedidos")),
  pedidos = .fuente("operacional", "pedido_id",
    c("pedido_id", "PdcCod", "PerCod", "fecha_creacion", "estado_original", "sacos", "valor", "moneda", "CiaCod")),
  lineas_pedido = .fuente("operacional", c("pedido_id", "PdcLin"),
    c("pedido_id", "PdcLin", "LinNegCod", "cantidad", "UMeCod", "sacos", "valor", "moneda", "PaiCodDtn", "PaiCodOri")),
  referencias_embarque = .fuente("operacional", c("pedido_id", "referencia"),
    c("pedido_id", "referencia", "origen", "periodo", "fecha")),
  primer_pedido_cliente = .fuente("operacional", "PerCod", c("PerCod", "pedido_id", "fecha_creacion", "cohorte")),
  cohortes_clientes = .fuente("operacional", "PerCod", c("PerCod", "cohorte")),
  volumen_cliente = .fuente("operacional", "PerCod", c("PerCod", "pedidos", "sacos")),
  unidades = .fuente("operacional", "UMeCod"), calidad = .fuente("operacional", "incidencia"),
  productos = .fuente("crm", "producto_id", pendiente = "Pendiente de homologación"),
  tiers = .fuente("crm", "evaluacion_id", pendiente = "Pendiente de homologación"),
  paises = .fuente("operacional", "iso2", pendiente = "Pendiente de homologación"),
  paises_syscafe = .fuente("operacional", "PaiCod"),
  embarques = .fuente("operacional", "embarque_id"), lotes = .fuente("operacional", "lote_id"),
  margenes = .fuente("operacional", "margen_id"), presupuesto = .fuente("operacional", "meta_id"),
  potencial = .fuente("operacional", "potencial_id"), cartera = .fuente("operacional", "obligacion_id"),
  empresas = .fuente("crm", "empresa_id"), contactos = .fuente("crm", "contacto_id"),
  descartados = .fuente("crm", "evento_id"), oportunidades = .fuente("crm", "oportunidad_id"),
  ofertas = .fuente("crm", "oferta_id"), hechos_productos = .fuente("operacional", c("pedido_id", "PdcLin")),
  gestiones = .fuente("crm", "gestion_id"), tareas = .fuente("crm", "tarea_id"), notas = .fuente("crm", "nota_id"),
  muestras = .fuente("crm", "muestra_id"), remisiones = .fuente("crm", "remision_id"),
  evaluaciones = .fuente("crm", "evaluacion_id"), responsables = .fuente("crm", "actor_id"),
  tipologias = .fuente("crm", "codigo"), parametros = .fuente("crm", "version"),
  auditoria = .fuente("crm", "evento_id")
)

.ficha <- c("Identificación" = "clientes_comerciales", "Contactos" = "contactos",
            "Historial comercial" = "primer_pedido_cliente", "Pedidos" = "pedidos", "Embarques" = "embarques",
            "Productos" = "hechos_productos", "Volumen histórico" = "volumen_cliente",
            "Muestras" = "muestras", "Oportunidades" = "oportunidades", "Ofertas" = "ofertas",
            "Actividades" = "tareas",
            "Rentabilidad" = "margenes", "Presupuesto" = "presupuesto", "Potencial" = "potencial",
            "Cartera" = "cartera", "Notas" = "notas", "Tareas" = "tareas")

.dependencias_vista <- list(
  resumen = c("Clientes por cohorte" = "cohortes_clientes", "Distribución por comercial" = "responsables",
             "Embudo de conversión" = "empresas", "Compromisos" = "referencias_embarque"),
  embudo = c("Condición histórica de cliente" = "primer_pedido_cliente", "Descartes" = "descartados"),
  conversiones = c("Duración desde captación y origen" = "empresas", "Conversión por comercial" = "responsables"),
  muestras = c("Envíos" = "remisiones", "Evaluaciones" = "evaluaciones"),
  seguimiento = c("Historial de envío" = "remisiones", "Resultados" = "evaluaciones"),
  directorio = c("Responsable" = "responsables", "Tier" = "tiers",
                 "Potencial" = "potencial", "Presupuesto" = "presupuesto"),
  cohortes = c("Parámetros de retención y reactivación" = "parametros"),
  detalle = c("Trazabilidad por lote" = "lotes"),
  ejecucion = c("Compromisos conciliables" = "referencias_embarque"),
  margen_cliente = c("Identidad comercial" = "clientes"),
  margen_pais = c("Homologación ISO" = "paises"),
  atributos = c("Producto y categoría homologados" = "productos"),
  cumplimiento = c("Embarques efectivos" = "embarques"),
  potencial = c("Producto homologado" = "productos"),
  panorama = c("Clientes por país" = "clientes", "Destinos operacionales" = "lineas_pedido",
               "Margen por país" = "margenes"),
  paises = c("Equivalencias ISO" = "paises")
)

.acciones_pendientes <- list(
  kanban = c("Cambiar etapa", "Abrir detalle de empresa"),
  oportunidades = c("Editar", "Asignar responsable", "Registrar acción", "Aceptar sugerencia",
                     "Descartar sugerencia", "Cerrar oportunidad", "Consultar historial"),
  tareas = c("Asignar tarea", "Marcar finalizada"),
  envios = c("Registrar envío"), evaluaciones = c("Registrar evaluación")
)

# Colores de etapa y prioridad ----

.colores_etapa_crm <- c(CONTACTO = "#6C757D", PROSPECTO = "#E69F00", LEAD = "#0072B2",
                       CLIENTE = "#009E73", DESCARTADO = "#CC79A7")
.colores_prioridad_crm <- c(ALTA = "#E69F00", MEDIA = "#0072B2", BAJA = "#009E73")

# Acciones de captura ----

.acciones <- list(contacto = list(titulo = "Crear contacto"), gestion = list(titulo = "Nueva gestión"),
                   oportunidad = list(titulo = "Nueva oportunidad"), muestra = list(titulo = "Nueva muestra"),
                   tarea = list(titulo = "Tareas y notas"))
