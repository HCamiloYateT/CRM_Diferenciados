# CRM Diferenciados | Diagnóstico de fases 0 y 1

**Corte documental:** 23 de noviembre de 2026. **Destino:** [CRM_Diferenciados](https://github.com/HCamiloYateT/CRM_Diferenciados). Repositorio público preexistente, sin referencias antes de inicializar. Rama principal `main`, commit inicial `7dd4dbe427b666acf51e2e8c7cb4cb6fc1f6ef6c`. Rama de diagnóstico `desarrollo-inicial`.

La instrucción consolidada del 23/11/2026 fija el alcance en **`LinNegCod = 20000` original**, para café excelso diferenciado de exportación. Sustituye el alcance preliminar que señalaba 21000. El diagnóstico es documental y estático. Los nombres de tablas y campos citados son evidencias de código o del levantamiento y requieren comprobación en las fuentes operacionales antes de cerrar los contratos.

## 1. Inventario y autoridad

| Material examinado | Evidencia y aplicación |
|---|---|
| Solicitud pegada y archivo consolidado | Alcance, entregables y criterios de aceptación. Se aplican las correcciones de la versión consolidada. |
| Dinámica Comercial, v0.2, 19/10/2026 | Texto, tablas y diagramas sobre journey, muestras, ejecución y márgenes, actividad y ficha integral. El encabezado heredado no altera el alcance indicado en el cuerpo. Documento de trabajo pendiente de aprobación. |
| Glosario, v0.1, 19/10/2026 | Secciones 1–8, definiciones institucionales, criterios de interpretación y decisiones pendientes. Autoridad terminológica, con propuestas sujetas a aprobación. |
| CRMClienteFinal.zip | `ExtraccionDatos.R`, `ExtraccionCorreos.R`, `CRMClienteFinal/{global,ui,server}.R`, `modules/modules.R`, `misc/{bitacora,filtros,values}.R`, vistas `tab_contacto`, `tab_individual`, `tab_tiempos`, `tab_ventas`, `tab_bitacora`. Inspección de encabezados y tipos de los dos Excel. |
| CRMDiferenciados.zip | `APP/{global,ui,server}.R`, `misc/{functions,filters,parameters,values}.R`, `misc/ui/` y los nueve archivos de `misc/modules/`. Lectura estática de entradas, datos y persistencia. |
| CRM Nacional | `00ccd4a7a03eceb20119fccbac6ab8d5d1a1e421`. AGENTS, README, DataPrep, arranque, contratos, fuentes, acciones, prospección, gestión, consulta, cohortes y trazabilidad. [Código](https://github.com/HCamiloYateT/CRM_Nacional/tree/00ccd4a7a03eceb20119fccbac6ab8d5d1a1e421) |
| racafeModulos | `4d8786738270cf57e73bd7b26bd661cce31edb60`. DESCRIPTION, NAMESPACE y componentes en R/. [Código](https://github.com/HCamiloYateT/racafeModulos/tree/4d8786738270cf57e73bd7b26bd661cce31edb60) |
| Libreria-Racafe | `90dea8606b43fea964547f3b22bb5bad72e73fa4`. Paquetes, DESCRIPTION, APIs y guías. [Código](https://github.com/HCamiloYateT/Libreria-Racafe/tree/90dea8606b43fea964547f3b22bb5bad72e73fa4) |
| Compartido | `492901a0943893e4a464ab87d7c18700499a1814`. Versión de recursos, CSS, JS y logos. [Código](https://github.com/HCamiloYateT/Compartido/tree/492901a0943893e4a464ab87d7c18700499a1814) |
| PlantillaBs4Dash | `30f12b7d5e23855aa3e4f25621bc534145e2675e`. APP, APP_SINGLE, arranque, piezas visuales y pruebas de plantilla. [Código](https://github.com/HCamiloYateT/PlantillaBs4Dash/tree/30f12b7d5e23855aa3e4f25621bc534145e2675e) |

Se aplican primero las instrucciones consolidadas. La Dinámica aporta el proceso comercial, el Glosario define conceptos y las fuentes verificadas deben probar claves y hechos. CRM Nacional aporta patrones técnicos, el prototipo aporta ideas y Cliente Final aporta antecedentes. Los estados de evidencia son **confirmado** en el encargo o levantamiento, **pendiente** de prueba o aprobación, **propuesto** y **simulado**. Los binarios de sesiones, historiales del IDE y cachés quedaron fuera del análisis funcional.

## 2. Comparación y diagnóstico técnico

| Frente | Cliente Final, histórico | Diferenciados, prototipo | Nacional, referencia actual | Decisión |
|---|---|---|---|---|
| Arranque | Shiny con lógica y consultas históricas en global y server | bs4Dash, carga recursiva de scripts, RData opcional y varias definiciones de módulos | Contrato de arranque, comprobación de APIs y componentes organizados | Adaptar arquitectura Nacional y plantilla. Eliminar dependencia de orden implícito de carga. |
| Identidad y contactos | Persona, cliente y filtros por razón social. Correos con asociaciones por dominio y nombre | IDs, cuentas, contactos y responsables simulados | Alta, directorio y acciones por unidad comercial | Adaptar controles. Resolver PerCod con evidencia y conservar alta previa a PerCod. |
| Pipeline y conversión | Seguimiento histórico y contactos, sin modelo explícito de etapas | Kanban demostrativo, visitante y lead cualificado como columnas | Kanban, promoción, descarte, reactivación y conversión orientados a Nacional | Adaptar UI y rediseñar reglas para creación de pedido. |
| Gestión | Bitácora con lecturas y escrituras. Flujo histórico concatena texto en SQL y contiene credenciales embebidas | Actividades y alertas con datos simulados y cambios de sesión | Tareas, notas, pendientes, oportunidades, notificaciones | Adaptar contratos y controles Nacional. Excluir el flujo de escritura histórico. |
| Muestras | Sin patrón equivalente identificado | Formulario, detalle, guía, evaluación y alertas de prueba. Guardar cierra modal y notifica sin persistir | Consulta y gestión sirven como patrón técnico | Rediseñar el circuito y reutilizar presentación. |
| Pedidos y tiempos | Ventas, consulta individual, filtros de creación/embarque y pestaña de tiempos. SQL trae múltiples líneas | Dashboard y resultados de demostración | DataPrep, trazabilidad y conciliaciones operativas sofisticadas con supuestos nacionales | Rediseñar extracción y hechos 20000. Recuperar claves candidatas y pruebas de integridad. |
| Economía y planeación | Series históricas mixtas y diferencial de pedidos | KPIs de margen y pipeline simulados | Presupuesto, rentabilidad, cartera, cohortes y validación financiera | Adaptar presentación, aislar FCC y anticipado, y diseñar contrato de metas por padre. |
| Geografía | Ciudad cliente, país y puerto origen/destino desde catálogos históricos | Países simulados en cuentas y panel | Filtros y estilos transversales, orientación geográfica nacional | Rediseñar homologación internacional y adaptar gráficos. |

**Cliente Final.** `ExtraccionDatos.R` y `global.R` unen pedidos de cabecera/detalle y enriquecen con persona, cliente, unidad, pago, producto, marca y puertos. En la extracción histórica el detalle se filtra por compañía y la vista de ventas filtra línea desde UI. El selector de Convencionales y las equivalencias de 62,5 kg quedan fuera del alcance. El filtro histórico por cliente fijo, fecha inicial y la fecha límite con formato año-día-mes requieren revisión. El uso de `PdcCanTot` repetido en líneas puede inflar cantidades. Los scripts de correo son antecedentes de matching y contactos, sujetos a evaluación de privacidad, exactitud y autorización antes de cualquier uso futuro. La bitácora inspira el registro por usuario/fecha y requiere persistencia transaccional propia.

| Libro | Hoja y esquema observado | Lectura |
|---|---|---|
| `CRM_Contacto.xlsx` | `Hoja 1`: Usuario, Cliente, Fecha, Contacto, Comentarios. Textos y fecha Excel en la fila de referencia. | Bitácora histórica con información identificable. Nombre de cliente insuficiente para identificar PerCod. |
| `Pedidos.xlsx` | `Sheet1`: SucCod, LotCod, MrcNom, MCNom, NomPre, DefNom, LinNegCod, LinProCod. 61.165 filas declaradas incluyendo encabezado. Códigos enteros y nombres de texto en inspección inicial; NomPre vacío en la fila examinada. | Catálogo histórico por lote y sucursal, sin identificador de pedido en el esquema observado. Validar dominios, cobertura y unicidad antes de usarlo como referencia. |

**Prototipo.** La inicialización carga RData opcional, pero el servidor activa explícitamente datos demo en el pipeline. El usuario de respaldo y la administración habilitada por defecto invalidan el patrón de control de acceso para uso real. Cuentas redefine componentes que ya existen en el paquete corporativo. El registro de módulos contiene marcadores y definiciones repetidas. Los generadores independientes producen universos que no se pueden conciliar.

| Archivo en `APP/misc/modules/` | Diagnóstico y destino |
|---|---|
| `Dashboard.R` | Indicadores, países, distribución, actividad y volumen ilustrativos. Recuperar diseño de resumen con agregados del ETL. |
| `Cuentas.R` | Directorio, ficha, productos, tier y priorización con simulación. Adaptar ficha corporativa con identidad y cuenta padre. |
| `Contacto.R` | Roles, idioma, zona horaria, notas y agenda con fixtures. Adaptar gestión y timeline mediante Nacional. |
| `Kanban.R` | Tarjetas, arrastre, acciones y SLA de muestra. Usar tarjetas corporativas y validar transiciones en servidor. |
| `LeadScoring.R` | Pesos editables en sesión y eventos simulados. 35/60 son demostrativos. Diseñar configuración y auditoría propias. |
| `Muestras.R` | Vista por estado, ficha, guía, feedback y registro simulado. 7/10/14 días son demostrativos. Diseñar persistencia y vínculo posterior. |
| `Actividades.R` | Interacciones, filtros, seguimiento y datos reactivos por sesión. Migrar patrón visual a tareas/actividades persistentes. |
| `Alertas.R` | Nurturing, disparadores y fechas de prueba. El umbral demostrativo varía entre módulos y no tiene aprobación. |
| `modulos.R` | Marcadores para oportunidades, segmentación, canales y otros. Conservar las necesidades que cuenten con contrato de datos y fase asignada. |

**CRM Nacional.** `APP/global.R` valida versiones y firmas y carga recursos compartidos fijados. `DataPrep.R` extrae y concilia datos, conserva dimensiones originales, comprueba claves y publica el snapshot mediante escritura fuera del archivo vigente. `APP/misc/datos/01_contrato.R` sirve de referencia para el contrato de entrada. `prospeccion/kanban`, `acciones`, `gestion_comercial`, `consulta_individual`, `cohortes_clientes`, `encabezado`, `controlbar` y `compartidos/trazabilidad_pedido` resuelven UX, reactividad, permisos y navegación reutilizables con adaptación. Reemplazar los predicados de facturación, equivalencias de línea, modalidades y umbrales nacionales por reglas verificadas de Diferenciados.

### Evidencia para delimitar 20000

- El encargo consolidado, §2.1, ordena filtrar por código original 20000. `ExtraccionDatos.R`, bloque Resúmenes, proyecta `EXPPEDI1.LinNegCod` y el catálogo `NLINEANE`. La consulta histórica no demuestra por sí sola el universo de exportación. Contrastar con compañía, producto, bandera de venta nacional, estado y catálogo en sistema central. Si la cabecera mezcla líneas, medir solo sus líneas elegibles y exigir evidencia de naturaleza exportadora antes de contarla como conversión.
- `APP/misc/datos/02_fuentes_comerciales.R` de Nacional conserva una enumeración que identifica 20000 como Diferenciados, 21000 como A la Medida y 10000 como Convencionales. Distintos archivos de Nacional convierten 20000 a 21000 por sus propios fines analíticos.
- Rutas sospechosas para cualquier adaptación: `DataPrep.R` (pedidos, lotes, márgenes, segmentación y maestros); `03_evolucion_historica.R`, `04_cartera.R`, `06_integridad_operativa.R`, `07_unidades_comerciales.R`, `08_trazabilidad_documental.R` en `APP/misc/datos/`; presupuesto, cartera, evolución del negocio, validación financiera, encabezado y ajustes de oportunidades. Usar el campo original previo a clasificación. Validar antes de homologar cualquier campo alternativo de línea y conservar ambas procedencias.

## 3. Dinámica Comercial y terminología

| Sección de Dinámica Comercial | Consecuencia para el diseño |
|---|---|
| §§1–2 Alcance, equipo y cuentas | Independencia downstream. PerCod institucional, jerarquía y tipología exclusiva. Director Trader y Jefatura Comercial, atención individual y compartida. |
| §3 y figura 1 | El interés verificable permite promoción directa. El pedido crea el cliente. Cancelación inicial y ventanas quedan abiertas. |
| §4 y figura 2 | Solicitud, courier, respuesta y pedido eventual. Preparación, validación técnica, responsables y estados de remisión son controles propuestos. |
| §5 y figura 3 | Registrar negociación confirmada en central y preservar línea, lote, despacho y factura. FOB y CAD son prácticas habituales independientes. Vigente/cancelado y facturación total son prácticas reportadas a contrastar. |
| §6 | Margen comercial anticipado y FCC definitivo en granos distintos. Comparación condicionada a costos, período y TRM. |
| §7 | Presupuesto a padre, ventas fuera de presupuesto, potencial con fuentes comparables, tres hechos de participación. Imputación oficial pendiente. |
| §8 y figura 4 | Primera compra y cohortes persisten ante reactivación. La ventana y el descarte posventa requieren aprobación. |
| §§9–10 y figura 5 | Oportunidades por adquisición, reactivación, penetración, producto/programa, presupuesto, rentabilidad y muestra aceptada. La ficha integra ámbitos sin fusionar sus estados. Cartera informativa. |
| §§11–12 | Mapa de fuentes candidato y cinco escenarios de aceptación. Traducir ejemplos a fixtures, sin incorporar sus registros. |
| §§13–14 | Control de cambios con versión, dueño, impacto histórico y aprobación. V0.2 incorpora journey y un procedimiento de muestras propuesto. |

El Glosario §§5.1–5.3 separa unidad de negocio, entidad, cuenta, unidad comercial, contacto personal, tipología, estado y responsable. §§5.4–5.6 distingue oportunidad, muestra, pedido comercial, lote, despacho, factura, precio, diferencial, cobertura, Incoterm y forma de pago. §§5.7–5.9 exige explicitar unidad, moneda, grano, denominador, alcance y versión para margen, presupuesto, penetración y segmentación. §§5.10–5.11 distingue evento histórico, corte, cohorte, actividad, retención, fuente, ausencia y calidad. §6 mantiene pendientes cancelación inicial, imputación, actividad, modalidades, TRM, FCC, nomenclatura y cartera. La revisión y aprobación de la v0.1 están pendientes. Facturación válida es el referente de Nacional y creación de pedido válido es el referente de Diferenciados.

## 4. Fuentes y contratos operacionales preliminares

**Confirmación disponible:** presencia en código histórico o mención en Dinámica. **Pendiente:** catálogo real, uniones y muestras conciliadas en acceso de solo lectura.

| Hecho o dimensión | Fuente / evidencia | Grano y clave candidata | Validación necesaria |
|---|---|---|---|
| Identidad institucional | `NPERSONA`, `NCLIENTE` en la extracción histórica. `CliNit` se transpone a `PerCod` en cabecera | Una persona/empresa por identificador institucional y contexto de compañía cuando corresponda | Duplicados, vigencia, relación padre/hija oficial y cambios de identificador. |
| Cabecera | `EXPPEDID`: `PdcCod`, `PdcFecCre`, `PdcEst`, `CliNit`, `UMeCod`, `PdcVtaNal`, `PdcCanTot` en SQL histórico | `CiaCod + PdcCod` candidata | Convenciones de fecha, estados, anulaciones, universo exportador, cobertura y reglas por compañía. |
| Línea | `EXPPEDI1`: `PdcLin`, `LinNegCod`, `LinProCod`, `PdcCan`, `PdcPreDif`, `PdcFecEmb` | Cabecera + `PdcLin` candidata | Dominio 20000 original, unidad y semántica de fechas, precio y diferencial. |
| Compromiso | Cabecera histórica `PdcAnoEmb/PdcMesEmb`. Fecha en línea candidata, significado pendiente | Compromiso del pedido/línea según fuente, por mes o fecha contractual observada | Precedencia cabecera/línea, revisiones, cantidades asignadas y parcialidades. |
| Lote y enlace | `EXPLOT1` histórica por pedido, lote, sucursal. Nacional expone otros cruces de lote | Lote por clave operacional con sucursal/compañía según sistema. Puente línea-lote separado | Probar cardinalidades, asignación de peso y claves del vínculo. El histórico no prueba la clave de línea. |
| Despacho y factura | Flujo reportado. Código Nacional documenta cruces de factura de su propio universo | Evento/documento o línea de documento identificable | Fuente efectiva exportadora, anulaciones, parciales y asociación a compromiso/pedido. |
| FCC | `CRMNALLOT`, `SACOS`, `MNFCC` en Dinámica | Lote y corte/versión candidatos | Confirmar clave, cobertura, peso físico, recálculo y semántica de fecha. |
| Anticipado y costos | `PdcPreDif`, signo/puntos históricos. `EXPEXTCT/NEXTCTS` para costos candidatos | Pedido o línea según fuente efectiva, costo por concepto | Verificar si diferencial es margen anticipado, unidad, conceptos y costos incluidos. |
| Catálogos | `NLINEANE`, `NTIPPROD`, `NUNIMEDI`, `NFORPAG`, `NPAISES`, `NPUERTOS`, `NCIUEXT` | Código institucional y ámbito. Puerto por país y puerto | Linajes, unicidad, homologación ISO, conversión de masa y vigencia. |
| Potencial aduanero | SISDUAN citado, sin acceso. Histórico consulta `DWSACOSE`, `IMPORTAD`, `NEXPORTA` | Hecho comparable por comprador/producto/mercado/período | Equivalencia de fuentes, cobertura, licencia y mapeo de identidades. |
| Cartera | Dueño institucional externo a Gestión Comercial. Fuente de exportación pendiente | Obligación/documento y fecha de corte | Saldos, vencimiento, cupos, permisos y criterio de alerta. |
| Presupuesto | Archivo y esquema de origen ausentes | Padre × período × versión, con unidad y moneda explícitas | Dueño, aprobación, metas múltiples y regla temporal. |
| Gestión y muestras | Persistencia propia futura | Entidad CRM, evento, solicitud/muestra y vínculos auditados | Registro existente, roles, cardinalidades y aprobaciones de circuito. |

**Creación** es fecha exacta de incorporación, alta en cohorte, antigüedad, frecuencia y pedidos generados. **Compromiso** es fecha o período contractual de embarque, con volumen/valor asignados al período. Mostrar `YYYY-MM` si la precisión es mensual, preservando la precisión de fuente. **Despacho** y **facturación** son eventos observados distintos. Una estimación secundaria se expone con su procedencia solo si aporta valor. Pendiente y vencido requieren una fecha de corte y ejecución comparable verificable. Para compromisos mensuales, evaluar el cierre del mes una vez aprobada la regla, sin inferir atraso diario.

Medir agregado de pedido con suma de líneas elegibles en grano único. Conservar totales de cabecera como control. Preagregar movimientos y FCC antes de combinar hechos, preservar faltantes y reportar cruces sin llave o sin peso asignable. En un pedido mixto no imputar a 20000 el total de cabecera. Si un lote sirve a varias líneas, exigir asignación aprobada o reportar FCC en el grano de lote.

## 5. Modelo conceptual y journey

| Área | Entidades, relaciones y auditoría propuestas |
|---|---|
| Identidad | Empresa con ID CRM interno y vínculo a PerCod verificable y versionado, opcional durante captación. Contacto personal relacionado con una o más empresas mediante rol y vigencia. Tipología exclusiva vigente entre importador, tostador, distribuidor y cadena. |
| Jerarquía | Padre 1:N cuentas hijas/PerCod en un corte. Historia mediante vigencia desde/hasta, autor, motivo y fuente. Analizar padre e hijo con conteos distintos y reportar huérfanos. Evitar ciclos y pertenencias vigentes ambiguas. |
| Ciclo | Etapa actual derivada del historial. Evento registra etapa anterior/nueva, fecha efectiva, fecha registro, autor o proceso, motivo, evidencia y versión de regla. Capturar pedido fuente y corte en el evento/comprobación de conversión. |
| Atención | Usuario institucional, rol, ámbito y asignación con cuenta, vigencia, titularidad y motivo. Admitir atención individual o compartida. Separar propietario de cartera hoy y responsable al ocurrir el hecho. |
| Gestión | Interacción, tarea, nota, recordatorio y oportunidad, con responsable, cuenta, evidencia, estado y vínculo opcional a contacto, muestra o pedido. Distinguir origen manual y analítico. |
| Muestras | Solicitud, muestra, remisión, evaluación y puente con pedido. Conservar identificadores, cambios, responsables y fechas por evento. Permitir investigación de cardinalidad N:M entre muestras y pedidos sin duplicar métricas. |
| Economía | Vistas/snapshots para pedido, línea, lote, despacho, factura, compromiso, FCC, margen anticipado, presupuesto, potencial y cartera. Contratos con versión, origen y corte. |
| Clasificaciones | Tipología, tier sugerido/aprobado y score como entidades analíticas distintas. Modelo/versión, variables, vigencia, decisión y evidencia. |
| Geografía | País institucional e ISO 3166-1 con aliases y vigencia; ciudad; región geográfica; mercado comercial configurable. País de empresa, contacto y destino son roles separados. |

| Etapa / evento | Entrada y salida | Estado |
|---|---|---|
| CONTACTO | Interacción inicial por feria, referencia, visita, correo, redes u otro canal. Validación a prospecto o promoción directa a lead con interés verificable. | Confirmado en Dinámica §3. |
| PROSPECTO | Cumple criterios comerciales de volumen y margen. Puede referenciarse a un cliente existente o avanzar a lead. | Confirmado en §3. Cortes de elegibilidad pendientes. |
| LEAD | Intención de ambas partes y plan comercial. El primer pedido elegible dispara conversión. | Confirmado en §3. Lead cualificado como atributo propuesto. |
| CLIENTE NUEVO | Primer pedido válido creado en 20000. Nuevo del período se deriva de su cohorte. | Confirmado en encargo §2.3 y Dinámica §3. Cancelación inicial pendiente. |
| CLIENTE ACTIVO / A RECUPERAR | Recencia y frecuencia de pedidos históricos. Pedido vigente sin nuevos pedidos requiere decisión. | Estados confirmados. Política de clasificación pendiente. |
| DESCARTADO | Decisión manual con causal y evidencia. Reactivación precompra manual y reactivación poscompra por pedido nuevo conforme a política. | Descarte confirmado. Excepciones y reversión pendientes. |

La conversión usa primera creación verificable por PerCod y agrega una primera compra propia para el padre. Registrar ID, estado, procedencia y versión de corte. Reingesta idempotente y desempate por clave estable para fechas iguales. Una hija nueva puede pertenecer a un padre histórico. Exponer reactivación como hecho posterior a la primera compra. Una cuenta histórica se incorpora con su evidencia transaccional; las etapas previas se reflejan solo si fueron observadas. Muestra aceptada, facturación, alta administrativa y score son hechos independientes de la primera compra. Si el único pedido inicial se cancela, preservar evento y anulación para revisión y suspender cualquier regla automática de reversión hasta aprobación.

**Atención comercial.** El encargo identifica una única comercial activa al corte. El nombre individual se omite de la publicación pública y su identidad debe resolverse en el catálogo institucional. Jefatura consulta y gestiona toda la cartera según el levantamiento. El alcance del Director Trader y permisos de edición se validan con el área. Alta de nuevos comerciales por asignación vigente sin modificar cohortes, primera compra ni atribuciones históricas.

**Muestras.** Registrar solicitud, producto/programa, origen, perfil, puntaje SCA si aplica, certificaciones, requisitos, cantidad/unidad, contacto, destinatario, país, guía y fechas efectivas. Solicitud y envío pueden existir sin PerCod. Proponer estados preparación, preparada, enviada, entregada, pendiente de respuesta, aceptada, rechazada y cerrada sin respuesta. Conservar respuesta y sus motivos separados del estado logístico. Aceptación sin pedido origina seguimiento; falta de respuesta conserva su propio estado. Validar responsables de Calidad/Logística, remisiones, control de lote, criterios y plazos.

**Segmentación.** Construir perfiles a fecha de corte con volumen, margen, frecuencia/recencia, potencial, penetración y cobertura; evaluar disponibilidad y sesgo por antigüedad, mercado, tipo, estacionalidad y datos faltantes. Comparar un score interpretable con variables robustamente escaladas, percentiles estratificados y agrupamiento exploratorio. Validar por cohorte temporal con estabilidad de asignación, sensibilidad a pesos/cortes, tamaño mínimo, desempeño futuro y revisión de casos con Comercial. Conservar perfiles insuficientes como sin clasificación. Publicar sugerencia y explicación, con aprobación de Jefatura, versión y vigencia. El lead scoring usa eventos prerrequisito observables, ventanas/pesos configurables y prueba fuera de muestra frente a conversión posterior. Evitar fuga de información del pedido a los predictores y transiciones automáticas por score.

## 6. Presupuesto, potencial, margen y geografía

**Contrato de presupuesto propuesto.** Identificación del padre, año, mes/período si existe, versión, fuente, fecha de corte, aprobación, meta de sacos con unidad y meta económica/margen con moneda y unidad cuando exista. Una meta vigente por grano aprobado, con validación de versiones y de totales anuales/mensuales. Agregar ejecución de hijos a padre antes de cruzar metas. Distinguir sin archivo, sin meta, cero aprobado y ejecución cero. Mostrar ejecución de cuentas sin meta y cumplimiento solo para meta positiva y comparable. Separar vistas por creación y compromiso/ejecución hasta que Planeación apruebe un referente.

**Potencial y margen.** La estimación de compras del cliente versiona fuente aduanera/histórica, criterio comercial, país, producto, período, masa y entidad homologada. Share of wallet por pedidos, despachos y facturas usa denominadores del mismo hecho y universo. Potencial no capturado es compras potenciales menos volumen comparable de Racafé, con revisión de discrepancias negativas. Margen final ponderado es suma de `MNFCC × kg elegibles` dividida por kg con FCC. Mostrar cobertura y pedidos sin FCC. El equivalente US¢/lb resulta de `(COP/kg ÷ TRM) × 0,45359237 × 100` con TRM aprobada, versión y costos compatibles. Mantener separado el margen anticipado a su grano verificado.

**Mercados.** Homologar códigos y nombres al ISO y reservar desconocidos con alerta de calidad. Una región comercial puede tener vigencia y configuración propias. Aplicar país de destino a compromisos o despacho según el hecho, país de empresa a cartera comercial y país de contacto a gestión personal. Usar geometrías y catálogos cacheados, con mapas mundiales, rankings y evolución. Probar integridad de totales ante joins y visibilidad de destinos desconocidos.

| Indicador | Base, calendario y control |
|---|---|
| Conversiones y clientes nuevos | Primer pedido válido por PerCod y padre, mes creación. Tasa de leads convertidos sobre leads elegibles con horizonte y cobertura declarados. |
| Pedidos, volumen y valor creados | Cabecera única y líneas 20000 elegibles por fecha de creación. Conciliar masa y moneda. |
| Calendario de embarque | Cantidad, valor, pedidos y clientes comprometidos por período contractual. Cumplido, pendiente, vencido y parciales solo tras validar ejecución y regla. |
| Despacho y facturación | Volumen y valor de cada hecho por fecha efectiva. Trazabilidad de documento y anulaciones. |
| Retención, actividad y reactivaciones | Seguimiento de cohortes por mes desde primera compra. Recencia, frecuencia, intervalo y reactivación histórica. Denominador y ventana explícitos. |
| Muestras | Solicitudes, remisiones, entregas, feedback y aceptadas sin pedido. Aceptadas/respondidas, respondidas/elegibles y aceptadas con pedido/aceptadas elegibles al horizonte. Plazos con fechas completas. |
| Economía y planeación | Anticipado, FCC, cobertura, ejecución, meta, desviación y cumplimiento en la base temporal seleccionada; penetración por hecho. |
| Mercado y cartera | Padres/PerCod, conversión, volumen, compromisos, márgenes y potencial por rol geográfico. Saldos, vencimientos y alertas según fuente/permiso. |

## 7. Reutilización y arquitectura

| Componente | Ruta / API / versión observada | Decisión y compatibilidad |
|---|---|---|
| Tablas y exportación | racafeModulos 0.6.7, `R/TablaVancouver.R`: `TablaVancouverUI`, `TablaVancouverServer`, `TablaVancouverDatos` | Directa para tablas. Configurar moneda, unidades, totales y exportación. Probar anchos y estilo de versión fijada. |
| KPI, tarjetas y modales | racafeModulos 0.6.7, `R/cajaValor*.R`, `R/CajaModal.R`, `R/Tarjeta.R` | Directa, por contratos UI/server. Eliminar implementaciones duplicadas del prototipo en el diseño futuro. |
| Tableros, tareas y timeline | racafeModulos 0.6.7, `R/Tableros.R`, `R/Timeline.R`, `R/MenuAcciones.R` | Directa para presentación. Verificar firmas `arrastrable`, `acciones_fn`, `agrupar`. Adaptar acciones y validación al modelo propio. |
| Encabezado y selectores | racafeModulos 0.6.7, `R/MenuHeader.R`, `R/ListaDesplegableAgregable.R`; racafeShiny 1.5.0 `ListaDesplegable` | Adaptar menús, permisos y catálogos. |
| Formularios y notas | racafeForms 0.8.0, `racafeForm`, `racafeValidateForm`, `racafeFormMessage`, `racafeFormsDependencies` | Directa para UI; reglas y validación de servidor son propias. |
| Controles y formato | racafeShiny 1.5.0, formatos, confirmación y controles corporativos | Directa; probar convención numérica, cancelación y confirmaciones. |
| Gráficos | racafeGraph 1.0.1, `ColoresRacafe`, `tema_racafe_plotly`, `tema_racafe_ggplot` | Directa para temas, adaptar mapas y semántica de gráficos. |
| Infraestructura | racafeCore 1.0.1 para utilidades; racafeBD 1.0.0 `ConectarBD`, `ConsultaSistema`, `CargarDatos` | Adaptar conexiones y lectura al sistema autorizado. Confirmar capacidad transaccional por operación y usar DBI parametrizado cuando corresponda. Instalar dependencias antes del arranque. |
| Integraciones opcionales | racafeDrive 1.0.0, racafeMail 0.2.0, racafeForecast 1.0.1 | Condicionadas a fuente documental, notificaciones autorizadas o necesidad analítica concreta. Pronóstico fuera del eje de compromisos. |
| Marca y comportamiento | Compartido 1.2.4 observado, `Styles/style.css`, tokens/controles/formularios/tableros, `Scripts/{forms,interactions,controlbar,crm-kanban,crm-prospeccion}.js` y logos | Directa para marca genérica, adaptar selectores CRM. Carga única, fijada por commit. Nacional referencia otro commit de assets, así que corresponde prueba de compatibilidad. |
| Estructura Shiny | PlantillaBs4Dash, `APP/{global,ui,server}.R`, `APP/misc/ui/` y guías | Adaptar cascarón APP con sidebar y controlbar y excluir demos. |
| Flujos de aplicación | CRM Nacional, `APP/misc/modules/{prospeccion,acciones,gestion_comercial,consulta_individual,cohortes_clientes,compartidos}` | Adaptar filtros, tarjetas, timeline, tareas y ficha. Rediseñar conversión, identidad histórica, series, cartera y presupuesto para 20000. |
| Consultas y vistas históricas | Cliente Final y prototipo | Consultar como antecedente. Rediseñar extracción, muestras, scoring, biografías de cliente, alertas y persistencia. |

**Arquitectura propuesta.** `DataPrep.R` orquesta lectura centralizada, validación de dominios y granularidad, snapshots y métricas de cobertura. Conservar dato original y metadatos de fuente/corte. Publicar de forma atómica con contrato de esquema, verificación de claves y último corte válido explícito ante fallos. Cargar dimensiones y hechos una vez y aplicar filtros reactivos compartidos. El servidor de cada módulo usa `moduleServer()`, entradas namespaced y servicio de persistencia con transacciones, permiso de servidor y control de concurrencia. Aislar el esquema CRM y sus escrituras auditadas del flujo de ingestión. Versionar SQL y contratos en fases posteriores. Mantener índices por claves de unión/filtros tras medir consultas, y caché por corte. Configurar secretos por entorno y preparar dependencias en instalación, verificadas en arranque. Objetivo tras fase técnica: iniciar con `shiny::runApp("APP")` y fuentes opcionales ausentes representadas explícitamente.

**Navegación preliminar.** Inicio, Prospección, Gestión, Muestras, Clientes, Pedidos y embarques, Rentabilidad, Presupuesto y potencial, Mercados internacionales y Administración. Pedidos separa creación, compromiso mensual y ejecución observada. Clientes incluye ficha por PerCod con enlace a padre: identificación y tipología, responsable histórico, contactos y journey, pedidos y compromisos, muestras, calidad, margen, presupuesto, potencial, cartera, actividades y oportunidades cuando disponibles. Administración concentra catálogos, usuarios, alcance, reglas, auditoría y calidad. Activar cada vista a medida que disponga de contrato y datos.

**Presentación.** Filtros compactos, KPI, gráficos/detalle y nota metodológica. Controles apropiados al ancho, multiselección con Todos/Ninguno cuando aplique, overlay de carga y estados distinguibles. Gráficos principales de al menos 500 px, tema corporativo, leyendas legibles y hover para detalle. Vancouver con borde rojo bajo el encabezado, totales visibles y scroll horizontal si aplica. Contar sin decimales; declarar moneda y unidades; mostrar año-mes para compromisos mensuales.

## 8. Matriz de decisiones y riesgos

Estado de todas las filas: pendiente al 23/11/2026. Evidencias D = Dinámica Comercial v0.2, G = Glosario v0.1, C = encargo consolidado, H = código histórico y N = Nacional.

| Prioridad | Decisión, evidencia | Responsable propuesto | Alternativas / tratamiento preliminar e impacto |
|---|---|---|---|
| Alta | Código original, universo exportador y validez de pedido (C §2, H, N) | TI / Comercial / Analítica | Contrastar catálogo y bandera de venta nacional, vigencia y compañías. Conservar código original y poner en cuarentena excepciones. Afecta todo el universo. |
| Alta | Cancelación primer pedido (D §3, G §6) | Dirección / Jefatura | Conservar conversión histórica con marca o recomputar elegibilidad con regla aprobada. Preservar evento y documentar efecto sobre cohortes. |
| Alta | Actividad, pedido vigente antiguo y descarte posventa (D §§3,8, G §6) | Comercial / Analítica | Ventana común, por tipología o individual; regla explícita para pedido abierto en el tiempo. Estudio de recencia antes de parametrizar. |
| Alta | Grano anticipado, modalidades y costos (D §§5–6, G §6) | Comercial / Finanzas | Verificar cabecera vs línea y distinguir diferencial, precio, cobertura y costos especiales. Afecta comparabilidad y rentabilidad. |
| Alta | TRM (D §6, G §6) | Finanzas / Analítica | Elegir fecha de transacción, de ejecución o tasa institucional aprobada. Mantener unidades originales hasta acuerdo. |
| Alta | Puentes pedido–línea–lote–FCC e identidad padre/hija (D §§2,5,11, H) | TI / Datos | Probar claves, vigencias, cobertura y asignación de peso. Cuantificar huérfanos y cambios de padre sin reescrituras silenciosas. |
| Alta | Compromiso, despacho efectivo y parciales (C §5, D §5, H) | Logística / TI | Validar fecha o mes acordado, fuente efectiva y reparto de volúmenes. Medir cumplimiento solo con ambos hechos comparables. |
| Alta | Fuente de presupuesto e imputación (D §7, C §7) | Planeación / Comercial | Contratar formato por padre. Evaluar creación, compromiso y ejecución con cortes separados. Sin meta válida, cumplimiento no disponible. |
| Alta | Responsables de muestras (D §4) | Comercial / Calidad / Logística | Confirmar quién valida, prepara, remite y registra respuesta y cuál es el vínculo oficial. Determina formularios y permisos. |
| Media | SLA y cierre sin respuesta (D §4, prototipo) | Comercial / Calidad | Evaluar distribución de tiempos y regla manual o fecha límite aprobada. Mantener rechazo independiente. |
| Media | Tier y scoring (D §8, C §8) | Analítica / Jefatura | Comparar score, percentiles y agrupamiento exploratorio. Validación temporal, parametrización y aprobación previa. |
| Media | Mercados, historial y oportunidades sin pedido (D §§8–9, C §§9,11) | Comercial / Analítica | Definir mercados y vigencia, pertenencia histórica, cierre de oportunidades y sugerencias revisables. |
| Media | SISDUAN y cartera (D §§5,7,11) | Datos / Tesorería | Acordar cobertura, homologación, permisos, alertas y condición reportada de bloqueo. CRM informativo, con trazabilidad. |

## 9. Plan y validación

| Fase | Entregable y condición de avance |
|---|---|
| 0 | Git independiente y ramas publicadas en el destino existente. |
| 1 | Diagnóstico versionado, inventarios, decisiones y diseño preliminar. |
| 2 | ER, diccionario y contratos campo a campo; DDL y migraciones versionadas tras contrastar llaves y permisos. |
| 3 | Base Shiny, versiones, ETL reproducible y contratos de ejecución/ausencia. |
| 4 | Prospección, identidad, historial y conversión mediante validación de pedidos. |
| 5 | Gestión, oportunidades, tareas y muestras con roles y circuito acordados. |
| 6 | Pedidos, compromisos, ejecución y reconciliación; depende de claves y datos de embarque. |
| 7 | Metodologías de clasificación y scoring, calibración y aprobación. |
| 8 | Margen, presupuesto, cartera y potencial, condicionados a fuentes y reglas financieras. |
| 9 | Mercado internacional, ficha, cohortes, retención y resumen ejecutivo. |
| 10 | Validación de integración, conciliación, permisos, persistencia, rendimiento y experiencia. |

**Ejecutado:** verificación del repositorio y referencias remotas, lectura estática de código/contratos, identificación de las normalizaciones heredadas, verificación de versiones/exportaciones, extracción de texto y tablas de Word, inspección de diagramas y verificación de encabezados y tipos iniciales de ambos Excel. La Fase 1 no consulta bases corporativas, ejecuta scripts históricos, instala dependencias ni prueba una aplicación funcional. Las comprobaciones sobre estados, esquemas, cardinalidades, volúmenes, moneda y permisos de cada fuente siguen pendientes en el ambiente correspondiente.

| Prueba por ejecutar | Resultado esperado |
|---|---|
| Dominio 20000 y exclusiones | Solo líneas 20000 originales elegibles, con prueba negativa para 10000, 21000 y 20000 reclasificado o de universo no elegible. |
| Modelo temporal | Fecha exacta de creación, mes contractual con precisión preservada, ejecución y factura separadas. Detección de parciales y cambios de compromiso cuando exista evidencia. |
| Conversión y ciclo | Cuenta previa a pedido, muestra aceptada, alta histórica y primer pedido producen estados esperados. Reingesta idempotente y cancelación inicial queda trazada. Reactivar conserva primera compra. |
| Granos y reconciliación | Multilínea/multilote, facturas parciales y muestras múltiples preservan sacos, valor y FCC. Conciliar por línea, período y país con fuentes y reportar excepciones. |
| Padres, responsables y geo | Conteos diferenciados, vigencias consistentes, cambios de responsable no cambian cohortes, filtros geográficos sin multiplicación. |
| Presupuesto, economía y potencial | Meta padre una vez, ejecución sin meta visible, fuente ausente no impide arrancar, NA y cero distintos, FCC ponderado, cobertura visible y denominadores alineados. |
| Oportunidades, scoring, alertas | Reglas versionadas, datos demo fuera del runtime, estado comercial independiente y administración restringida. |
| Seguridad, persistencia y rendimiento | Permisos y escrituras transaccionales, concurrencia, auditoría, ETL sin sobrescritura manual, esquema validado, actualización atómica, arranque sin instalación/migración y consultas medidas. |
| Experiencia | Filtros, notas, tabla Vancouver, exportación, overlay, accesibilidad, mapa y consistencia entre ficha y agregados. |
