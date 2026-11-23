# Diccionario relacional histórico

Derivado de los scripts históricos 001 y 002. Para persistencia vigente, consultar [RDS](persistencia_rds.md). Los tipos son internos. La tabla conserva las declaraciones de campo,
incluidas claves, valores por defecto y colaciones. Un campo sin NOT NULL o PK admite nulo.
PK y UNIQUE compuestas, FK, índices explícitos y CHECK aparecen debajo de cada entidad.
InnoDB crea índices auxiliares para FK cuando se necesitan. El [modelo lógico](modelo.md) define el significado,
grano, origen y validaciones que requieren el servicio o los archivos operacionales.

## `pais`

| Campo | Tipo y restricciones de columna |
|---|---|
| `iso2` | `CHAR(2) CHARACTER SET ascii COLLATE ascii_bin PRIMARY KEY` |
| `nombre` | `VARCHAR(120) NOT NULL` |

- `CONSTRAINT ck_iso2 CHECK (REGEXP_LIKE(iso2, '^[A-Z]{2}$', 'c'))`

## `pais_equivalencia`

| Campo | Tipo y restricciones de columna |
|---|---|
| `fuente` | `VARCHAR(48) NOT NULL` |
| `codigo_original` | `VARCHAR(80) NOT NULL` |
| `desde` | `DATE NOT NULL` |
| `hasta` | `DATE` |
| `iso2` | `CHAR(2) CHARACTER SET ascii COLLATE ascii_bin NOT NULL` |
| `evidencia` | `VARCHAR(500) NOT NULL` |

- `PRIMARY KEY (fuente, codigo_original, desde)`
- `FOREIGN KEY (iso2) REFERENCES pais(iso2)`
- `CHECK (hasta IS NULL OR hasta > desde)`

## `tipologia`

| Campo | Tipo y restricciones de columna |
|---|---|
| `codigo` | `VARCHAR(24) PRIMARY KEY` |
| `nombre` | `VARCHAR(100) NOT NULL UNIQUE` |

## `actor`

| Campo | Tipo y restricciones de columna |
|---|---|
| `actor_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `identidad` | `VARCHAR(128) NOT NULL UNIQUE` |
| `clase` | `ENUM('USUARIO','PROCESO') NOT NULL` |
| `habilitado` | `BOOLEAN NOT NULL DEFAULT TRUE` |

## `rol_actor`

| Campo | Tipo y restricciones de columna |
|---|---|
| `actor_id` | `BIGINT UNSIGNED NOT NULL` |
| `rol` | `VARCHAR(40) NOT NULL` |

- `PRIMARY KEY (actor_id, rol)`
- `FOREIGN KEY (actor_id) REFERENCES actor(actor_id)`

## `empresa`

| Campo | Tipo y restricciones de columna |
|---|---|
| `empresa_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `nombre` | `VARCHAR(200) NOT NULL` |
| `pais_iso2` | `CHAR(2) CHARACTER SET ascii COLLATE ascii_bin` |
| `tipologia_codigo` | `VARCHAR(24)` |
| `creado_por` | `BIGINT UNSIGNED NOT NULL` |
| `creado_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |

- `FOREIGN KEY (pais_iso2) REFERENCES pais(iso2)`
- `FOREIGN KEY (tipologia_codigo) REFERENCES tipologia(codigo)`
- `FOREIGN KEY (creado_por) REFERENCES actor(actor_id)`

## `persona_operativa`

| Campo | Tipo y restricciones de columna |
|---|---|
| `percod` | `VARCHAR(64) COLLATE utf8mb4_bin PRIMARY KEY` |
| `fuente` | `VARCHAR(48) NOT NULL` |
| `corte` | `DATETIME(6) NOT NULL` |
| `pais_empresa_iso2` | `CHAR(2) CHARACTER SET ascii COLLATE ascii_bin` |

- `FOREIGN KEY (pais_empresa_iso2) REFERENCES pais(iso2)`

## `empresa_percod`

| Campo | Tipo y restricciones de columna |
|---|---|
| `vinculo_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `empresa_id` | `BIGINT UNSIGNED NOT NULL` |
| `percod` | `VARCHAR(64) COLLATE utf8mb4_bin NOT NULL` |
| `desde` | `DATETIME(6) NOT NULL` |
| `hasta` | `DATETIME(6)` |
| `registrado_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `validado_por` | `BIGINT UNSIGNED NOT NULL` |
| `evidencia` | `VARCHAR(500) NOT NULL` |
| `cerrado_por` | `BIGINT UNSIGNED` |
| `motivo_cierre` | `VARCHAR(500)` |
| `percod_vigente` | `VARCHAR(64) COLLATE utf8mb4_bin GENERATED ALWAYS AS (CASE WHEN hasta IS NULL THEN percod END) STORED` |

- `UNIQUE (percod_vigente)`
- `UNIQUE (empresa_id, percod, desde)`
- `KEY ix_percod_vigencia (percod, desde, hasta)`
- `FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id)`
- `FOREIGN KEY (percod) REFERENCES persona_operativa(percod)`
- `FOREIGN KEY (validado_por) REFERENCES actor(actor_id)`
- `FOREIGN KEY (cerrado_por) REFERENCES actor(actor_id)`
- `CHECK (hasta IS NULL OR hasta > desde)`
- `CHECK ((hasta IS NULL AND cerrado_por IS NULL AND motivo_cierre IS NULL) OR (hasta IS NOT NULL AND cerrado_por IS NOT NULL AND motivo_cierre IS NOT NULL))`

## `cuenta_padre`

| Campo | Tipo y restricciones de columna |
|---|---|
| `padre_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `fuente` | `VARCHAR(48) NOT NULL` |
| `codigo_fuente` | `VARCHAR(100) NOT NULL` |
| `nombre` | `VARCHAR(200) NOT NULL` |

- `UNIQUE (fuente, codigo_fuente)`

## `cuenta_padre_miembro`

| Campo | Tipo y restricciones de columna |
|---|---|
| `padre_id` | `BIGINT UNSIGNED NOT NULL` |
| `percod` | `VARCHAR(64) COLLATE utf8mb4_bin NOT NULL` |
| `desde` | `DATE NOT NULL` |
| `hasta` | `DATE` |
| `validado_por` | `BIGINT UNSIGNED NOT NULL` |
| `registrado_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `evidencia` | `VARCHAR(500) NOT NULL` |
| `miembro_vigente` | `VARCHAR(64) COLLATE utf8mb4_bin GENERATED ALWAYS AS (CASE WHEN hasta IS NULL THEN percod END) STORED` |

- `PRIMARY KEY (percod, desde)`
- `UNIQUE (miembro_vigente)`
- `KEY ix_padre_vigencia (padre_id, desde, hasta)`
- `FOREIGN KEY (padre_id) REFERENCES cuenta_padre(padre_id)`
- `FOREIGN KEY (percod) REFERENCES persona_operativa(percod)`
- `FOREIGN KEY (validado_por) REFERENCES actor(actor_id)`
- `CHECK (hasta IS NULL OR hasta > desde)`

## `contacto`

| Campo | Tipo y restricciones de columna |
|---|---|
| `contacto_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `nombre` | `VARCHAR(200) NOT NULL` |
| `correo` | `VARCHAR(254)` |
| `pais_iso2` | `CHAR(2) CHARACTER SET ascii COLLATE ascii_bin` |

- `FOREIGN KEY (pais_iso2) REFERENCES pais(iso2)`

## `empresa_contacto`

| Campo | Tipo y restricciones de columna |
|---|---|
| `empresa_id` | `BIGINT UNSIGNED NOT NULL` |
| `contacto_id` | `BIGINT UNSIGNED NOT NULL` |
| `funcion` | `VARCHAR(120)` |

- `PRIMARY KEY (empresa_id, contacto_id)`
- `FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id)`
- `FOREIGN KEY (contacto_id) REFERENCES contacto(contacto_id)`

## `asignacion_comercial`

| Campo | Tipo y restricciones de columna |
|---|---|
| `asignacion_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `empresa_id` | `BIGINT UNSIGNED NOT NULL` |
| `comercial_id` | `BIGINT UNSIGNED NOT NULL` |
| `desde` | `DATETIME(6) NOT NULL` |
| `hasta` | `DATETIME(6)` |
| `asignado_por` | `BIGINT UNSIGNED NOT NULL` |
| `motivo` | `VARCHAR(500) NOT NULL` |

- `UNIQUE (empresa_id, comercial_id, desde)`
- `KEY ix_asignacion_comercial (comercial_id, desde, hasta)`
- `FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id)`
- `FOREIGN KEY (comercial_id) REFERENCES actor(actor_id)`
- `FOREIGN KEY (asignado_por) REFERENCES actor(actor_id)`
- `CHECK (hasta IS NULL OR hasta > desde)`

## `adquisicion_evento`

| Campo | Tipo y restricciones de columna |
|---|---|
| `evento_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `empresa_id` | `BIGINT UNSIGNED NOT NULL` |
| `etapa_anterior` | `ENUM('CONTACTO','PROSPECTO','LEAD')` |
| `etapa_nueva` | `ENUM('CONTACTO','PROSPECTO','LEAD') NOT NULL` |
| `efectiva_en` | `DATETIME(6) NOT NULL` |
| `registrada_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `actor_id` | `BIGINT UNSIGNED NOT NULL` |
| `motivo` | `VARCHAR(500) NOT NULL` |
| `evidencia` | `VARCHAR(500)` |

- `KEY ix_adquisicion_empresa_fecha (empresa_id, efectiva_en, evento_id)`
- `FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id)`
- `FOREIGN KEY (actor_id) REFERENCES actor(actor_id)`
- `CHECK (etapa_anterior IS NULL OR etapa_anterior <> etapa_nueva)`

## `pedido_ref`

| Campo | Tipo y restricciones de columna |
|---|---|
| `pedido_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `fuente` | `VARCHAR(48) NOT NULL` |
| `clave_fuente` | `VARCHAR(200) NOT NULL` |
| `percod` | `VARCHAR(64) COLLATE utf8mb4_bin NOT NULL` |
| `creado_en` | `DATETIME(6) NOT NULL` |
| `precision_creacion` | `ENUM('DIA','INSTANTE') NOT NULL` |
| `corte` | `DATETIME(6) NOT NULL` |

- `UNIQUE (fuente, clave_fuente)`
- `UNIQUE (pedido_id, percod)`
- `KEY ix_pedido_cliente_fecha (percod, creado_en, pedido_id)`
- `FOREIGN KEY (percod) REFERENCES persona_operativa(percod)`

## `pedido_estado_observado`

| Campo | Tipo y restricciones de columna |
|---|---|
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `corte` | `DATETIME(6) NOT NULL` |
| `estado_original` | `VARCHAR(64) NOT NULL` |
| `fecha_efectiva` | `DATE` |

- `PRIMARY KEY (pedido_id, corte)`
- `FOREIGN KEY (pedido_id) REFERENCES pedido_ref(pedido_id)`

## `conversion`

| Campo | Tipo y restricciones de columna |
|---|---|
| `percod` | `VARCHAR(64) COLLATE utf8mb4_bin PRIMARY KEY` |
| `primer_pedido_id` | `BIGINT UNSIGNED NOT NULL UNIQUE` |
| `registrado_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `corte_evidencia` | `DATETIME(6) NOT NULL` |
| `version_regla` | `VARCHAR(40) NOT NULL` |

- `FOREIGN KEY (primer_pedido_id, percod) REFERENCES pedido_ref(pedido_id, percod)`

## `conversion_correccion`

| Campo | Tipo y restricciones de columna |
|---|---|
| `correccion_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `percod` | `VARCHAR(64) COLLATE utf8mb4_bin NOT NULL` |
| `pedido_anterior_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_nuevo_id` | `BIGINT UNSIGNED NOT NULL` |
| `corte_evidencia` | `DATETIME(6) NOT NULL` |
| `version_regla` | `VARCHAR(40) NOT NULL` |
| `aprobado_por` | `BIGINT UNSIGNED NOT NULL` |
| `registrada_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `evidencia` | `VARCHAR(500) NOT NULL` |

- `KEY ix_conversion_correccion (percod, registrada_en)`
- `FOREIGN KEY (percod) REFERENCES conversion(percod)`
- `FOREIGN KEY (pedido_anterior_id, percod) REFERENCES pedido_ref(pedido_id, percod)`
- `FOREIGN KEY (pedido_nuevo_id, percod) REFERENCES pedido_ref(pedido_id, percod)`
- `FOREIGN KEY (aprobado_por) REFERENCES actor(actor_id)`
- `CHECK (pedido_anterior_id <> pedido_nuevo_id)`

## `regla_actividad`

| Campo | Tipo y restricciones de columna |
|---|---|
| `version_regla` | `VARCHAR(40) PRIMARY KEY` |
| `parametros` | `JSON NOT NULL` |
| `aprobada_por` | `BIGINT UNSIGNED` |
| `aprobada_en` | `DATETIME(6)` |

- `FOREIGN KEY (aprobada_por) REFERENCES actor(actor_id)`
- `CHECK ((aprobada_por IS NULL) = (aprobada_en IS NULL))`

## `actividad_evaluada`

| Campo | Tipo y restricciones de columna |
|---|---|
| `percod` | `VARCHAR(64) COLLATE utf8mb4_bin NOT NULL` |
| `corte` | `DATETIME(6) NOT NULL` |
| `version_regla` | `VARCHAR(40) NOT NULL` |
| `estado` | `VARCHAR(40) NOT NULL` |

- `PRIMARY KEY (percod, corte, version_regla)`
- `FOREIGN KEY (percod) REFERENCES persona_operativa(percod)`
- `FOREIGN KEY (version_regla) REFERENCES regla_actividad(version_regla)`

## `muestra_solicitud`

| Campo | Tipo y restricciones de columna |
|---|---|
| `solicitud_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `empresa_id` | `BIGINT UNSIGNED NOT NULL` |
| `contacto_id` | `BIGINT UNSIGNED` |
| `solicitada_en` | `DATETIME(6) NOT NULL` |
| `requisitos` | `TEXT` |
| `registrada_por` | `BIGINT UNSIGNED NOT NULL` |

- `FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id)`
- `FOREIGN KEY (empresa_id, contacto_id) REFERENCES empresa_contacto(empresa_id, contacto_id)`
- `FOREIGN KEY (registrada_por) REFERENCES actor(actor_id)`

## `muestra`

| Campo | Tipo y restricciones de columna |
|---|---|
| `muestra_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `solicitud_id` | `BIGINT UNSIGNED NOT NULL` |
| `referencia_producto` | `VARCHAR(120)` |
| `atributos` | `JSON` |

- `FOREIGN KEY (solicitud_id) REFERENCES muestra_solicitud(solicitud_id)`

## `muestra_remision`

| Campo | Tipo y restricciones de columna |
|---|---|
| `remision_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `transportadora` | `VARCHAR(120)` |
| `guia` | `VARCHAR(120)` |
| `enviada_en` | `DATETIME(6)` |
| `entregada_en` | `DATETIME(6)` |
| `destino_iso2` | `CHAR(2) CHARACTER SET ascii COLLATE ascii_bin` |

- `FOREIGN KEY (destino_iso2) REFERENCES pais(iso2)`
- `CHECK (enviada_en IS NULL OR entregada_en IS NULL OR entregada_en >= enviada_en)`

## `muestra_remision_detalle`

| Campo | Tipo y restricciones de columna |
|---|---|
| `remision_id` | `BIGINT UNSIGNED NOT NULL` |
| `muestra_id` | `BIGINT UNSIGNED NOT NULL` |
| `cantidad` | `DECIMAL(18,6)` |
| `unidad` | `VARCHAR(24)` |

- `PRIMARY KEY (remision_id, muestra_id)`
- `FOREIGN KEY (remision_id) REFERENCES muestra_remision(remision_id)`
- `FOREIGN KEY (muestra_id) REFERENCES muestra(muestra_id)`
- `CHECK ((cantidad IS NULL) = (unidad IS NULL))`
- `CHECK (cantidad IS NULL OR cantidad > 0)`

## `muestra_evaluacion`

| Campo | Tipo y restricciones de columna |
|---|---|
| `evaluacion_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `muestra_id` | `BIGINT UNSIGNED NOT NULL` |
| `remision_id` | `BIGINT UNSIGNED` |
| `registrada_por` | `BIGINT UNSIGNED NOT NULL` |
| `evaluada_en` | `DATETIME(6)` |
| `registrada_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `respuesta` | `TEXT NOT NULL` |

- `FOREIGN KEY (muestra_id) REFERENCES muestra(muestra_id)`
- `FOREIGN KEY (remision_id, muestra_id) REFERENCES muestra_remision_detalle(remision_id, muestra_id)`
- `FOREIGN KEY (registrada_por) REFERENCES actor(actor_id)`

## `muestra_pedido`

| Campo | Tipo y restricciones de columna |
|---|---|
| `muestra_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `vinculado_por` | `BIGINT UNSIGNED NOT NULL` |
| `vinculado_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `evidencia` | `VARCHAR(500) NOT NULL` |

- `PRIMARY KEY (muestra_id, pedido_id)`
- `FOREIGN KEY (muestra_id) REFERENCES muestra(muestra_id)`
- `FOREIGN KEY (pedido_id) REFERENCES pedido_ref(pedido_id)`
- `FOREIGN KEY (vinculado_por) REFERENCES actor(actor_id)`

## `oportunidad_sugerencia`

| Campo | Tipo y restricciones de columna |
|---|---|
| `sugerencia_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `empresa_id` | `BIGINT UNSIGNED NOT NULL` |
| `tipo` | `ENUM('ADQUISICION','CROSS_SELLING','UP_SELLING') NOT NULL` |
| `origen` | `VARCHAR(80) NOT NULL` |
| `criterio` | `TEXT NOT NULL` |
| `responsable_id` | `BIGINT UNSIGNED` |
| `generada_en` | `DATETIME(6) NOT NULL` |
| `clave_origen` | `VARCHAR(180) NOT NULL` |

- `UNIQUE (origen, clave_origen)`
- `UNIQUE (sugerencia_id, empresa_id, tipo)`
- `FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id)`
- `FOREIGN KEY (responsable_id) REFERENCES actor(actor_id)`

## `oportunidad_decision`

| Campo | Tipo y restricciones de columna |
|---|---|
| `sugerencia_id` | `BIGINT UNSIGNED PRIMARY KEY` |
| `decision` | `ENUM('ACEPTADA','RECHAZADA') NOT NULL` |
| `decidido_por` | `BIGINT UNSIGNED NOT NULL` |
| `decidida_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `motivo` | `VARCHAR(500) NOT NULL` |

- `UNIQUE (sugerencia_id, decision)`
- `FOREIGN KEY (sugerencia_id) REFERENCES oportunidad_sugerencia(sugerencia_id)`
- `FOREIGN KEY (decidido_por) REFERENCES actor(actor_id)`

## `oportunidad`

| Campo | Tipo y restricciones de columna |
|---|---|
| `oportunidad_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `empresa_id` | `BIGINT UNSIGNED NOT NULL` |
| `tipo` | `ENUM('ADQUISICION','CROSS_SELLING','UP_SELLING') NOT NULL` |
| `sugerencia_id` | `BIGINT UNSIGNED UNIQUE` |
| `decision_origen` | `ENUM('ACEPTADA','RECHAZADA')` |
| `responsable_id` | `BIGINT UNSIGNED NOT NULL` |
| `creada_por` | `BIGINT UNSIGNED NOT NULL` |
| `creada_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `criterio` | `TEXT NOT NULL` |

- `KEY ix_oportunidad_empresa_tipo (empresa_id, tipo)`
- `FOREIGN KEY (sugerencia_id, decision_origen) REFERENCES oportunidad_decision(sugerencia_id, decision)`
- `FOREIGN KEY (sugerencia_id, empresa_id, tipo) REFERENCES oportunidad_sugerencia(sugerencia_id, empresa_id, tipo)`
- `FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id)`
- `FOREIGN KEY (responsable_id) REFERENCES actor(actor_id)`
- `FOREIGN KEY (creada_por) REFERENCES actor(actor_id)`
- `CHECK ((sugerencia_id IS NULL AND decision_origen IS NULL) OR (sugerencia_id IS NOT NULL AND decision_origen IS NOT NULL AND decision_origen = 'ACEPTADA'))`

## `oportunidad_evento`

| Campo | Tipo y restricciones de columna |
|---|---|
| `evento_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `oportunidad_id` | `BIGINT UNSIGNED NOT NULL` |
| `actor_id` | `BIGINT UNSIGNED NOT NULL` |
| `registrado_en` | `DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6)` |
| `accion` | `VARCHAR(48) NOT NULL` |
| `detalle` | `TEXT NOT NULL` |

- `KEY ix_oportunidad_evento (oportunidad_id, registrado_en)`
- `FOREIGN KEY (oportunidad_id) REFERENCES oportunidad(oportunidad_id)`
- `FOREIGN KEY (actor_id) REFERENCES actor(actor_id)`

## `tier_modelo`

| Campo | Tipo y restricciones de columna |
|---|---|
| `version_modelo` | `VARCHAR(40) PRIMARY KEY` |
| `parametros` | `JSON NOT NULL` |
| `aprobado_por` | `BIGINT UNSIGNED` |
| `aprobado_en` | `DATETIME(6)` |

- `FOREIGN KEY (aprobado_por) REFERENCES actor(actor_id)`
- `CHECK ((aprobado_por IS NULL) = (aprobado_en IS NULL))`

## `tier_evaluacion`

| Campo | Tipo y restricciones de columna |
|---|---|
| `evaluacion_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `padre_id` | `BIGINT UNSIGNED NOT NULL` |
| `version_modelo` | `VARCHAR(40) NOT NULL` |
| `corte` | `DATETIME(6) NOT NULL` |
| `sugerido` | `ENUM('T1','T2','T3')` |
| `aprobado` | `ENUM('T1','T2','T3')` |
| `decidido_por` | `BIGINT UNSIGNED` |
| `decidido_en` | `DATETIME(6)` |
| `vigencia_desde` | `DATE` |
| `explicacion` | `TEXT NOT NULL` |

- `UNIQUE (padre_id, version_modelo, corte)`
- `FOREIGN KEY (padre_id) REFERENCES cuenta_padre(padre_id)`
- `FOREIGN KEY (version_modelo) REFERENCES tier_modelo(version_modelo)`
- `FOREIGN KEY (decidido_por) REFERENCES actor(actor_id)`
- `CHECK ((aprobado IS NULL AND decidido_por IS NULL AND decidido_en IS NULL AND vigencia_desde IS NULL) OR (aprobado IS NOT NULL AND decidido_por IS NOT NULL AND decidido_en IS NOT NULL AND vigencia_desde IS NOT NULL))`

## `corte_operacional`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `generado_en` | `DATETIME(6) NOT NULL` |
| `version_contrato` | `VARCHAR(40) NOT NULL` |
| `manifiesto` | `JSON NOT NULL` |

## `pedido_corte`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `estado_original` | `VARCHAR(64) NOT NULL` |

- `PRIMARY KEY (corte_id, pedido_id)`
- `FOREIGN KEY (corte_id) REFERENCES corte_operacional(corte_id)`
- `FOREIGN KEY (pedido_id) REFERENCES pedido_ref(pedido_id)`

## `pedido_linea`

| Campo | Tipo y restricciones de columna |
|---|---|
| `linea_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `linea_fuente` | `VARCHAR(100) NOT NULL` |
| `lin_neg_cod` | `SMALLINT UNSIGNED NOT NULL` |
| `producto_fuente` | `VARCHAR(100)` |
| `pais_destino_iso2` | `CHAR(2) CHARACTER SET ascii COLLATE ascii_bin` |
| `cantidad_original` | `DECIMAL(20,6)` |
| `unidad_original` | `VARCHAR(24)` |
| `kg` | `DECIMAL(20,6)` |
| `sacos` | `DECIMAL(20,6)` |
| `valor` | `DECIMAL(20,6)` |
| `moneda` | `CHAR(3)` |

- `UNIQUE (corte_id, pedido_id, linea_fuente)`
- `UNIQUE (corte_id, pedido_id, linea_id)`
- `FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id)`
- `FOREIGN KEY (pais_destino_iso2) REFERENCES pais(iso2)`
- `CHECK (lin_neg_cod = 20000)`
- `CHECK ((cantidad_original IS NULL) = (unidad_original IS NULL))`
- `CHECK ((valor IS NULL) = (moneda IS NULL))`

## `fecha_embarque_origen`

| Campo | Tipo y restricciones de columna |
|---|---|
| `fecha_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `linea_id` | `BIGINT UNSIGNED` |
| `clave_observacion` | `VARCHAR(160) NOT NULL` |
| `campo_fuente` | `VARCHAR(100) NOT NULL` |
| `valor_original` | `VARCHAR(100) NOT NULL` |
| `fecha_dia` | `DATE` |
| `periodo` | `CHAR(7)` |

- `UNIQUE (corte_id, pedido_id, clave_observacion)`
- `FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id)`
- `FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id)`
- `CHECK (fecha_dia IS NULL OR periodo IS NULL)`
- `CHECK (periodo IS NULL OR REGEXP_LIKE(periodo, '^[0-9]{4}-(0[1-9]|1[0-2])$'))`

## `compromiso`

| Campo | Tipo y restricciones de columna |
|---|---|
| `compromiso_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `clave_contrato` | `VARCHAR(150) NOT NULL` |
| `fecha_dia` | `DATE` |
| `periodo` | `CHAR(7)` |
| `evidencia` | `VARCHAR(500) NOT NULL` |

- `UNIQUE (corte_id, pedido_id, clave_contrato)`
- `UNIQUE (corte_id, pedido_id, compromiso_id)`
- `FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id)`
- `CHECK ((fecha_dia IS NULL) <> (periodo IS NULL))`
- `CHECK (periodo IS NULL OR REGEXP_LIKE(periodo, '^[0-9]{4}-(0[1-9]|1[0-2])$'))`

## `compromiso_linea`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `compromiso_id` | `BIGINT UNSIGNED NOT NULL` |
| `linea_id` | `BIGINT UNSIGNED NOT NULL` |
| `kg_asignados` | `DECIMAL(20,6)` |
| `sacos_asignados` | `DECIMAL(20,6)` |
| `valor_asignado` | `DECIMAL(20,6)` |
| `moneda` | `CHAR(3)` |
| `evidencia` | `VARCHAR(500) NOT NULL` |

- `PRIMARY KEY (corte_id, pedido_id, compromiso_id, linea_id)`
- `FOREIGN KEY (corte_id, pedido_id, compromiso_id) REFERENCES compromiso(corte_id, pedido_id, compromiso_id)`
- `FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id)`
- `CHECK ((valor_asignado IS NULL) = (moneda IS NULL))`

## `lote`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `fuente` | `VARCHAR(48) NOT NULL` |
| `clave_lote` | `VARCHAR(160) NOT NULL` |

- `PRIMARY KEY (corte_id, fuente, clave_lote)`
- `FOREIGN KEY (corte_id) REFERENCES corte_operacional(corte_id)`

## `linea_lote`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `linea_id` | `BIGINT UNSIGNED NOT NULL` |
| `fuente_lote` | `VARCHAR(48) NOT NULL` |
| `clave_lote` | `VARCHAR(160) NOT NULL` |
| `kg_asignados` | `DECIMAL(20,6)` |
| `evidencia` | `VARCHAR(500) NOT NULL` |

- `PRIMARY KEY (corte_id, pedido_id, linea_id, fuente_lote, clave_lote)`
- `FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id)`
- `FOREIGN KEY (corte_id, fuente_lote, clave_lote) REFERENCES lote(corte_id, fuente, clave_lote)`

## `embarque_efectivo`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `clave_movimiento` | `VARCHAR(160) NOT NULL` |
| `fuente` | `VARCHAR(48) NOT NULL` |
| `fecha_efectiva` | `DATE NOT NULL` |
| `destino_iso2` | `CHAR(2) CHARACTER SET ascii COLLATE ascii_bin` |

- `PRIMARY KEY (corte_id, pedido_id, fuente, clave_movimiento)`
- `FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id)`
- `FOREIGN KEY (destino_iso2) REFERENCES pais(iso2)`

## `embarque_linea`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `fuente` | `VARCHAR(48) NOT NULL` |
| `clave_movimiento` | `VARCHAR(160) NOT NULL` |
| `linea_id` | `BIGINT UNSIGNED NOT NULL` |
| `kg_efectivos` | `DECIMAL(20,6)` |
| `sacos_efectivos` | `DECIMAL(20,6)` |
| `valor_efectivo` | `DECIMAL(20,6)` |
| `moneda` | `CHAR(3)` |
| `evidencia` | `VARCHAR(500) NOT NULL` |

- `PRIMARY KEY (corte_id, pedido_id, fuente, clave_movimiento, linea_id)`
- `FOREIGN KEY (corte_id, pedido_id, fuente, clave_movimiento) REFERENCES embarque_efectivo(corte_id, pedido_id, fuente, clave_movimiento)`
- `FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id)`
- `CHECK ((valor_efectivo IS NULL) = (moneda IS NULL))`

## `cumplimiento_asignacion`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `fuente_embarque` | `VARCHAR(48) NOT NULL` |
| `clave_movimiento` | `VARCHAR(160) NOT NULL` |
| `compromiso_id` | `BIGINT UNSIGNED NOT NULL` |
| `linea_id` | `BIGINT UNSIGNED NOT NULL` |
| `kg_efectivos` | `DECIMAL(20,6) NOT NULL` |
| `evidencia` | `VARCHAR(500) NOT NULL` |

- `PRIMARY KEY (corte_id, pedido_id, fuente_embarque, clave_movimiento, compromiso_id, linea_id)`
- `FOREIGN KEY (corte_id, pedido_id, fuente_embarque, clave_movimiento, linea_id) REFERENCES embarque_linea(corte_id, pedido_id, fuente, clave_movimiento, linea_id)`
- `FOREIGN KEY (corte_id, pedido_id, compromiso_id, linea_id) REFERENCES compromiso_linea(corte_id, pedido_id, compromiso_id, linea_id)`
- `CHECK (kg_efectivos > 0)`

## `factura_observada`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `fuente` | `VARCHAR(48) NOT NULL` |
| `clave_documento` | `VARCHAR(160) NOT NULL` |
| `fecha_factura` | `DATE NOT NULL` |
| `estado_original` | `VARCHAR(64)` |

- `PRIMARY KEY (corte_id, pedido_id, fuente, clave_documento)`
- `FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id)`

## `margen_anticipado`

| Campo | Tipo y restricciones de columna |
|---|---|
| `margen_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `pedido_id` | `BIGINT UNSIGNED NOT NULL` |
| `linea_id` | `BIGINT UNSIGNED` |
| `clave_fuente` | `VARCHAR(160) NOT NULL` |
| `fuente` | `VARCHAR(48) NOT NULL` |
| `unidad` | `VARCHAR(32) NOT NULL` |
| `valor` | `DECIMAL(20,8) NOT NULL` |

- `UNIQUE (corte_id, fuente, clave_fuente)`
- `FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id)`
- `FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id)`

## `fcc_lote`

| Campo | Tipo y restricciones de columna |
|---|---|
| `corte_id` | `BIGINT UNSIGNED NOT NULL` |
| `fuente_lote` | `VARCHAR(48) NOT NULL` |
| `clave_lote` | `VARCHAR(160) NOT NULL` |
| `fuente_fcc` | `VARCHAR(48) NOT NULL` |
| `version_fuente` | `VARCHAR(80) NOT NULL` |
| `mnfcc_cop_kg` | `DECIMAL(20,8) NOT NULL` |
| `kg_base` | `DECIMAL(20,6)` |

- `PRIMARY KEY (corte_id, fuente_lote, clave_lote, fuente_fcc, version_fuente)`
- `FOREIGN KEY (corte_id, fuente_lote, clave_lote) REFERENCES lote(corte_id, fuente, clave_lote)`
- `CHECK (kg_base IS NULL OR kg_base > 0)`

## `presupuesto_version`

| Campo | Tipo y restricciones de columna |
|---|---|
| `version_id` | `BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY` |
| `fuente` | `VARCHAR(48) NOT NULL` |
| `version_fuente` | `VARCHAR(80) NOT NULL` |
| `corte` | `DATETIME(6) NOT NULL` |
| `aprobada_por` | `BIGINT UNSIGNED` |
| `aprobada_en` | `DATETIME(6)` |

- `UNIQUE (fuente, version_fuente)`
- `FOREIGN KEY (aprobada_por) REFERENCES actor(actor_id)`
- `CHECK ((aprobada_por IS NULL) = (aprobada_en IS NULL))`

## `presupuesto_anual`

| Campo | Tipo y restricciones de columna |
|---|---|
| `version_id` | `BIGINT UNSIGNED NOT NULL` |
| `anio` | `SMALLINT UNSIGNED NOT NULL` |
| `cliente_fuente` | `VARCHAR(160) NOT NULL` |
| `padre_id` | `BIGINT UNSIGNED` |
| `evidencia_padre` | `VARCHAR(500)` |
| `sacos` | `DECIMAL(20,6)` |
| `margen_uscent_lb` | `DECIMAL(20,8)` |
| `mnfcc` | `DECIMAL(20,8)` |
| `mnfcc_unidad` | `VARCHAR(40)` |
| `mnfcc_definicion` | `VARCHAR(500)` |

- `PRIMARY KEY (version_id, anio, cliente_fuente)`
- `UNIQUE (version_id, anio, padre_id)`
- `FOREIGN KEY (version_id) REFERENCES presupuesto_version(version_id)`
- `FOREIGN KEY (padre_id) REFERENCES cuenta_padre(padre_id)`
- `CHECK (anio BETWEEN 1900 AND 9999)`
- `CHECK ((padre_id IS NULL) = (evidencia_padre IS NULL))`
- `CHECK (sacos IS NULL OR sacos >= 0)`
- `CHECK ((mnfcc IS NULL AND mnfcc_unidad IS NULL AND mnfcc_definicion IS NULL) OR (mnfcc IS NOT NULL AND mnfcc_unidad IS NOT NULL AND mnfcc_definicion IS NOT NULL))`
