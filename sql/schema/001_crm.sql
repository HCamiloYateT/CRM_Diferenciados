-- Diseño Fase 2. Dialecto provisional MySQL 8.0.16+, esquema vacío previamente autorizado.
-- Aplicar solo mediante un procedimiento futuro aprobado. Sin conexiones ni datos iniciales.

CREATE TABLE pais (
  iso2 CHAR(2) CHARACTER SET ascii COLLATE ascii_bin PRIMARY KEY,
  nombre VARCHAR(120) NOT NULL,
  CONSTRAINT ck_iso2 CHECK (REGEXP_LIKE(iso2, '^[A-Z]{2}$', 'c'))
);

CREATE TABLE pais_equivalencia (
  fuente VARCHAR(48) NOT NULL,
  codigo_original VARCHAR(80) NOT NULL,
  desde DATE NOT NULL,
  hasta DATE,
  iso2 CHAR(2) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,
  evidencia VARCHAR(500) NOT NULL,
  PRIMARY KEY (fuente, codigo_original, desde),
  FOREIGN KEY (iso2) REFERENCES pais(iso2),
  CHECK (hasta IS NULL OR hasta > desde)
);

CREATE TABLE tipologia (
  codigo VARCHAR(24) PRIMARY KEY,
  nombre VARCHAR(100) NOT NULL UNIQUE
);

CREATE TABLE actor (
  actor_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  identidad VARCHAR(128) NOT NULL UNIQUE,
  clase ENUM('USUARIO','PROCESO') NOT NULL,
  habilitado BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE TABLE rol_actor (
  actor_id BIGINT UNSIGNED NOT NULL,
  rol VARCHAR(40) NOT NULL,
  PRIMARY KEY (actor_id, rol),
  FOREIGN KEY (actor_id) REFERENCES actor(actor_id)
);

CREATE TABLE empresa (
  empresa_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(200) NOT NULL,
  pais_iso2 CHAR(2) CHARACTER SET ascii COLLATE ascii_bin,
  tipologia_codigo VARCHAR(24),
  creado_por BIGINT UNSIGNED NOT NULL,
  creado_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  FOREIGN KEY (pais_iso2) REFERENCES pais(iso2),
  FOREIGN KEY (tipologia_codigo) REFERENCES tipologia(codigo),
  FOREIGN KEY (creado_por) REFERENCES actor(actor_id)
);

-- PerCod es opaco hasta verificar el tipo y reglas de representación en la exportación.
CREATE TABLE persona_operativa (
  percod VARCHAR(64) COLLATE utf8mb4_bin PRIMARY KEY,
  fuente VARCHAR(48) NOT NULL,
  corte DATETIME(6) NOT NULL,
  pais_empresa_iso2 CHAR(2) CHARACTER SET ascii COLLATE ascii_bin,
  FOREIGN KEY (pais_empresa_iso2) REFERENCES pais(iso2)
) DEFAULT CHARSET=utf8mb4;

CREATE TABLE empresa_percod (
  vinculo_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  empresa_id BIGINT UNSIGNED NOT NULL,
  percod VARCHAR(64) COLLATE utf8mb4_bin NOT NULL,
  desde DATETIME(6) NOT NULL,
  hasta DATETIME(6),
  registrado_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  validado_por BIGINT UNSIGNED NOT NULL,
  evidencia VARCHAR(500) NOT NULL,
  cerrado_por BIGINT UNSIGNED,
  motivo_cierre VARCHAR(500),
  percod_vigente VARCHAR(64) COLLATE utf8mb4_bin GENERATED ALWAYS AS
    (CASE WHEN hasta IS NULL THEN percod END) STORED,
  UNIQUE (percod_vigente),
  UNIQUE (empresa_id, percod, desde),
  KEY ix_percod_vigencia (percod, desde, hasta),
  FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id),
  FOREIGN KEY (percod) REFERENCES persona_operativa(percod),
  FOREIGN KEY (validado_por) REFERENCES actor(actor_id),
  FOREIGN KEY (cerrado_por) REFERENCES actor(actor_id),
  CHECK (hasta IS NULL OR hasta > desde),
  CHECK ((hasta IS NULL AND cerrado_por IS NULL AND motivo_cierre IS NULL) OR
         (hasta IS NOT NULL AND cerrado_por IS NOT NULL AND motivo_cierre IS NOT NULL))
) DEFAULT CHARSET=utf8mb4;

CREATE TABLE cuenta_padre (
  padre_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  fuente VARCHAR(48) NOT NULL,
  codigo_fuente VARCHAR(100) NOT NULL,
  nombre VARCHAR(200) NOT NULL,
  UNIQUE (fuente, codigo_fuente)
);

CREATE TABLE cuenta_padre_miembro (
  padre_id BIGINT UNSIGNED NOT NULL,
  percod VARCHAR(64) COLLATE utf8mb4_bin NOT NULL,
  desde DATE NOT NULL,
  hasta DATE,
  validado_por BIGINT UNSIGNED NOT NULL,
  registrado_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  evidencia VARCHAR(500) NOT NULL,
  miembro_vigente VARCHAR(64) COLLATE utf8mb4_bin GENERATED ALWAYS AS
    (CASE WHEN hasta IS NULL THEN percod END) STORED,
  PRIMARY KEY (percod, desde),
  UNIQUE (miembro_vigente),
  KEY ix_padre_vigencia (padre_id, desde, hasta),
  FOREIGN KEY (padre_id) REFERENCES cuenta_padre(padre_id),
  FOREIGN KEY (percod) REFERENCES persona_operativa(percod),
  FOREIGN KEY (validado_por) REFERENCES actor(actor_id),
  CHECK (hasta IS NULL OR hasta > desde)
) DEFAULT CHARSET=utf8mb4;

CREATE TABLE contacto (
  contacto_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(200) NOT NULL,
  correo VARCHAR(254),
  pais_iso2 CHAR(2) CHARACTER SET ascii COLLATE ascii_bin,
  FOREIGN KEY (pais_iso2) REFERENCES pais(iso2)
);

CREATE TABLE empresa_contacto (
  empresa_id BIGINT UNSIGNED NOT NULL,
  contacto_id BIGINT UNSIGNED NOT NULL,
  funcion VARCHAR(120),
  PRIMARY KEY (empresa_id, contacto_id),
  FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id),
  FOREIGN KEY (contacto_id) REFERENCES contacto(contacto_id)
);

CREATE TABLE asignacion_comercial (
  asignacion_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  empresa_id BIGINT UNSIGNED NOT NULL,
  comercial_id BIGINT UNSIGNED NOT NULL,
  desde DATETIME(6) NOT NULL,
  hasta DATETIME(6),
  asignado_por BIGINT UNSIGNED NOT NULL,
  motivo VARCHAR(500) NOT NULL,
  UNIQUE (empresa_id, comercial_id, desde),
  KEY ix_asignacion_comercial (comercial_id, desde, hasta),
  FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id),
  FOREIGN KEY (comercial_id) REFERENCES actor(actor_id),
  FOREIGN KEY (asignado_por) REFERENCES actor(actor_id),
  CHECK (hasta IS NULL OR hasta > desde)
);

CREATE TABLE adquisicion_evento (
  evento_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  empresa_id BIGINT UNSIGNED NOT NULL,
  etapa_anterior ENUM('CONTACTO','PROSPECTO','LEAD'),
  etapa_nueva ENUM('CONTACTO','PROSPECTO','LEAD') NOT NULL,
  efectiva_en DATETIME(6) NOT NULL,
  registrada_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  actor_id BIGINT UNSIGNED NOT NULL,
  motivo VARCHAR(500) NOT NULL,
  evidencia VARCHAR(500),
  KEY ix_adquisicion_empresa_fecha (empresa_id, efectiva_en, evento_id),
  FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id),
  FOREIGN KEY (actor_id) REFERENCES actor(actor_id),
  CHECK (etapa_anterior IS NULL OR etapa_anterior <> etapa_nueva)
);

-- Referencia mínima a una cabecera con al menos una línea elegible en el snapshot.
CREATE TABLE pedido_ref (
  pedido_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  fuente VARCHAR(48) NOT NULL,
  clave_fuente VARCHAR(200) NOT NULL,
  percod VARCHAR(64) COLLATE utf8mb4_bin NOT NULL,
  creado_en DATETIME(6) NOT NULL,
  precision_creacion ENUM('DIA','INSTANTE') NOT NULL,
  corte DATETIME(6) NOT NULL,
  UNIQUE (fuente, clave_fuente),
  UNIQUE (pedido_id, percod),
  KEY ix_pedido_cliente_fecha (percod, creado_en, pedido_id),
  FOREIGN KEY (percod) REFERENCES persona_operativa(percod)
) DEFAULT CHARSET=utf8mb4;

CREATE TABLE pedido_estado_observado (
  pedido_id BIGINT UNSIGNED NOT NULL,
  corte DATETIME(6) NOT NULL,
  estado_original VARCHAR(64) NOT NULL,
  fecha_efectiva DATE,
  PRIMARY KEY (pedido_id, corte),
  FOREIGN KEY (pedido_id) REFERENCES pedido_ref(pedido_id)
);

CREATE TABLE conversion (
  percod VARCHAR(64) COLLATE utf8mb4_bin PRIMARY KEY,
  primer_pedido_id BIGINT UNSIGNED NOT NULL UNIQUE,
  registrado_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  corte_evidencia DATETIME(6) NOT NULL,
  version_regla VARCHAR(40) NOT NULL,
  FOREIGN KEY (primer_pedido_id, percod) REFERENCES pedido_ref(pedido_id, percod)
) DEFAULT CHARSET=utf8mb4;

CREATE TABLE conversion_correccion (
  correccion_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  percod VARCHAR(64) COLLATE utf8mb4_bin NOT NULL,
  pedido_anterior_id BIGINT UNSIGNED NOT NULL,
  pedido_nuevo_id BIGINT UNSIGNED NOT NULL,
  corte_evidencia DATETIME(6) NOT NULL,
  version_regla VARCHAR(40) NOT NULL,
  aprobado_por BIGINT UNSIGNED NOT NULL,
  registrada_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  evidencia VARCHAR(500) NOT NULL,
  KEY ix_conversion_correccion (percod, registrada_en),
  FOREIGN KEY (percod) REFERENCES conversion(percod),
  FOREIGN KEY (pedido_anterior_id, percod) REFERENCES pedido_ref(pedido_id, percod),
  FOREIGN KEY (pedido_nuevo_id, percod) REFERENCES pedido_ref(pedido_id, percod),
  FOREIGN KEY (aprobado_por) REFERENCES actor(actor_id),
  CHECK (pedido_anterior_id <> pedido_nuevo_id)
) DEFAULT CHARSET=utf8mb4;

CREATE TABLE regla_actividad (
  version_regla VARCHAR(40) PRIMARY KEY,
  parametros JSON NOT NULL,
  aprobada_por BIGINT UNSIGNED,
  aprobada_en DATETIME(6),
  FOREIGN KEY (aprobada_por) REFERENCES actor(actor_id),
  CHECK ((aprobada_por IS NULL) = (aprobada_en IS NULL))
);

CREATE TABLE actividad_evaluada (
  percod VARCHAR(64) COLLATE utf8mb4_bin NOT NULL,
  corte DATETIME(6) NOT NULL,
  version_regla VARCHAR(40) NOT NULL,
  estado VARCHAR(40) NOT NULL,
  PRIMARY KEY (percod, corte, version_regla),
  FOREIGN KEY (percod) REFERENCES persona_operativa(percod),
  FOREIGN KEY (version_regla) REFERENCES regla_actividad(version_regla)
) DEFAULT CHARSET=utf8mb4;

CREATE TABLE muestra_solicitud (
  solicitud_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  empresa_id BIGINT UNSIGNED NOT NULL,
  contacto_id BIGINT UNSIGNED,
  solicitada_en DATETIME(6) NOT NULL,
  requisitos TEXT,
  registrada_por BIGINT UNSIGNED NOT NULL,
  FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id),
  FOREIGN KEY (empresa_id, contacto_id) REFERENCES empresa_contacto(empresa_id, contacto_id),
  FOREIGN KEY (registrada_por) REFERENCES actor(actor_id)
);

CREATE TABLE muestra (
  muestra_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  solicitud_id BIGINT UNSIGNED NOT NULL,
  referencia_producto VARCHAR(120),
  atributos JSON,
  FOREIGN KEY (solicitud_id) REFERENCES muestra_solicitud(solicitud_id)
);

CREATE TABLE muestra_remision (
  remision_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  transportadora VARCHAR(120),
  guia VARCHAR(120),
  enviada_en DATETIME(6),
  entregada_en DATETIME(6),
  destino_iso2 CHAR(2) CHARACTER SET ascii COLLATE ascii_bin,
  FOREIGN KEY (destino_iso2) REFERENCES pais(iso2),
  CHECK (enviada_en IS NULL OR entregada_en IS NULL OR entregada_en >= enviada_en)
);

CREATE TABLE muestra_remision_detalle (
  remision_id BIGINT UNSIGNED NOT NULL,
  muestra_id BIGINT UNSIGNED NOT NULL,
  cantidad DECIMAL(18,6),
  unidad VARCHAR(24),
  PRIMARY KEY (remision_id, muestra_id),
  FOREIGN KEY (remision_id) REFERENCES muestra_remision(remision_id),
  FOREIGN KEY (muestra_id) REFERENCES muestra(muestra_id),
  CHECK ((cantidad IS NULL) = (unidad IS NULL)),
  CHECK (cantidad IS NULL OR cantidad > 0)
);

CREATE TABLE muestra_evaluacion (
  evaluacion_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  muestra_id BIGINT UNSIGNED NOT NULL,
  remision_id BIGINT UNSIGNED,
  registrada_por BIGINT UNSIGNED NOT NULL,
  evaluada_en DATETIME(6),
  registrada_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  respuesta TEXT NOT NULL,
  FOREIGN KEY (muestra_id) REFERENCES muestra(muestra_id),
  FOREIGN KEY (remision_id, muestra_id) REFERENCES muestra_remision_detalle(remision_id, muestra_id),
  FOREIGN KEY (registrada_por) REFERENCES actor(actor_id)
);

CREATE TABLE muestra_pedido (
  muestra_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  vinculado_por BIGINT UNSIGNED NOT NULL,
  vinculado_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  evidencia VARCHAR(500) NOT NULL,
  PRIMARY KEY (muestra_id, pedido_id),
  FOREIGN KEY (muestra_id) REFERENCES muestra(muestra_id),
  FOREIGN KEY (pedido_id) REFERENCES pedido_ref(pedido_id),
  FOREIGN KEY (vinculado_por) REFERENCES actor(actor_id)
);

CREATE TABLE oportunidad_sugerencia (
  sugerencia_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  empresa_id BIGINT UNSIGNED NOT NULL,
  tipo ENUM('ADQUISICION','CROSS_SELLING','UP_SELLING') NOT NULL,
  origen VARCHAR(80) NOT NULL,
  criterio TEXT NOT NULL,
  responsable_id BIGINT UNSIGNED,
  generada_en DATETIME(6) NOT NULL,
  clave_origen VARCHAR(180) NOT NULL,
  UNIQUE (origen, clave_origen),
  UNIQUE (sugerencia_id, empresa_id, tipo),
  FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id),
  FOREIGN KEY (responsable_id) REFERENCES actor(actor_id)
);

CREATE TABLE oportunidad_decision (
  sugerencia_id BIGINT UNSIGNED PRIMARY KEY,
  decision ENUM('ACEPTADA','RECHAZADA') NOT NULL,
  decidido_por BIGINT UNSIGNED NOT NULL,
  decidida_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  motivo VARCHAR(500) NOT NULL,
  UNIQUE (sugerencia_id, decision),
  FOREIGN KEY (sugerencia_id) REFERENCES oportunidad_sugerencia(sugerencia_id),
  FOREIGN KEY (decidido_por) REFERENCES actor(actor_id)
);

CREATE TABLE oportunidad (
  oportunidad_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  empresa_id BIGINT UNSIGNED NOT NULL,
  tipo ENUM('ADQUISICION','CROSS_SELLING','UP_SELLING') NOT NULL,
  sugerencia_id BIGINT UNSIGNED UNIQUE,
  decision_origen ENUM('ACEPTADA','RECHAZADA'),
  responsable_id BIGINT UNSIGNED NOT NULL,
  creada_por BIGINT UNSIGNED NOT NULL,
  creada_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  criterio TEXT NOT NULL,
  KEY ix_oportunidad_empresa_tipo (empresa_id, tipo),
  FOREIGN KEY (sugerencia_id, decision_origen) REFERENCES oportunidad_decision(sugerencia_id, decision),
  FOREIGN KEY (sugerencia_id, empresa_id, tipo)
    REFERENCES oportunidad_sugerencia(sugerencia_id, empresa_id, tipo),
  FOREIGN KEY (empresa_id) REFERENCES empresa(empresa_id),
  FOREIGN KEY (responsable_id) REFERENCES actor(actor_id),
  FOREIGN KEY (creada_por) REFERENCES actor(actor_id),
  CHECK ((sugerencia_id IS NULL AND decision_origen IS NULL) OR
         (sugerencia_id IS NOT NULL AND decision_origen IS NOT NULL AND decision_origen = 'ACEPTADA'))
);

CREATE TABLE oportunidad_evento (
  evento_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  oportunidad_id BIGINT UNSIGNED NOT NULL,
  actor_id BIGINT UNSIGNED NOT NULL,
  registrado_en DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
  accion VARCHAR(48) NOT NULL,
  detalle TEXT NOT NULL,
  KEY ix_oportunidad_evento (oportunidad_id, registrado_en),
  FOREIGN KEY (oportunidad_id) REFERENCES oportunidad(oportunidad_id),
  FOREIGN KEY (actor_id) REFERENCES actor(actor_id)
);

CREATE TABLE tier_modelo (
  version_modelo VARCHAR(40) PRIMARY KEY,
  parametros JSON NOT NULL,
  aprobado_por BIGINT UNSIGNED,
  aprobado_en DATETIME(6),
  FOREIGN KEY (aprobado_por) REFERENCES actor(actor_id),
  CHECK ((aprobado_por IS NULL) = (aprobado_en IS NULL))
);

CREATE TABLE tier_evaluacion (
  evaluacion_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  padre_id BIGINT UNSIGNED NOT NULL,
  version_modelo VARCHAR(40) NOT NULL,
  corte DATETIME(6) NOT NULL,
  sugerido ENUM('T1','T2','T3'),
  aprobado ENUM('T1','T2','T3'),
  decidido_por BIGINT UNSIGNED,
  decidido_en DATETIME(6),
  vigencia_desde DATE,
  explicacion TEXT NOT NULL,
  UNIQUE (padre_id, version_modelo, corte),
  FOREIGN KEY (padre_id) REFERENCES cuenta_padre(padre_id),
  FOREIGN KEY (version_modelo) REFERENCES tier_modelo(version_modelo),
  FOREIGN KEY (decidido_por) REFERENCES actor(actor_id),
  CHECK ((aprobado IS NULL AND decidido_por IS NULL AND decidido_en IS NULL AND vigencia_desde IS NULL) OR
         (aprobado IS NOT NULL AND decidido_por IS NOT NULL AND decidido_en IS NOT NULL AND vigencia_desde IS NOT NULL))
);
