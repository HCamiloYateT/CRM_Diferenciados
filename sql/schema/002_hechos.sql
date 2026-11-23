-- Contratos físicos candidatos. Tipos internos, no equivalen al esquema de origen.
-- Aplicar después de 001_crm.sql solo bajo autorización futura.

CREATE TABLE corte_operacional (
  corte_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  generado_en DATETIME(6) NOT NULL,
  version_contrato VARCHAR(40) NOT NULL,
  manifiesto JSON NOT NULL
);

CREATE TABLE pedido_corte (
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  estado_original VARCHAR(64) NOT NULL,
  PRIMARY KEY (corte_id, pedido_id),
  FOREIGN KEY (corte_id) REFERENCES corte_operacional(corte_id),
  FOREIGN KEY (pedido_id) REFERENCES pedido_ref(pedido_id)
);

CREATE TABLE pedido_linea (
  linea_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  linea_fuente VARCHAR(100) NOT NULL,
  lin_neg_cod SMALLINT UNSIGNED NOT NULL,
  producto_fuente VARCHAR(100),
  pais_destino_iso2 CHAR(2) CHARACTER SET ascii COLLATE ascii_bin,
  cantidad_original DECIMAL(20,6),
  unidad_original VARCHAR(24),
  kg DECIMAL(20,6),
  sacos DECIMAL(20,6),
  valor DECIMAL(20,6),
  moneda CHAR(3),
  UNIQUE (corte_id, pedido_id, linea_fuente),
  UNIQUE (corte_id, pedido_id, linea_id),
  FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id),
  FOREIGN KEY (pais_destino_iso2) REFERENCES pais(iso2),
  CHECK (lin_neg_cod = 20000),
  CHECK ((cantidad_original IS NULL) = (unidad_original IS NULL)),
  CHECK ((valor IS NULL) = (moneda IS NULL))
);

-- Observación de fecha original. Una fila de cabecera tiene linea_id nulo.
CREATE TABLE fecha_embarque_origen (
  fecha_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  linea_id BIGINT UNSIGNED,
  clave_observacion VARCHAR(160) NOT NULL,
  campo_fuente VARCHAR(100) NOT NULL,
  valor_original VARCHAR(100) NOT NULL,
  fecha_dia DATE,
  periodo CHAR(7),
  UNIQUE (corte_id, pedido_id, clave_observacion),
  FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id),
  FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id),
  CHECK (fecha_dia IS NULL OR periodo IS NULL),
  CHECK (periodo IS NULL OR REGEXP_LIKE(periodo, '^[0-9]{4}-(0[1-9]|1[0-2])$'))
);

CREATE TABLE compromiso (
  compromiso_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  clave_contrato VARCHAR(150) NOT NULL,
  fecha_dia DATE,
  periodo CHAR(7),
  evidencia VARCHAR(500) NOT NULL,
  UNIQUE (corte_id, pedido_id, clave_contrato),
  UNIQUE (corte_id, pedido_id, compromiso_id),
  FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id),
  CHECK ((fecha_dia IS NULL) <> (periodo IS NULL)),
  CHECK (periodo IS NULL OR REGEXP_LIKE(periodo, '^[0-9]{4}-(0[1-9]|1[0-2])$'))
);

CREATE TABLE compromiso_linea (
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  compromiso_id BIGINT UNSIGNED NOT NULL,
  linea_id BIGINT UNSIGNED NOT NULL,
  kg_asignados DECIMAL(20,6),
  sacos_asignados DECIMAL(20,6),
  valor_asignado DECIMAL(20,6),
  moneda CHAR(3),
  evidencia VARCHAR(500) NOT NULL,
  PRIMARY KEY (corte_id, pedido_id, compromiso_id, linea_id),
  FOREIGN KEY (corte_id, pedido_id, compromiso_id)
    REFERENCES compromiso(corte_id, pedido_id, compromiso_id),
  FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id),
  CHECK ((valor_asignado IS NULL) = (moneda IS NULL))
);

CREATE TABLE lote (
  corte_id BIGINT UNSIGNED NOT NULL,
  fuente VARCHAR(48) NOT NULL,
  clave_lote VARCHAR(160) NOT NULL,
  PRIMARY KEY (corte_id, fuente, clave_lote),
  FOREIGN KEY (corte_id) REFERENCES corte_operacional(corte_id)
);

CREATE TABLE linea_lote (
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  linea_id BIGINT UNSIGNED NOT NULL,
  fuente_lote VARCHAR(48) NOT NULL,
  clave_lote VARCHAR(160) NOT NULL,
  kg_asignados DECIMAL(20,6),
  evidencia VARCHAR(500) NOT NULL,
  PRIMARY KEY (corte_id, pedido_id, linea_id, fuente_lote, clave_lote),
  FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id),
  FOREIGN KEY (corte_id, fuente_lote, clave_lote) REFERENCES lote(corte_id, fuente, clave_lote)
);

CREATE TABLE embarque_efectivo (
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  clave_movimiento VARCHAR(160) NOT NULL,
  fuente VARCHAR(48) NOT NULL,
  fecha_efectiva DATE NOT NULL,
  destino_iso2 CHAR(2) CHARACTER SET ascii COLLATE ascii_bin,
  PRIMARY KEY (corte_id, pedido_id, fuente, clave_movimiento),
  FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id),
  FOREIGN KEY (destino_iso2) REFERENCES pais(iso2)
);

CREATE TABLE embarque_linea (
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  fuente VARCHAR(48) NOT NULL,
  clave_movimiento VARCHAR(160) NOT NULL,
  linea_id BIGINT UNSIGNED NOT NULL,
  kg_efectivos DECIMAL(20,6),
  sacos_efectivos DECIMAL(20,6),
  valor_efectivo DECIMAL(20,6),
  moneda CHAR(3),
  evidencia VARCHAR(500) NOT NULL,
  PRIMARY KEY (corte_id, pedido_id, fuente, clave_movimiento, linea_id),
  FOREIGN KEY (corte_id, pedido_id, fuente, clave_movimiento)
    REFERENCES embarque_efectivo(corte_id, pedido_id, fuente, clave_movimiento),
  FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id),
  CHECK ((valor_efectivo IS NULL) = (moneda IS NULL))
);

CREATE TABLE cumplimiento_asignacion (
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  fuente_embarque VARCHAR(48) NOT NULL,
  clave_movimiento VARCHAR(160) NOT NULL,
  compromiso_id BIGINT UNSIGNED NOT NULL,
  linea_id BIGINT UNSIGNED NOT NULL,
  kg_efectivos DECIMAL(20,6) NOT NULL,
  evidencia VARCHAR(500) NOT NULL,
  PRIMARY KEY (corte_id, pedido_id, fuente_embarque, clave_movimiento, compromiso_id, linea_id),
  FOREIGN KEY (corte_id, pedido_id, fuente_embarque, clave_movimiento, linea_id)
    REFERENCES embarque_linea(corte_id, pedido_id, fuente, clave_movimiento, linea_id),
  FOREIGN KEY (corte_id, pedido_id, compromiso_id, linea_id)
    REFERENCES compromiso_linea(corte_id, pedido_id, compromiso_id, linea_id),
  CHECK (kg_efectivos > 0)
);

CREATE TABLE factura_observada (
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  fuente VARCHAR(48) NOT NULL,
  clave_documento VARCHAR(160) NOT NULL,
  fecha_factura DATE NOT NULL,
  estado_original VARCHAR(64),
  PRIMARY KEY (corte_id, pedido_id, fuente, clave_documento),
  FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id)
);

CREATE TABLE margen_anticipado (
  margen_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  corte_id BIGINT UNSIGNED NOT NULL,
  pedido_id BIGINT UNSIGNED NOT NULL,
  linea_id BIGINT UNSIGNED,
  clave_fuente VARCHAR(160) NOT NULL,
  fuente VARCHAR(48) NOT NULL,
  unidad VARCHAR(32) NOT NULL,
  valor DECIMAL(20,8) NOT NULL,
  UNIQUE (corte_id, fuente, clave_fuente),
  FOREIGN KEY (corte_id, pedido_id) REFERENCES pedido_corte(corte_id, pedido_id),
  FOREIGN KEY (corte_id, pedido_id, linea_id) REFERENCES pedido_linea(corte_id, pedido_id, linea_id)
);

CREATE TABLE fcc_lote (
  corte_id BIGINT UNSIGNED NOT NULL,
  fuente_lote VARCHAR(48) NOT NULL,
  clave_lote VARCHAR(160) NOT NULL,
  fuente_fcc VARCHAR(48) NOT NULL,
  version_fuente VARCHAR(80) NOT NULL,
  mnfcc_cop_kg DECIMAL(20,8) NOT NULL,
  kg_base DECIMAL(20,6),
  PRIMARY KEY (corte_id, fuente_lote, clave_lote, fuente_fcc, version_fuente),
  FOREIGN KEY (corte_id, fuente_lote, clave_lote) REFERENCES lote(corte_id, fuente, clave_lote),
  CHECK (kg_base IS NULL OR kg_base > 0)
);

-- Un cliente presupuestal identificado por fuente, con padre explícito cuando se valide.
CREATE TABLE presupuesto_version (
  version_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  fuente VARCHAR(48) NOT NULL,
  version_fuente VARCHAR(80) NOT NULL,
  corte DATETIME(6) NOT NULL,
  aprobada_por BIGINT UNSIGNED,
  aprobada_en DATETIME(6),
  UNIQUE (fuente, version_fuente),
  FOREIGN KEY (aprobada_por) REFERENCES actor(actor_id),
  CHECK ((aprobada_por IS NULL) = (aprobada_en IS NULL))
);

CREATE TABLE presupuesto_anual (
  version_id BIGINT UNSIGNED NOT NULL,
  anio SMALLINT UNSIGNED NOT NULL,
  cliente_fuente VARCHAR(160) NOT NULL,
  padre_id BIGINT UNSIGNED,
  evidencia_padre VARCHAR(500),
  sacos DECIMAL(20,6),
  margen_uscent_lb DECIMAL(20,8),
  mnfcc DECIMAL(20,8),
  mnfcc_unidad VARCHAR(40),
  mnfcc_definicion VARCHAR(500),
  PRIMARY KEY (version_id, anio, cliente_fuente),
  UNIQUE (version_id, anio, padre_id),
  FOREIGN KEY (version_id) REFERENCES presupuesto_version(version_id),
  FOREIGN KEY (padre_id) REFERENCES cuenta_padre(padre_id),
  CHECK (anio BETWEEN 1900 AND 9999),
  CHECK ((padre_id IS NULL) = (evidencia_padre IS NULL)),
  CHECK (sacos IS NULL OR sacos >= 0),
  CHECK ((mnfcc IS NULL AND mnfcc_unidad IS NULL AND mnfcc_definicion IS NULL) OR
         (mnfcc IS NOT NULL AND mnfcc_unidad IS NOT NULL AND mnfcc_definicion IS NOT NULL))
);
