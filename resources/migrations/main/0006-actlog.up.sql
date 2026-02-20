CREATE TABLE IF NOT EXISTS `actlog`
(
    `id`                     BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
    `trace_id`               VARCHAR(191)    NOT NULL,
    `request_id`             VARCHAR(191)             DEFAULT NULL,
    `session_id`             VARCHAR(191)             DEFAULT NULL,
    `principal_id`           BIGINT UNSIGNED          DEFAULT NULL,
    `principal_email`        VARCHAR(191)             DEFAULT NULL,
    `principal_account_type` VARCHAR(32)              DEFAULT NULL,
    `principal_roles`        TEXT                     DEFAULT NULL,
    `intent`                 VARCHAR(128)    NOT NULL,
    `capability`             VARCHAR(128)             DEFAULT NULL,
    `outcome`                ENUM('ok', 'error') NOT NULL DEFAULT 'error',
    `status`                 SMALLINT UNSIGNED NOT NULL DEFAULT 500,
    `error_type`             VARCHAR(128)             DEFAULT NULL,
    `latency_ms`             DECIMAL(12,3)            DEFAULT NULL,
    `executed`               TIMESTAMP(6)    NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `message`                TEXT                     DEFAULT NULL,
    PRIMARY KEY (`id`),
    KEY `idx_actlog_trace_id` (`trace_id`),
    KEY `idx_actlog_request_id` (`request_id`),
    KEY `idx_actlog_session_id` (`session_id`),
    KEY `idx_actlog_principal_id` (`principal_id`),
    KEY `idx_actlog_intent` (`intent`),
    KEY `idx_actlog_capability` (`capability`),
    KEY `idx_actlog_outcome` (`outcome`),
    KEY `idx_actlog_status` (`status`),
    KEY `idx_actlog_executed` (`executed`),
    CONSTRAINT `fk_actlog_principal_id`
        FOREIGN KEY (`principal_id`) REFERENCES `users` (`id`)
            ON UPDATE CASCADE
            ON DELETE SET NULL
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci
  COMMENT = 'Ferment /v1/act audit trail';
