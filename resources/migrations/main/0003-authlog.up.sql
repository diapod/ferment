CREATE TABLE IF NOT EXISTS `authlog`
(
    `id`        BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
    `user_id`   BIGINT UNSIGNED          DEFAULT NULL,
    `client_ip` INET6                    DEFAULT NULL,
    `operation` VARCHAR(128)    NOT NULL,
    `success`   BOOLEAN         NOT NULL DEFAULT TRUE,
    `level`     ENUM('debug', 'info', 'notice', 'warning', 'error', 'critical', 'alert', 'emergency') NOT NULL DEFAULT 'info',
    `executed`  TIMESTAMP(6)    NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `message`   TEXT                     DEFAULT NULL,
    PRIMARY KEY (`id`),
    KEY `idx_authlog_executed` (`executed`),
    KEY `idx_authlog_operation` (`operation`),
    KEY `idx_authlog_operation_executed` (`operation`, `executed`),
    KEY `idx_authlog_level` (`level`),
    KEY `idx_authlog_success` (`success`),
    KEY `idx_authlog_client_ip` (`client_ip`),
    KEY `idx_authlog_user_id` (`user_id`),
    CONSTRAINT `fk_authlog_user_id`
        FOREIGN KEY (`user_id`) REFERENCES `users` (`id`)
            ON UPDATE CASCADE
            ON DELETE SET NULL
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci
  COMMENT = 'Ferment authentication operation log';
