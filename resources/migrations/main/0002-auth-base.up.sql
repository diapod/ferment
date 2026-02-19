CREATE TABLE IF NOT EXISTS `password_suites`
(
    `id`         BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
    `suite`      JSON            NOT NULL,
    `suite_hash` CHAR(64) GENERATED ALWAYS AS (SHA2(`suite`, 256)) STORED,
    PRIMARY KEY (`id`),
    UNIQUE KEY `uq_password_suites_hash` (`suite_hash`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci
  COMMENT = 'Ferment shared password suites';
--;;

CREATE TABLE IF NOT EXISTS `users`
(
    `id`                BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
    `email`             VARCHAR(191)    NOT NULL,
    `account_type`      VARCHAR(32)     NOT NULL DEFAULT 'user',
    `password_suite_id` BIGINT UNSIGNED          DEFAULT NULL,
    `password`          JSON                     DEFAULT NULL,
    `login_attempts`    SMALLINT UNSIGNED NOT NULL DEFAULT 0,
    `soft_locked`       TIMESTAMP(6)            DEFAULT NULL,
    `locked`            TIMESTAMP(6)            DEFAULT NULL,
    `created_at`        TIMESTAMP(6)    NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updated_at`        TIMESTAMP(6)    NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    PRIMARY KEY (`id`),
    UNIQUE KEY `uq_users_email` (`email`),
    KEY `idx_users_account_type` (`account_type`),
    KEY `idx_users_password_suite_id` (`password_suite_id`),
    CONSTRAINT `fk_users_password_suite`
        FOREIGN KEY (`password_suite_id`) REFERENCES `password_suites` (`id`)
            ON UPDATE CASCADE
            ON DELETE SET NULL
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci
  COMMENT = 'Ferment users with auth credentials';
