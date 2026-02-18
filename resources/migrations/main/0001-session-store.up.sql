CREATE TABLE IF NOT EXISTS `sessions`
(
    `session_id`     VARCHAR(191)  NOT NULL,
    `version`        BIGINT        NOT NULL DEFAULT 0,
    `state`          VARCHAR(32)   NOT NULL DEFAULT 'warm',
    `frozen`         TINYINT(1)    NOT NULL DEFAULT 1,
    `created_at`     VARCHAR(64)   NOT NULL,
    `updated_at`     VARCHAR(64)   NOT NULL,
    `last_access_at` VARCHAR(64)   NOT NULL,
    `frozen_at`      VARCHAR(64)            DEFAULT NULL,
    `thawed_at`      VARCHAR(64)            DEFAULT NULL,
    `summary`        JSON                   DEFAULT NULL,
    `snapshot`       JSON                   DEFAULT NULL,
    `meta`           JSON                   DEFAULT NULL,
    `turns`          JSON          NOT NULL,
    `facts`          JSON          NOT NULL,
    PRIMARY KEY (`session_id`),
    KEY `idx_sessions_updated_at` (`updated_at`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci
  COMMENT = 'Ferment session store';
--;;

CREATE TABLE IF NOT EXISTS `session_vars`
(
    `session_id` VARCHAR(191) NOT NULL,
    `id`         VARCHAR(191) NOT NULL,
    `value`      MEDIUMBLOB,
    `updated_at` TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    PRIMARY KEY (`session_id`, `id`),
    KEY `idx_session_vars_id` (`id`),
    CONSTRAINT `fk_session_vars_session`
        FOREIGN KEY (`session_id`) REFERENCES `sessions` (`session_id`)
            ON DELETE CASCADE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci
  COMMENT = 'Ferment session key-value store';
