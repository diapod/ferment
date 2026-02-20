CREATE TABLE IF NOT EXISTS `user_roles`
(
    `user_id`    BIGINT UNSIGNED NOT NULL,
    `role`       VARCHAR(128)    NOT NULL,
    `created_at` TIMESTAMP(6)    NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    PRIMARY KEY (`user_id`, `role`),
    KEY `idx_user_roles_role` (`role`),
    KEY `idx_user_roles_created_at` (`created_at`),
    CONSTRAINT `fk_user_roles_user_id`
        FOREIGN KEY (`user_id`) REFERENCES `users` (`id`)
            ON UPDATE CASCADE
            ON DELETE CASCADE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci
  COMMENT = 'Explicit user roles for runtime authorization';
