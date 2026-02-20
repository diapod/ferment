CREATE TABLE IF NOT EXISTS `roles`
(
    `role`        VARCHAR(128) NOT NULL,
    `description` VARCHAR(255)          DEFAULT NULL,
    `created_at`  TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    PRIMARY KEY (`role`),
    KEY `idx_roles_created_at` (`created_at`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci
  COMMENT = 'Canonical role dictionary';
--;;

INSERT IGNORE INTO `roles` (`role`, `description`)
VALUES ('role/anonymous', 'Anonymous principal'),
       ('role/user', 'Authenticated user'),
       ('role/operator', 'Operational account'),
       ('role/admin', 'Administrative account'),
       ('role/infra-admin', 'Infrastructure administrator');
--;;

INSERT IGNORE INTO `roles` (`role`)
SELECT DISTINCT `role`
FROM `user_roles`
WHERE `role` IS NOT NULL
  AND `role` <> '';
--;;

ALTER TABLE `user_roles`
    ADD CONSTRAINT `fk_user_roles_role`
        FOREIGN KEY (`role`) REFERENCES `roles` (`role`)
            ON UPDATE CASCADE
            ON DELETE RESTRICT;
