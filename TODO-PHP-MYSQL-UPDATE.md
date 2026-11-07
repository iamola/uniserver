# PHP and MySQL update TODO

This checklist records the remaining work for the PHP 8.4 / MySQL modernization on `php-mysql-84-update`. PHP 8.4 references already exist in parts of the configuration, but the feature is not wired through the application end to end. Do not describe the stack as upgraded until the checklist is completed and the required runtime packages are available and tested.

## PHP 8.4 integration

- [ ] **Controller UI declarations:** Add a `MMSS_php84` menu component and its event-handler declaration to `UniController/main_unit.pas`.
- [ ] **Controller PHP version menu:** Add the PHP 8.4 menu item to `UniController/main_unit.lfm`, wired to `MMSS_php84Click`.
- [ ] **Controller selection handler:** Implement the PHP 8.4 selection/toggle handler in `UniController/main_unit.pas`, following the existing version-selection behavior (persist `PHP_SELECT`, update the environment/PATH, refresh state, prompt for Apache restart, and update the PEAR configuration where applicable).
- [ ] **Controller state/menu logic:** In `UniController/us_server_state.pas`, include PHP 8.4 in the installed-version check, enable/disable its menu entry based on `core\php84`, and set the PHP 8.4 checked state and its test/development/production INI availability.
- [ ] **Basic PHP configuration editor:** Add PHP 8.4 INI path selection to `UniController/edit_php_basic_form.pas` so its test, development, and production settings can be read and edited for `php84`.
- [ ] **PHP extension editor:** Add PHP 8.4 extension directory and INI selection handling to `UniController/php_extensions_form.pas`.
- [ ] **Service parity:** Update `UniService/default_config_vars.pas` to declare and initialize PHP 8.4, validate `PHP_SELECT=php84`, and include the selected PHP INI, CLI INI, Apache module config, and corresponding backup paths. Keep controller and service defaults consistent; verify legacy `us_user.ini` values continue to work.
- [ ] **Packaging/configuration:** Supply the PHP 8.4 Windows runtime in `core\php84` (including the expected INI files, extensions, and Apache module configuration `core\apache2\conf\extra_us\php84.conf`) and confirm the packaging/release process includes it. Pascal references alone do not provide the PHP runtime.
- [ ] **PHP validation:** Build the UniController and UniService projects; test startup with no PHP installed and with each supported PHP directory, select PHP 8.4, switch back to an older PHP version, edit each PHP 8.4 INI profile, toggle an extension, and verify Apache loads the matching module.

## MySQL modernization

- [ ] **Choose and document the target:** Specify the intended MySQL (or MariaDB) release and Windows architecture/source. “Latest” is time-dependent; do not hard-code an assumed release without identifying it.
- [ ] **Package the database runtime:** Provide the selected server/client binaries, required DLLs, configuration, and licensing/release metadata in the expected `core\mysql` layout. The controller cannot upgrade the bundled database merely by changing a version string.
- [ ] **Version and startup compatibility:** Review `MY_SQL_VER` parsing and all version-dependent startup flags (including MySQL 8 shared-memory handling) against the chosen release; make parsing safe for dotted version strings and MariaDB identifiers.
- [ ] **Configuration compatibility:** Validate `my.ini`, data/log/PID paths, initialization and recovery commands, authentication/password changes, service installation, console, backup, and restore against the selected MySQL release. Keep MariaDB behavior working if it remains supported.
- [ ] **Data upgrade safety:** Document a backup-and-restore or supported in-place migration procedure. Do not reuse an older database data directory with a newer server without confirming the vendor-supported upgrade path.
- [ ] **MySQL validation:** Test a clean initialization, start/stop, service start/stop, root and restricted-user operations, database create/delete, backup/restore, error logging, configured port, and upgrade from the currently bundled version using disposable test data.

## Completion criteria

- [ ] Both Pascal projects compile with the intended Lazarus/Free Pascal toolchain.
- [ ] PHP 8.4 selection, configuration editing, extensions, Apache integration, and service backup/restore paths work in an installed package.
- [ ] The chosen MySQL release passes the runtime and migration tests above, and MariaDB behavior is preserved or explicitly removed/documented.
- [ ] Release notes state exact PHP/MySQL versions, package sources, compatibility limits, and database migration instructions.
