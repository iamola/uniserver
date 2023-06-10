program UniController;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main_unit, default_config_vars, us_common_procedures,
  us_common_functions, about_form, us_server_state, server_cert_key_gen_form,
  vhost_create_form, vhost_delete_form, apache_modules_form, apache_basic_form,
  root_www_pass_access_form, root_ssl_pass_access_form, php_extensions_form,
  edit_php_basic_form, msmtp_php_mail_form, are_servers_runable, httpsend,
  server_internet_status_form, pc_win_startup_form, dtdns_form, cron_form,
  lazcontrols, command_line_start_up, mysql_db_create_delete_form,
  mysql_create_restricted_user_form, mysql_edit_restricted_user_form,
  mysql_database_backup_form, mysql_database_restore_form, message_dlg_form;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TAbout, About);
  Application.CreateForm(Tserver_cert_key_gen, server_cert_key_gen);
  Application.CreateForm(Tvhost_create, vhost_create);
  Application.CreateForm(Tvhost_delete, vhost_delete);
  Application.CreateForm(Tapache_modules, apache_modules);
  Application.CreateForm(Tapache_basic, apache_basic);
  Application.CreateForm(Troot_www_pass_access, root_www_pass_access);
  Application.CreateForm(Troot_ssl_pass_access, root_ssl_pass_access);
  Application.CreateForm(Tphp_extensions, php_extensions);
  Application.CreateForm(Tedit_php_basic, edit_php_basic);
  Application.CreateForm(Tmsmtp_php_mail, msmtp_php_mail);
  Application.CreateForm(Tserver_internet_status, server_internet_status);
  Application.CreateForm(Tpc_win_startup, pc_win_startup);
  Application.CreateForm(Tdtdns, dtdns);
  Application.CreateForm(Tcron, cron);
  Application.CreateForm(Tmysql_db_create_delete, mysql_db_create_delete);
  Application.CreateForm(Tmysql_create_restricted_user, mysql_create_restricted_user);
  Application.CreateForm(Tmysql_edit_restricted_user, mysql_edit_restricted_user);
  Application.CreateForm(Tmysql_database_backup, mysql_database_backup);
  Application.CreateForm(Tmysql_database_restore, mysql_database_restore);
  Application.CreateForm(Tus_message_dlg, us_message_dlg);
  Application.Run;
end.
