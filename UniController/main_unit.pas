unit main_unit;

{#############################################################################
'# Name: main_unit.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#
'#
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus,
  default_config_vars,
  us_common_procedures,
  us_common_functions,
  are_servers_runable,
  about_form,
  windows,
  us_server_state,
  server_cert_key_gen_form,
  vhost_create_form,
  vhost_delete_form,
  apache_basic_form,
  apache_modules_form,
  root_www_pass_access_form,
  root_ssl_pass_access_form,
  php_extensions_form,
  edit_php_basic_form,
  msmtp_php_mail_form,
  server_internet_status_form,
  pc_win_startup_form,
  dtdns_form,
  cron_form,
  command_line_start_up,
  mysql_db_create_delete_form,
  mysql_create_restricted_user_form,
  mysql_edit_restricted_user_form,
  mysql_database_backup_form,
  mysql_database_restore_form;

type

  { TMain }

  TMain = class(TForm)
    Bevel1: TBevel;
    Btn_opt2: TButton;
    Btn_mysql_console: TButton;
    Btn_opt1: TButton;
    Btn_opt3: TButton;
    Btn_start_apache: TButton;
    Btn_start_mysql: TButton;
    Btn_server_console: TButton;
    Btn_view_www: TButton;
    Btn_view_ssl: TButton;
    Btn_documentation: TButton;
    GB2: TGroupBox;
    GB_mysql_utilities: TGroupBox;
    Image1: TImage;
    apache_img: TImage;
    menuImageList: TImageList;
    MenuItem1: TMenuItem;
    MenuItem17_spacer: TMenuItem;
    MenuItem19_spacer: TMenuItem;
    MenuItem18_spacer: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;

    MMSS_php56: TMenuItem;
    MMSS_php70: TMenuItem;
    MMSS_php71: TMenuItem;
    MMSS_php72: TMenuItem;
    MMSS_php73: TMenuItem;

    MMS_hosts: TMenuItem;
    MMS_select_portable_browser: TMenuItem;
    MMS_select_default_browser: TMenuItem;
    MMS_pear_frontend: TMenuItem;
    MMS_mysql_database_backup: TMenuItem;
    MMS_mysql_database_restore: TMenuItem;
    MenuItem5: TMenuItem;
    MMS_mysql_edit_user: TMenuItem;
    MMS_mysql_create_user: TMenuItem;
    MMS_mysql_database_create_delete: TMenuItem;
    MMS_mysql_error_log: TMenuItem;
    MenuItem20_spacer: TMenuItem;
    MMS_cron: TMenuItem;
    MMS_dtdns: TMenuItem;
    MMS_pc_win_startup: TMenuItem;
    MMS_clean_up: TMenuItem;
    MMSS_display_at_startup_page1: TMenuItem;
    MMSS_select_page_to_display1: TMenuItem;
    MenuItem_18_spacer: TMenuItem;
    MMSS_display_at_startup_page2: TMenuItem;
    MMSS_select_page_to_display2: TMenuItem;
    MMS_acache_start_up_pages: TMenuItem;
    MMS_server_internet_status: TMenuItem;
    MenuItem14_spacer: TMenuItem;
    MenuItem15_spacer: TMenuItem;
    MenuItem16_spacer: TMenuItem;
    MMS_php_mail_msmtp: TMenuItem;
    MMS_view_phpinfo: TMenuItem;
    MMSS_php_edit_basic: TMenuItem;
    MMSS_php_edit_modules: TMenuItem;
    MMS_php_basic_and_modules: TMenuItem;
    MMSS_folder_www_access_pass: TMenuItem;
    MenuItem13_spacer: TMenuItem;
    MMSS_folder_ssl_access_pass: TMenuItem;
    MMS_folder_access_passwords: TMenuItem;
    MMS_edit_uniformserver_pac: TMenuItem;
    MenuItem12_spacer: TMenuItem;
    MMSS_apache_error_log: TMenuItem;
    MMSS_apache_access_log: TMenuItem;
    MMSS_apache_ssl_error_log: TMenuItem;
    MenuItem11_spacer: TMenuItem;
    MMSS_apache_ssl_access_log: TMenuItem;
    MMSS_apache_delete_all_logs: TMenuItem;
    MMS_apache_logs: TMenuItem;
    MMSS_apache_edit_basic: TMenuItem;
    MMSS_apache_edit_modules: TMenuItem;
    MMS_apache_basic_and_modules: TMenuItem;
    MMSS_apache_edit_httpd_conf: TMenuItem;
    MMSS_apache_edit_ssl_conf: TMenuItem;
    MMS_apache_edit_configs: TMenuItem;
    MenuItem10_spacer: TMenuItem;
    MMS_apache_vhosts: TMenuItem;
    MMSS_apache_delete_vhost: TMenuItem;
    MMSS_apache_add_vhost: TMenuItem;
    MenuItem9_spacer: TMenuItem;
    MMSS_enable_disable_ssl: TMenuItem;
    MMS_apache_ssl: TMenuItem;
    MMSS_server_cert_key: TMenuItem;
    MMSS_apache_server_information: TMenuItem;
    MMSS_apache_server_status: TMenuItem;
    MMS_apache_server_info_status: TMenuItem;
    MMSS_apache_select_root_folder_www: TMenuItem;
    MMSS_apache_select_root_folder_ssl: TMenuItem;
    MMS_apache_change_root_folder: TMenuItem;
    MMSS_apache_change_port: TMenuItem;
    MMSS_apache_change_ssl_port: TMenuItem;
    MMS_apache_change_ports: TMenuItem;
    MenuItem8_spacer: TMenuItem;
    MMSS_php_acc_eaccelerator: TMenuItem;
    MMSS_php_acc_apc: TMenuItem;
    MMSS_php_acc_xcache: TMenuItem;
    MMSS_php_acc_zop: TMenuItem;
    MMS_php_accelerators: TMenuItem;
    MMS_php_view_accelerator_control_panel: TMenuItem;
    MMS_edit_selected_php_config: TMenuItem;
    MenuItem5_spacer: TMenuItem;
    MMS_mysql_restore_root_pwd: TMenuItem;
    MMS_mysql_change_pwd: TMenuItem;
    MMS_mysql_change_port: TMenuItem;
    MenuItem3_spacer: TMenuItem;
    MMSS_php_production: TMenuItem;
    MMSS_php_development: TMenuItem;
    MMSS_php_ini: TMenuItem;
    MenuItem1_spacer: TMenuItem;
    MMS_select_php_config: TMenuItem;
    MMS_select_php_version: TMenuItem;
    MMS_mysql_edit_config: TMenuItem;
    mysql_img: TImage;
    MainMenu1: TMainMenu;
    MMS_force_unix_shebang: TMenuItem;
    MMS_force_windows_shebang: TMenuItem;
    MMS_shebang_help: TMenuItem;
    MM_apache: TMenuItem;
    MM_general: TMenuItem;
    MMS_perl_view_test_pl: TMenuItem;
    MM_perl: TMenuItem;
    MM_php: TMenuItem;
    MM_mysql: TMenuItem;
    MM_about: TMenuItem;
    MMS_apache_syntax_check: TMenuItem;
    SystrayIcon: TTrayIcon;
    Timer1: TTimer;
    procedure Btn_mysql_consoleClick(Sender: TObject);
    procedure Btn_view_sslClick(Sender: TObject);
    procedure Btn_opt2Click(Sender: TObject);
    procedure Btn_opt3Click(Sender: TObject);
    procedure Btn_documentationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MMSS_display_at_startup_page1Click(Sender: TObject);
    procedure MMSS_display_at_startup_page2Click(Sender: TObject);
    procedure MMSS_php56Click(Sender: TObject);
    procedure MMSS_php70Click(Sender: TObject);
    procedure MMSS_php71Click(Sender: TObject);
    procedure MMSS_php72Click(Sender: TObject);
    procedure MMSS_php73Click(Sender: TObject);
    procedure MMSS_select_page_to_display1Click(Sender: TObject);
    procedure MMSS_select_page_to_display2Click(Sender: TObject);
    procedure MMSS_apache_access_logClick(Sender: TObject);
    procedure MMSS_apache_add_vhostClick(Sender: TObject);
    procedure MMSS_apache_delete_all_logsClick(Sender: TObject);
    procedure MMSS_apache_delete_vhostClick(Sender: TObject);
    procedure MMSS_apache_edit_basicClick(Sender: TObject);
    procedure MMSS_apache_edit_httpd_confClick(Sender: TObject);
    procedure MMSS_apache_edit_modulesClick(Sender: TObject);
    procedure MMSS_apache_edit_ssl_confClick(Sender: TObject);
    procedure MMSS_apache_error_logClick(Sender: TObject);
    procedure MMSS_apache_ssl_access_logClick(Sender: TObject);
    procedure MMSS_apache_ssl_error_logClick(Sender: TObject);
    procedure MMSS_enable_disable_sslClick(Sender: TObject);
    procedure MMSS_folder_ssl_access_passClick(Sender: TObject);
    procedure MMSS_folder_www_access_passClick(Sender: TObject);
    procedure MMSS_php_edit_basicClick(Sender: TObject);
    procedure MMSS_php_edit_modulesClick(Sender: TObject);
    procedure MMSS_server_cert_keyClick(Sender: TObject);
    procedure MMSS_apache_change_portClick(Sender: TObject);
    procedure MMSS_apache_change_ssl_portClick(Sender: TObject);
    procedure MMSS_apache_select_root_folder_sslClick(Sender: TObject);
    procedure MMSS_apache_select_root_folder_wwwClick(Sender: TObject);
    procedure MMSS_apache_server_informationClick(Sender: TObject);
    procedure MMSS_apache_server_statusClick(Sender: TObject);
    procedure MMSS_php_acc_apcClick(Sender: TObject);
    procedure MMSS_php_acc_eacceleratorClick(Sender: TObject);
    procedure MMSS_php_acc_xcacheClick(Sender: TObject);
    procedure MMSS_php_acc_zopClick(Sender: TObject);
    procedure MMSS_php_developmentClick(Sender: TObject);
    procedure MMSS_php_iniClick(Sender: TObject);
    procedure MMSS_php_productionClick(Sender: TObject);
    procedure MMS_apache_select_root_folder_sslClick(Sender: TObject);
    procedure MMS_apache_syntax_checkClick(Sender: TObject);
    procedure Btn_opt1Click(Sender: TObject);
    procedure Btn_start_apacheClick(Sender: TObject);
    procedure Btn_start_mysqlClick(Sender: TObject);
    procedure Btn_server_consoleClick(Sender: TObject);
    procedure Btn_view_wwwClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MMS_clean_upClick(Sender: TObject);
    procedure MMS_cronClick(Sender: TObject);
    procedure MMS_dtdnsClick(Sender: TObject);
    procedure MMS_edit_selected_php_configClick(Sender: TObject);
    procedure MMS_edit_uniformserver_pacClick(Sender: TObject);
    procedure MMS_force_unix_shebangClick(Sender: TObject);
    procedure MMS_force_windows_shebangClick(Sender: TObject);
    procedure MMS_hostsClick(Sender: TObject);
    procedure MMS_mysql_change_portClick(Sender: TObject);
    procedure MMS_mysql_change_pwdClick(Sender: TObject);
    procedure MMS_mysql_create_userClick(Sender: TObject);
    procedure MMS_mysql_database_backupClick(Sender: TObject);
    procedure MMS_mysql_database_create_deleteClick(Sender: TObject);
    procedure MMS_mysql_database_restoreClick(Sender: TObject);
    procedure MMS_mysql_edit_configClick(Sender: TObject);
    procedure MMS_mysql_edit_userClick(Sender: TObject);
    procedure MMS_mysql_error_logClick(Sender: TObject);
    procedure MMS_mysql_restore_root_pwdClick(Sender: TObject);
    procedure MMS_pc_win_startupClick(Sender: TObject);
    procedure MMS_pear_frontendClick(Sender: TObject);
    procedure MMS_perl_view_test_plClick(Sender: TObject);
    procedure MMS_php_mail_msmtpClick(Sender: TObject);
    procedure MMS_php_view_accelerator_control_panelClick(Sender: TObject);
    procedure MMS_server_internet_statusClick(Sender: TObject);
    procedure MMS_shebang_helpClick(Sender: TObject);
    procedure MMS_select_portable_browserClick(Sender: TObject);
    procedure MMS_select_default_browserClick(Sender: TObject);
    procedure MMS_view_phpinfoClick(Sender: TObject);
    procedure MM_aboutClick(Sender: TObject);
    procedure SystrayIconClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.lfm}
{$R unicon_images.rc}  // Add image resource for tray icon -> Added into the Project Resources

{ TMain }

procedure TMain.Btn_start_apacheClick(Sender: TObject);
var
  url:string; // Page url displayed in default browser
begin
   If (Main.Btn_start_apache.Caption  = START_AP) Then
    begin
     Btn_start_apache.Enabled := False; // Disable button
     us_start_apache_program;           // Start Apache server
     us_update_server_state;            // Update server state. Set button and indicator state

     //--Display user configured pages at start-up
     If USUC_DISPLAY_PAGE_1 = 'yes' Then
      begin
        url := 'http://' + UENV_US_SERVERNAME +':' + UENV_AP_PORT + USUC_PAGE_1;

        //Change url if PHP not enabled. Replace file index.php with ""
        If UENV_PHP_SELECT = 'None' Then
         begin
            url := StringReplace(url, 'index.php','',[rfReplaceAll]);  // Replace index.php with /
         end;

        browser_display_url(url); //Display URL in default/portable browser
      end;

      If USUC_DISPLAY_PAGE_2 = 'yes' Then
      begin
       url := 'http://' + UENV_US_SERVERNAME +':' + UENV_AP_PORT + USUC_PAGE_2;

       //Change url if PHP not enabled. Replace file index.php with ""
       If UENV_PHP_SELECT = 'None' Then
        begin
           url := StringReplace(url, 'index.php','',[rfReplaceAll]);  // Replace index.php with /
        end;

       browser_display_url(url); //Display URL in default/portable browser
      end;

    end
  Else
    begin
     us_kill_apache_program;  // Button Stop Apache - Kill Apache server
     us_update_server_state;  // Update server state. Set button and indicator state
    end;

end;

//Default button function is phpMyAdmin
procedure TMain.Btn_opt1Click(Sender: TObject);
var
  url :string;
begin
  url := 'http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/us_opt1/' + US_OPT1_PAGE;
  browser_display_url(url); //Display URL in default/portable browser
end;

procedure TMain.MMS_apache_syntax_checkClick(Sender: TObject);
begin
    us_apache_syntax_check; //Opens a CMD (DOS) window and displays syntax check result
end;

procedure TMain.FormWindowStateChange(Sender: TObject);
begin
 case WindowState of
   wsNormal:    us_update_server_state; // Update menus
   wsMinimized: begin
                If USC_TrayIconEnabled Then
                  begin
                    Main.Hide;                     // Hide
                    Main.WindowState := wsNormal;  // Set normal
                    Main.Hide;                     // Hide again
                    Main.ShowInTaskBar := stNever; // Remove from task bar
                  end;
                 us_update_server_state;           // Update menus
                end;
 end;
end;

procedure TMain.MMSS_display_at_startup_page1Click(Sender: TObject);
begin
  If Main.MMSS_display_at_startup_page1.Checked Then
   begin
     Main.MMSS_display_at_startup_page1.Checked := False; // Set to un-checked
     USUC_DISPLAY_PAGE_1 := 'no';                         // Do not display at start-up
   end
  Else
   begin
    Main.MMSS_display_at_startup_page1.Checked := True;   // Set to checked
    USUC_DISPLAY_PAGE_1 := 'yes';                         // Display at start-up
   end;
   us_ini_set(USF_US_USER_INI,'USER','DISPLAY_PAGE_1',USUC_DISPLAY_PAGE_1); // save to config file
  us_update_server_state;                                 // Update menus
  msg_restart_apache;                                     // If running display message
end;

procedure TMain.MMSS_display_at_startup_page2Click(Sender: TObject);
begin
    If Main.MMSS_display_at_startup_page2.Checked Then
   begin
     Main.MMSS_display_at_startup_page2.Checked := False;  // Set to un-checked
     USUC_DISPLAY_PAGE_2 := 'no';                          // Do not display at start-up
   end
  Else
   begin
    Main.MMSS_display_at_startup_page2.Checked := True;    // Set to checked
    USUC_DISPLAY_PAGE_2 := 'yes';                          // Display at start-up
   end;
  us_ini_set(USF_US_USER_INI,'USER','DISPLAY_PAGE_2',USUC_DISPLAY_PAGE_2); // save to config file
  us_update_server_state;                                                  // Update menus
  msg_restart_apache;                                                      // If running display message
end;

procedure TMain.MMSS_php56Click(Sender: TObject);
begin
 //Toggle action
 If Main.MMSS_php56.Checked Then
   begin
    UENV_PHP_SELECT :='None';                                            // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','None');              // save to config file
   end
 Else
   begin
    UENV_PHP_SELECT :='php56';                                           // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','php56');             // save to config file
   end;
 windows.SetEnvironmentVariable('PHP_SELECT',PCHAR(UENV_PHP_SELECT)); // PHP Select
 us_set_environment_path;  // Set new enironment paths
 us_update_server_state;   // Update menus
 msg_restart_apache;       // Reminder dialog - Restart apache.
 us_update_go_pear_config; // update paths in go-pear configuration file
end;

procedure TMain.MMSS_php70Click(Sender: TObject);
begin
 //Toggle action
 If Main.MMSS_php70.Checked Then
   begin
    UENV_PHP_SELECT :='None';                                            // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','None');              // save to config file
   end
 Else
   begin
    UENV_PHP_SELECT :='php70';                                           // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','php70');             // save to config file
   end;
 windows.SetEnvironmentVariable('PHP_SELECT',PCHAR(UENV_PHP_SELECT));   // PHP Select
 us_set_environment_path;  // Set new enironment paths
 us_update_server_state;   // Update menus
 msg_restart_apache;       // Reminder dialog - Restart apache.
 us_update_go_pear_config; // update paths in go-pear configuration file
end;

procedure TMain.MMSS_php71Click(Sender: TObject);
begin
 //Toggle action
 If Main.MMSS_php71.Checked Then
   begin
    UENV_PHP_SELECT :='None';                                            // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','None');              // save to config file
   end
 Else
   begin
    UENV_PHP_SELECT :='php71';                                           // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','php71');             // save to config file
   end;
 windows.SetEnvironmentVariable('PHP_SELECT',PCHAR(UENV_PHP_SELECT));   // PHP Select
 us_set_environment_path;  // Set new enironment paths
 us_update_server_state;   // Update menus
 msg_restart_apache;       // Reminder dialog - Restart apache.
 us_update_go_pear_config; // update paths in go-pear configuration file
end;

procedure TMain.MMSS_php72Click(Sender: TObject);
begin
 //Toggle action
 If Main.MMSS_php72.Checked Then
   begin
    UENV_PHP_SELECT :='None';                                            // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','None');              // save to config file
   end
 Else
   begin
    UENV_PHP_SELECT :='php72';                                           // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','php72');             // save to config file
   end;
 windows.SetEnvironmentVariable('PHP_SELECT',PCHAR(UENV_PHP_SELECT));   // PHP Select
 us_set_environment_path;  // Set new enironment paths
 us_update_server_state;   // Update menus
 msg_restart_apache;       // Reminder dialog - Restart apache.
 us_update_go_pear_config; // update paths in go-pear configuration file
end;

procedure TMain.MMSS_php73Click(Sender: TObject);
begin
 //Toggle action
 If Main.MMSS_php73.Checked Then
   begin
    UENV_PHP_SELECT :='None';                                            // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','None');              // save to config file
   end
 Else
   begin
    UENV_PHP_SELECT :='php73';                                           // Set var
    us_ini_set(USF_US_USER_INI,'USER','PHP_SELECT','php73');             // save to config file
   end;
 windows.SetEnvironmentVariable('PHP_SELECT',PCHAR(UENV_PHP_SELECT));   // PHP Select
 us_set_environment_path;  // Set new enironment paths
 us_update_server_state;   // Update menus
 msg_restart_apache;       // Reminder dialog - Restart apache.
 us_update_go_pear_config; // update paths in go-pear configuration file
end;

procedure TMain.MMSS_select_page_to_display1Click(Sender: TObject);
var
  page1:         string; // User selected page
begin
    page1:='';

  //Get a valid web page to display at start-up and save to config file
  If us_get_valid_web_page(page1) Then
   begin
     USUC_PAGE_1   := page1;                            // Set var
     us_ini_set(USF_US_USER_INI,'USER','PAGE_1',page1); // save to config file
     us_update_server_state;                            // Update menus
     us_MessageDlg('DtDNS Info','New PAGE was set', mtcustom,[mbOk],0); //Display information
   end
  Else
      us_MessageDlg('DtDNS Info','Cancel clicked: No action taken', mtcustom,[mbOk],0);

end;

procedure TMain.MMSS_select_page_to_display2Click(Sender: TObject);
var
  page2:         string; // User selected page
begin
    page2:='';

   //Get a valid web page to display at start-up and save to config file
   If us_get_valid_web_page(page2) Then
    begin
      USUC_PAGE_2   := page2;                            // Set var
      us_ini_set(USF_US_USER_INI,'USER','PAGE_2',page2); // save to config file
      us_update_server_state;                            // Update menus
       us_MessageDlg('DtDNS Info','New PAGE was set', mtcustom,[mbOk],0); //Display information
    end
   Else
     us_MessageDlg('DtDNS Info','Cancel clicked: No action taken', mtcustom,[mbOk],0);
end;

procedure TMain.MMSS_apache_access_logClick(Sender: TObject);
begin
  us_display_in_editor(USF_APACHE_ACCESS_LOG);
end;

procedure TMain.MMSS_apache_add_vhostClick(Sender: TObject);
begin
   vhost_create.ShowModal; //Dsplay vhost form
end;

procedure TMain.MMSS_apache_delete_all_logsClick(Sender: TObject);
begin
  // Delete logs
  If FileExists(USF_APACHE_ERROR_LOG)      Then
     DeleteFile(pchar(USF_APACHE_ERROR_LOG));
  If FileExists(USF_APACHE_ACCESS_LOG)     Then
     DeleteFile(pchar(USF_APACHE_ACCESS_LOG));
  If FileExists(USF_APACHE_SSL_ERROR_LOG)  Then
     DeleteFile(pchar(USF_APACHE_SSL_ERROR_LOG));
  If FileExists(USF_APACHE_SSL_ACCESS_LOG) Then
     DeleteFile(pchar(USF_APACHE_SSL_ACCESS_LOG));

  us_update_server_state; // Update menus
end;

procedure TMain.MMSS_apache_delete_vhostClick(Sender: TObject);
begin
     vhost_delete.ShowModal; //Display orm
end;

procedure TMain.MMSS_apache_edit_basicClick(Sender: TObject);
begin
  apache_basic.ShowModal;   //Dispay form
end;

procedure TMain.MMSS_apache_edit_httpd_confClick(Sender: TObject);
begin
    us_display_in_editor(USF_APACHE_CNF); //Display Apache coniguration file
end;

procedure TMain.MMSS_apache_edit_modulesClick(Sender: TObject);
begin
  apache_modules.ShowModal; //Display form
end;

procedure TMain.MMSS_apache_edit_ssl_confClick(Sender: TObject);
begin
    us_display_in_editor(USF_APACHE_SSL_CNF); //Display Apache SSL coniguration file
end;

procedure TMain.MMSS_apache_error_logClick(Sender: TObject);
begin
  us_display_in_editor(USF_APACHE_ERROR_LOG);
end;

procedure TMain.MMSS_apache_ssl_access_logClick(Sender: TObject);
begin
  us_display_in_editor(USF_APACHE_SSL_ACCESS_LOG);
end;

procedure TMain.MMSS_apache_ssl_error_logClick(Sender: TObject);
begin
  us_display_in_editor(USF_APACHE_SSL_ERROR_LOG);
end;

procedure TMain.MMSS_enable_disable_sslClick(Sender: TObject);
var
  str:String;
begin
  //Button action toggel.
  If  Main.MMSS_enable_disable_ssl.Caption = Btn_text_enable_ssl Then //Enable SSL
   begin
     //To enable SSL in Apache config file a server certificate must exist
     If FileExists(USF_CERT) Then
       us_enable_apache_ssl     // Enable Apache ssl in configuration file httpd.conf

     Else // Ask user if they wish to create a certificate
       begin
        // Server Certificate Not Found
        str := '';
        str := str +  'A server certificate was not found.' + sLineBreak + sLineBreak;
        str := str +  'Would you like to continue and '     + sLineBreak;
        str := str +  'generate a certificate and Key?';

        if us_MessageDlg('Server Certificate Not Found', str, mtConfirmation,[mbYes, mbNo],0) = mrYes then
             server_cert_key_gen.show; //Display certificate key generator form
        end;
   end
  Else //Disable SSL
     us_disable_apache_ssl; // Disable Apache ssl in configuration file httpd.conf

  //Update server state
  us_update_server_state;   // Update menus

  //Reminder
  msg_restart_apache;       // Check Apache runnning and inform user to restart Apache
end;

procedure TMain.MMSS_folder_ssl_access_passClick(Sender: TObject);
begin
  root_ssl_pass_access.ShowModal;
end;

procedure TMain.MMSS_folder_www_access_passClick(Sender: TObject);
begin
  root_www_pass_access.ShowModal;
end;

procedure TMain.MMSS_php_edit_basicClick(Sender: TObject);
begin
  edit_php_basic.ShowModal;
end;

procedure TMain.MMSS_php_edit_modulesClick(Sender: TObject);
begin
  php_extensions.Showmodal;
end;

procedure TMain.MMSS_server_cert_keyClick(Sender: TObject);
begin
    server_cert_key_gen.ShowModal;
end;


procedure TMain.MMSS_apache_change_portClick(Sender: TObject);
var
  old_apache_port:string;
  new_apache_port:string;
begin
   old_apache_port:= '';
   new_apache_port:= '';

 If us_get_new_port(UENV_AP_PORT,new_apache_port,'80') Then
   begin
     old_apache_port:= UENV_AP_PORT;                                  // Save old port value
     UENV_AP_PORT := new_apache_port;                                 // Set var
     us_ini_set(USF_US_USER_INI,'USER','AP_PORT',UENV_AP_PORT);       // save to config file
     windows.SetEnvironmentVariable('AP_PORT',PCHAR(UENV_AP_PORT));   // Set environoment variable
     us_proxy_file_port_update(old_apache_port,new_apache_port);      // Update PAC file with new port

     us_update_server_state;                                          // Update menus
     us_MessageDlg('Apache info','Port was changed', mtcustom,[mbOk],0) ; // Display message
     msg_restart_apache;                                              // If running display message
   end
 Else
    us_MessageDlg('Apache info','Cancel clicked: No action taken', mtcustom,[mbOk],0) ; //Display information

end;

procedure TMain.MMSS_apache_change_ssl_portClick(Sender: TObject);
var
  new_apache_ssl_port:string;
begin
   new_apache_ssl_port:= '';

 If us_get_new_port(UENV_AP_SSL_PORT,new_apache_ssl_port,'443') Then
   begin
     UENV_AP_SSL_PORT := new_apache_ssl_port;                               // Set var
     us_ini_set(USF_US_USER_INI,'USER','AP_SSL_PORT',UENV_AP_SSL_PORT);     // save to config file
     windows.SetEnvironmentVariable('AP_SSL_PORT',PCHAR(UENV_AP_SSL_PORT)); // Set environoment variable
     us_update_server_state;                                                // Update menus
     us_MessageDlg('Apache Info','Port was changed', mtcustom,[mbOk],0) ;       // Display message
     msg_restart_apache;                                                    // If running display message
   end
 Else
    us_MessageDlg('Apache Info','Cancel clicked: No action taken', mtcustom,[mbOk],0) ; //Display message

end;

procedure TMain.MMSS_apache_select_root_folder_sslClick(Sender: TObject);
var
  new_folder:string;
  folder_temp:string;
  drive1:string;
  drive2:string;
begin
    new_folder :='';   // Initial value
   If us_get_new_root_folder(new_folder,'ssl') Then
    begin
    //-----
    folder_temp := ''; //Set initial value
    new_folder := StringReplace(new_folder, '\','/',[rfReplaceAll]); // Replace back with forward slashes
    UENV_US_ROOTF_SSL := new_folder;                                 // Save full path

    //===Determine new value to save to configuration file.
    //Is selected folder in Uniform Server application folder
    If Pos(UniConPath_F,new_folder) = 1 then //New folder contains UniConPath_F
     Begin
       folder_temp :=  StringReplace(new_folder, UniConPath_F,'',[]); //Remove  UniConPath_F
       folder_temp := '.'+ folder_temp;                               //Add current folder marker '.'
     End
    Else
     begin
       //Is selected folder on the same drive as UniServerZ
       drive1 := ExtractFileDrive(new_folder);   //get drive e.g C:
       drive2 := ExtractFileDrive(UniConPath_F); //get drive e.g C:
       If (drive1 = drive2) Then                 //On same drive
        begin
          folder_temp := StringReplace(new_folder, drive1,'',[]); //Remove drive letter to give /.....
        end
        Else     //Not on the same drive
         begin
           folder_temp := new_folder; //Save folder unchanged
         end;
     end;
    us_ini_set(USF_US_USER_INI,'USER','US_ROOTF_SSL',folder_temp);            // save to config file
    USF_SSL_HTACCESS          :=  UENV_US_ROOTF_SSL+'\.htaccess';             // update path SSL Root htaccess file
    USF_SSL_HTACCESS_BACK     :=  UENV_US_ROOTF_SSL+'\.htaccess_back';        // update path SSL Root htaccess backup file
    windows.SetEnvironmentVariable('US_ROOTF_SSL',PCHAR(UENV_US_ROOTF_SSL));  // Set environoment variable

    us_MessageDlg('Apache Info','New Apache root-folder ssl set', mtcustom,[mbOk],0); //Display message
    us_update_server_state; // Update menus
    //----
    end
   Else
     us_MessageDlg('Apache Info','Cancel clicked: No action taken', mtcustom,[mbOk],0) ; //Display information message

end;

procedure TMain.MMSS_apache_select_root_folder_wwwClick(Sender: TObject);
var
  new_folder  :string;  // New path
  folder_temp :string;
  drive1 :string;
  drive2 :string;
begin
    new_folder :='';   //Initial value

  If us_get_new_root_folder(new_folder,'www') Then
   begin
   //---
   folder_temp := ''; //Set initial value
  new_folder := StringReplace(new_folder, '\','/',[rfReplaceAll]); // Replace back with forward slashes
  UENV_US_ROOTF_WWW := new_folder;                                 // Save full path

  //===Determine new value to save to configuration file.
  //Is selected folder in Uniform Server application folder
  If Pos(UniConPath_F,new_folder) = 1 then //New folder contains UniConPath_F
   Begin
     folder_temp :=  StringReplace(new_folder, UniConPath_F,'',[]); //Remove  UniConPath_F
     folder_temp := '.'+ folder_temp;                               //Add current folder marker '.'
   End
  Else
   begin
     //Is selected folder on the same drive as UniServerZ
     drive1 := ExtractFileDrive(new_folder);   //get drive e.g C:
     drive2 := ExtractFileDrive(UniConPath_F); //get drive e.g C:
     If (drive1 = drive2) Then                 //On same drive
      begin
        folder_temp := StringReplace(new_folder, drive1,'',[]); //Remove drive letter to give /.....
      end
      Else     //Not on the same drive
       begin
         folder_temp := new_folder; //Save folder unchanged
       end;
   end;
  us_ini_set(USF_US_USER_INI,'USER','US_ROOTF_WWW',folder_temp);            // save to config file
  USF_WWW_HTACCESS          :=  UENV_US_ROOTF_WWW+'\.htaccess';             // update path WWW Root htaccess file
  USF_WWW_HTACCESS_BACK     :=  UENV_US_ROOTF_WWW+'\.htaccess_back';        // update WWW Root htaccess backup file
  windows.SetEnvironmentVariable('US_ROOTF_WWW',PCHAR(UENV_US_ROOTF_WWW));  // Set environoment variable

  us_MessageDlg('Apache Info','New Apache root-folder www set', mtcustom,[mbOk],0); //Display message
  us_update_server_state; // Update menus

   //---
   end
  Else
    us_MessageDlg('Apache Info','Cancel clicked: No action taken', mtcustom,[mbOk],0); //Display message

end;

procedure TMain.MMSS_apache_server_informationClick(Sender: TObject);
var
  url :string;
begin
   url := 'http://'+ UENV_US_SERVERNAME +':'+ UENV_AP_PORT +'/server-info'; // Display Apache Server Information page
   browser_display_url(url);                                                // Display URL in default/portable browser
end;

procedure TMain.MMSS_apache_server_statusClick(Sender: TObject);
var
  url :string;
begin
  url := 'http://'+ UENV_US_SERVERNAME +':'+UENV_AP_PORT+'/server-status'; // Display Apache Server Status page
  browser_display_url(url);                                                // Display URL in default/portable browser
end;



procedure TMain.Btn_mysql_consoleClick(Sender: TObject);
begin
  us_display_mysql_prompt; // Display MySQL command console and MySQL prompt
end;

procedure TMain.Btn_view_sslClick(Sender: TObject);
var
  url:string;
begin
  url := 'https://' + UENV_US_SERVERNAME + ':' + UENV_AP_SSL_PORT + '/';
  browser_display_url(url); // Display URL in default/portable browser
end;

//Default button function is Adminer
procedure TMain.Btn_opt2Click(Sender: TObject);
var
  url:string;
begin
  url := 'http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/us_opt2/' + US_OPT2_PAGE;
  browser_display_url(url); // Display URL in default/portable browser
end;

//Default button function is phpMyBackupPro
procedure TMain.Btn_opt3Click(Sender: TObject);
var
  url:string;
begin
  url := 'http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/us_opt3/' + US_OPT3_PAGE;
  browser_display_url(url); //Display URL in default/portable browser
end;

procedure TMain.Btn_documentationClick(Sender: TObject);
 var
  url:string;
  PathFslash:string;
begin
  //URL Format eg: URL:='file:///G:/us_traymenu/docs/manual/index.html'

  PathFslash := StringReplace(US_DOCS, '\','/',[rfReplaceAll]);
  url        := 'file:///' + PathFslash + '/manual/index.html';
  browser_display_url(url);  // Display Server documentation page

end;

procedure TMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   //--Clean up
  sList_cron.Free;   // Remove from memory
end;

procedure TMain.MMSS_php_acc_apcClick(Sender: TObject);
begin
  disable_all_php_accelerators; // Disable accelerator

   //Check state of button
   If Main.MMSS_php_acc_apc.Checked = False Then
      enable_php_accelerator('apc');  // Enable apc accelerator

   us_update_server_state; // Set button and indicator state
   msg_restart_apache;     // Reminder dialog - Restart apache.
end;

procedure TMain.MMSS_php_acc_eacceleratorClick(Sender: TObject);
begin
   disable_all_php_accelerators; // Disable accelerator

    //Check state of button
    If Main.MMSS_php_acc_eaccelerator.Checked = False Then
      enable_php_accelerator('eaccelerator');  // Enable eaccelerator accelerator

    us_update_server_state; // Set button and indicator state
    msg_restart_apache;     // Reminder dialog - Restart apache.
end;

procedure TMain.MMSS_php_acc_xcacheClick(Sender: TObject);
begin
   disable_all_php_accelerators; // Disable accelerator

    //Check state of button
    If Main.MMSS_php_acc_xcache.Checked = False Then
      enable_php_accelerator('xcache');  // Enable xcache accelerator

    us_update_server_state; // Set button and indicator state
    msg_restart_apache;     // Reminder dialog - Restart apache.
end;

procedure TMain.MMSS_php_acc_zopClick(Sender: TObject);
begin
   disable_all_php_accelerators; // Disable accelerator

    //Check state of button
    If Main.MMSS_php_acc_zop.Checked = False Then
      enable_php_accelerator('zop');  // Enable Zend OpCache accelerator

    us_update_server_state; // Set button and indicator state
    msg_restart_apache;     // Reminder dialog - Restart apache.
end;

procedure TMain.MMSS_php_developmentClick(Sender: TObject);
begin
   UENV_PHP_INI_SELECT :='php_development.ini';                                 // Set var
   us_ini_set(USF_US_USER_INI,'USER','PHP_INI_SELECT','php_development.ini');   // save to config file
   windows.SetEnvironmentVariable('PHP_INI_SELECT',PCHAR(UENV_PHP_INI_SELECT)); // PHP Ini Select environoment
   us_update_server_state;                                                      // Update menus
   msg_restart_apache;     // Reminder dialog - Restart apache.
end;

procedure TMain.MMSS_php_iniClick(Sender: TObject);
begin
  UENV_PHP_INI_SELECT :='php_test.ini';                                        // Set var
  us_ini_set(USF_US_USER_INI,'USER','PHP_INI_SELECT','php_test.ini');          // save to config file
  windows.SetEnvironmentVariable('PHP_INI_SELECT',PCHAR(UENV_PHP_INI_SELECT)); // PHP Ini Select environoment
  us_update_server_state;                                                      // Update menus
  msg_restart_apache;     // Reminder dialog - Restart apache.
end;

procedure TMain.MMSS_php_productionClick(Sender: TObject);
begin
   UENV_PHP_INI_SELECT :='php_production.ini';                                  // Set var
   us_ini_set(USF_US_USER_INI,'USER','PHP_INI_SELECT','php_production.ini');    // save to config file
   windows.SetEnvironmentVariable('PHP_INI_SELECT',PCHAR(UENV_PHP_INI_SELECT)); // PHP Ini Select environoment
   us_update_server_state;                                                      // Update menus
   msg_restart_apache;     // Reminder dialog - Restart apache.
end;

procedure TMain.MMS_apache_select_root_folder_sslClick(Sender: TObject);
var
  new_folder:string;
  UENV_US_ROOTF_SSL_temp:string;  // Temp for Absolute path

begin
   new_folder :='';   //Initial value
   If us_get_new_root_folder(new_folder,'ssl') Then
    begin
      UENV_US_ROOTF_SSL := new_folder;                                               // Set new root folder var
      UENV_US_ROOTF_SSL := StringReplace(UENV_US_ROOTF_SSL, '\','/',[rfReplaceAll]); // Replace back with forward slashes
      UENV_US_ROOTF_SSL_temp := UENV_US_ROOTF_SSL;                                   // Save full path in _temp

      //Note If path selected is portable replace with environment variable %US_ROOTF% as appropriate
      UENV_US_ROOTF_SSL := StringReplace(UENV_US_ROOTF_SSL,UniConPath_F,'%US_ROOTF%',[rfReplaceAll]);

      us_ini_set(USF_US_USER_INI,'USER','US_ROOTF_SSL',UENV_US_ROOTF_SSL);     // save to config file
      windows.SetEnvironmentVariable('US_ROOTF_SSL',PCHAR(UENV_US_ROOTF_SSL)); // Set environoment variable

      //Update internal as if read from config file - expanded
      UENV_US_ROOTF_SSL := UENV_US_ROOTF_SSL_temp;    // Absolute path

      us_MessageDlg('Apache Info','New Apache root-folder ssl set', mtcustom,[mbOk],0) ; //Display message
      us_update_server_state;                         // Update menus

    end
   Else
     us_MessageDlg('Apache Info','Cancel clicked: No action taken', mtcustom,[mbOk],0) ; //Display message

end;

procedure TMain.Btn_start_mysqlClick(Sender: TObject);
begin
  If (Main.Btn_start_mysql.Caption  = START_MY) Then
     us_start_mysql_program       // Start MySQL server
  Else
     us_clean_stop_mysql_program; // Stop MySQL server

  us_update_server_state;         // Set button and indicator state
end;

procedure TMain.Btn_server_consoleClick(Sender: TObject);
begin
  us_display_server_console; // Display Console
end;

procedure TMain.Btn_view_wwwClick(Sender: TObject);
var
  url:string;
begin
  url := 'http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/';
  browser_display_url(url); // Display URL in default/portable browser
end;

procedure TMain.FormCreate(Sender: TObject);
var
 new_mysql_pwd:string;
begin
  new_mysql_pwd:='';
  Timer1.Enabled := False;          // Disable timer

If us_application_is_runable Then   // A draconian check. If a space found in path
                                    // or incorrect location UniController is terminated.
 Begin
  us_command_line_start_up;         // UniController started with parameters. Process and exit application

  us_main_init;                     // Set initial values for variables, paths and environment variables.
  us_update_server_state;           // Set button and indicator state
  us_update_go_pear_config;         // Update go-pear configuration file if it exists
  us_check_servers_are_runable;     // Warnings ports in use, ZoneAlarm running

   //---Set MySQL password if root password detected
   If DirectoryExists(US_MYSQL)           // Check MySQL installed
    and (us_get_mysql_password()= 'root') // Check new password not already set
    and (USC_NAG_USER)                    // Check user wants tobe reminded
    and MySqlPortFree                     // Check port is free to use
    and Not MySqlRunning                  // Cannot change password if running
   Then
    begin  // Set new root password or allow user to cancel
      If us_get_new_pwd(MY_PWD,new_mysql_pwd) Then   // Get new password. If set change password.
        mysql_change_root_password(new_mysql_pwd)    // Change existing password
      Else
         us_MessageDlg(US_MYMAR_TXT+' Info','Cancel clicked: No action taken', mtcustom,[mbOk],0); //Display information message
    end;

  us_set_icon_and_hover_text;      // Set icon displayed and hover text based on ServerType in config file

  //---TrayIcon - If enabled miniimise to TrayIcon
  If USC_TrayIconEnabled Then
   begin
     Main.visible:=False;                // Hide form
     Main.ShowInTaskBar := stNever;      // Hide from taskbar
     Application.ShowMainForm  := False; // Hide application
     SystrayIcon.Show;                   // Enable trayicon
   end;

  //--- If UniController not started by a PC-Win start-up
  //    then display main form.
  If (ParamCount = 0) Then
   begin
    Application.ShowMainForm  := True; // Show application
   end;

  //--- If UniController was started by a PC-Win start-up the following are
  //    automaticly started. Note: Parameter "pc_win_start" passed will initiate auto-run
  If (ParamCount >= 1) and (ParamStr(1) = 'pc_win_start') Then
   begin
     //showmessage('Auto run detected '+ParamStr(1));
     //== Items to run are defined in config file us_config.ini

     //Apache
     sleep(1000);
     If USC_RunApacheAtStartUp Then us_start_apache_program; // Start Apache

     //MySQL
     sleep(1000);
     If USC_RunMysqlAtStartUp Then us_start_mysql_program;   // Start MySQL

     //Update server state
     sleep(1000);
     us_update_server_state; // Set button and indicator state
   end;

  //OK to start timmed processes
  Timer1.Enabled := True;    // Enable timer
  end //us_application_is_runable
 else
  Application.Terminate;                         // Exit application
end;

procedure TMain.MMS_clean_upClick(Sender: TObject);
begin
  us_clean_up;     // Delete all files listed in config us_clean_up.ini
end;

procedure TMain.MMS_cronClick(Sender: TObject);
begin
  cron.ShowModal;  // Display Cron menu
end;

procedure TMain.MMS_dtdnsClick(Sender: TObject);
begin
  dtdns.ShowModal; // Display DtDns menu;
end;

procedure TMain.MMS_edit_selected_php_configClick(Sender: TObject);
begin
  //==Edit selected PHP configuration file

    //--PHP 56 selected
    If UENV_PHP_SELECT ='php56' then
      begin
       //Edit config php_test.ini
           If UENV_PHP_INI_SELECT ='php_test.ini' Then
              us_display_in_editor(USF_PHP_INI_TEST_56);

       //Edit config php_development.ini
           If UENV_PHP_INI_SELECT ='php_development.ini' Then
              us_display_in_editor(USF_PHP_INI_DEV_56);

       //Edit config php_production.ini
           If UENV_PHP_INI_SELECT ='php_production.ini' Then
               us_display_in_editor(USF_PHP_INI_PROD_56);
      end;
     //--End PHP 56 selected


    //--PHP 70 selected
    If UENV_PHP_SELECT ='php70' then
      begin
       //Edit config php_test.ini
           If UENV_PHP_INI_SELECT ='php_test.ini' Then
              us_display_in_editor(USF_PHP_INI_TEST_70);

       //Edit config php_development.ini
           If UENV_PHP_INI_SELECT ='php_development.ini' Then
              us_display_in_editor(USF_PHP_INI_DEV_70);

       //Edit config php_production.ini
           If UENV_PHP_INI_SELECT ='php_production.ini' Then
               us_display_in_editor(USF_PHP_INI_PROD_70);
      end;
     //--End PHP 70 selected

     //--PHP 71 selected
    If UENV_PHP_SELECT ='php71' then
      begin
       //Edit config php_test.ini
           If UENV_PHP_INI_SELECT ='php_test.ini' Then
              us_display_in_editor(USF_PHP_INI_TEST_71);

       //Edit config php_development.ini
           If UENV_PHP_INI_SELECT ='php_development.ini' Then
              us_display_in_editor(USF_PHP_INI_DEV_71);

       //Edit config php_production.ini
           If UENV_PHP_INI_SELECT ='php_production.ini' Then
               us_display_in_editor(USF_PHP_INI_PROD_71);
      end;
     //--End PHP 71 selected

      //--PHP 72 selected
    If UENV_PHP_SELECT ='php72' then
      begin
       //Edit config php_test.ini
           If UENV_PHP_INI_SELECT ='php_test.ini' Then
              us_display_in_editor(USF_PHP_INI_TEST_72);

       //Edit config php_development.ini
           If UENV_PHP_INI_SELECT ='php_development.ini' Then
              us_display_in_editor(USF_PHP_INI_DEV_72);

       //Edit config php_production.ini
           If UENV_PHP_INI_SELECT ='php_production.ini' Then
               us_display_in_editor(USF_PHP_INI_PROD_72);
      end;
     //--End PHP 72 selected

     //--PHP 73 selected
    If UENV_PHP_SELECT ='php73' then
      begin
       //Edit config php_test.ini
           If UENV_PHP_INI_SELECT ='php_test.ini' Then
              us_display_in_editor(USF_PHP_INI_TEST_73);

       //Edit config php_development.ini
           If UENV_PHP_INI_SELECT ='php_development.ini' Then
              us_display_in_editor(USF_PHP_INI_DEV_73);

       //Edit config php_production.ini
           If UENV_PHP_INI_SELECT ='php_production.ini' Then
               us_display_in_editor(USF_PHP_INI_PROD_73);
      end;
     //--End PHP 73 selected
end;

procedure TMain.MMS_edit_uniformserver_pacClick(Sender: TObject);
begin
  us_display_in_editor(USF_PAC);  //View Edit PAC file
end;

procedure TMain.MMS_force_unix_shebangClick(Sender: TObject);
begin
   force_shebang_update(US_UNIX_SHEBANG); // Update to Unix shebang in all perl scripts
   us_MessageDlg('Perl Info','Perl Shebangs have been updated', mtcustom,[mbOk],0) ; //Display information message
end;

procedure TMain.MMS_force_windows_shebangClick(Sender: TObject);
begin
 force_shebang_update(US_WINDOWS_SHEBANG); // Update to Windows shebang in all perl scripts
    us_MessageDlg('Perl Info','Perl Shebangs have been updated', mtcustom,[mbOk],0) ; //Display information message
end;

procedure TMain.MMS_hostsClick(Sender: TObject);
begin
  us_launch_edit_hosts_utility; // Run Uniform Server Utility
  Application.Minimize;         // Hide main application
end;

procedure TMain.MMS_mysql_change_portClick(Sender: TObject);
 var
   new_mysql_port:string;
begin
  new_mysql_port:= '';

  If MysqlRunning() Then
     msg_stop_mysql_port_change // Inform user to stop MySQL server
  Else                          // Not running change port
    begin
      If us_get_new_port(UENV_MYSQL_TCP_PORT,new_mysql_port,'3306') Then
       begin
         UENV_MYSQL_TCP_PORT := new_mysql_port;                                       // Set var
         us_ini_set(USF_US_USER_INI,'USER','MYSQL_TCP_PORT',UENV_MYSQL_TCP_PORT);     // save to config file
         windows.SetEnvironmentVariable('MYSQL_TCP_PORT',PCHAR(UENV_MYSQL_TCP_PORT)); // Set environoment variable
         us_update_server_state;                                                      // Update menus
         us_MessageDlg(US_MYMAR_TXT+' Info','Port was changed', mtcustom,[mbOk],0) ; //Display information message
       end
      Else
        us_MessageDlg(US_MYMAR_TXT+' Info','Cancel clicked: No action taken', mtcustom,[mbOk],0) ; //Display information message
    end;
end;

procedure TMain.MMS_mysql_change_pwdClick(Sender: TObject);
var
 new_mysql_pwd:string;        // New root password to create
begin
 new_mysql_pwd:= '';
 If us_get_new_pwd(MY_PWD,new_mysql_pwd) Then   // Get new password. If set change password.
   mysql_change_root_password(new_mysql_pwd)    // Change existing password
 Else
   us_MessageDlg(US_MYMAR_TXT+' Info','Cancel clicked: No action taken', mtcustom,[mbOk],0); //Display information message
end;

procedure TMain.MMS_mysql_create_userClick(Sender: TObject);
begin
    mysql_create_restricted_user.ShowModal;     //Display Create MySQL user;
end;

procedure TMain.MMS_mysql_database_backupClick(Sender: TObject);
begin
  mysql_database_backup.ShowModal;     //Display Database backup;
end;

procedure TMain.MMS_mysql_database_create_deleteClick(Sender: TObject);
begin
  mysql_db_create_delete.ShowModal;     //Display Database create-delete;
end;

procedure TMain.MMS_mysql_database_restoreClick(Sender: TObject);
begin
  mysql_database_restore.ShowModal;     //Display Database restore;
end;

procedure TMain.MMS_mysql_edit_configClick(Sender: TObject);
begin
    us_display_in_editor(USF_MYSQL_INI);
end;

procedure TMain.MMS_mysql_edit_userClick(Sender: TObject);
begin
  mysql_edit_restricted_user.ShowModal;     //Display Edit MySQL user;
end;

procedure TMain.MMS_mysql_error_logClick(Sender: TObject);
begin
  us_display_in_editor(USF_MYSQL_ERROR_LOG);
end;

procedure TMain.MMS_mysql_restore_root_pwdClick(Sender: TObject);
begin
   mysql_restore_root_password;     // Restore MySql root password
   us_update_server_state;
end;

procedure TMain.MMS_pc_win_startupClick(Sender: TObject);
begin
    pc_win_startup.ShowModal;     //Display PC-Win start-up
end;

procedure TMain.MMS_pear_frontendClick(Sender: TObject);
 var
  url :String;
begin
 url := 'http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/us_pear/index.php';
 browser_display_url(url); // Display URL in default/portable browser
end;

procedure TMain.MMS_perl_view_test_plClick(Sender: TObject);
var
  url :string;
begin
   url := 'http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/cgi-bin/test.pl';
   browser_display_url(url); // Display URL in default/portable browser
end;


procedure TMain.MMS_php_mail_msmtpClick(Sender: TObject);
begin
  msmtp_php_mail.ShowModal;
end;

procedure TMain.MMS_php_view_accelerator_control_panelClick(Sender: TObject);
begin
  view_selected_acc_control_panel;  // View selected accelerator, control pannel
end;

procedure TMain.MMS_server_internet_statusClick(Sender: TObject);

begin
   server_internet_status.ShowModal;  //Display Internet Status Window;
end;

procedure TMain.MMS_shebang_helpClick(Sender: TObject);
var
 title:string;
 str:string;
begin
  title     := 'Perl Shebang Help';
  str:='';
  str := str + 'Perl scripts developed on Unix will not run on a Window machine' + sLineBreak;
  str := str + 'the Shebang requires conversion to a Windows format.' + sLineBreak+ sLineBreak;

  str := str + 'The Shabang allows Apache to find the Perl interpreter ' + sLineBreak;
  str := str + 'Windows Shebang - First line of a Perl script:' + sLineBreak;
  str := str + '#!perl' + sLineBreak+ sLineBreak;

  str := str + 'Note 1: After installing third-party scripts to the cgi-bin folder. ' + sLineBreak;
  str := str + 'Force a Windows shebang update by running "Force Windows Shebang". ' + sLineBreak;
  str := str + 'from the Perl drop-down menu.' + sLineBreak+ sLineBreak;

  str := str + 'Note 2: Before uploading scripts from the cgi-bin folder to a Unix server. ' + sLineBreak;
  str := str + 'Force a Unix shebang update by running "Force Unix Shebang". ' + sLineBreak;
  str := str + 'from the Perl drop-down menu.' + sLineBreak;

  us_MessageDlg(title, str, mtInformation,[mbOk],0) ;  //Display message
end;

procedure TMain.MMS_select_portable_browserClick(Sender: TObject);
begin
 If Main.MMS_select_portable_browser.Checked Then
  begin
    Main.MMS_select_portable_browser.Checked := False;
    Main.MMS_select_default_browser.Checked  := True;
    PortableBrowser                          := False; // Portable browser flag
  end
 Else
  begin
   Main.MMS_select_portable_browser.Checked := True;
   Main.MMS_select_default_browser.Checked  := False;
   PortableBrowser                          := True; // Portable browser flag
  end;
end;

procedure TMain.MMS_select_default_browserClick(Sender: TObject);
begin
 If Main.MMS_select_default_browser.Checked Then
  begin
    Main.MMS_select_portable_browser.Checked := True;
    Main.MMS_select_default_browser.Checked  := False;
    PortableBrowser                          := True; // Portable browser flag
  end
 Else
  begin
   Main.MMS_select_portable_browser.Checked := False;
   Main.MMS_select_default_browser.Checked  := True;
   PortableBrowser                          := False; // Portable browser flag
  end;
 end;

procedure TMain.MMS_view_phpinfoClick(Sender: TObject);
var
 url :String;
begin
  url := 'http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/us_extra/phpinfo.php';
  browser_display_url(url); // Display URL in default/portable browser
end;

procedure TMain.MM_aboutClick(Sender: TObject);
begin
  about.ShowModal;                              //Display About;
end;

procedure TMain.SystrayIconClick(Sender: TObject);
begin
   Main.Show; //Show main form
end;


{====================================================================
This procedure periodically runs DtDNS update and Cron.
If either is disabled that section is skipped.
--------------------------------------------------------------------}
procedure TMain.Timer1Timer(Sender: TObject);
begin
 Timer1.Enabled := False; //Disable timer

 //--- Auto run DtDNS
 If DTDNS_auto_run Then dtdns_perform_update; // Run DtDNS updater

 //--- Auto run Cron
 If USC_enable_cron Then run_cron;            // Run Cron

 Timer1.Enabled := True;  //Enable timer
end;




end.

