unit us_server_state;

{#############################################################################
'# Name: us_server_state.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#
'#
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Dialogs,Forms,
  default_config_vars,us_common_functions,
  us_common_procedures,
  Graphics,
  RegExpr,FileUtil,
  ExtCtrls, LCLType;


//=== Update server state and indicators ===
procedure us_update_server_state;     // Update buttons/indicators

implementation

uses
    main_unit;

{*****************************************************************************
Display current server state
Updates indicators and buttons
Opens a CMD (dos) window.
=============================================================================}
procedure us_update_server_state;
Var
 AP : Boolean; // Apache running = true
 MY : Boolean; // MySQL running  = true

 sList                  :TStringList;  // String list
 i                      :integer;      // Loop counter
 selected_ini_file      :String;       // Full path selected php config file

 apc_code               :boolean; // Code exists in selected php ini file
 eaccelerator_code      :boolean; // Code exists in selected php ini file
 xcache_code            :boolean; // Code exists in selected php ini file
 zendopcache_code       :boolean; // Code exists in selected php ini file

 apc_file               :boolean; // File exists in selected php ini file
 eaccelerator_file      :boolean; // File exists in selected php ini file
 xcache_file            :boolean; // Filee exists in selected php ini file
 zendopcache_file       :boolean; // File exists in selected php ini file

begin

 //===Set button text

 //--MySQL MySQL/MariaDB button, menu and window Text
 Main.MM_mysql.Caption              := US_MYMAR_TXT;                              // MySQL
 Main.MMS_mysql_change_port.Caption := 'Change '+US_MYMAR_TXT+' port';            // Change MySQL port
 Main.MMS_mysql_change_pwd.Caption  := 'Change '+US_MYMAR_TXT+' password';        // Change MySQL root password
 Main.MMS_mysql_create_user.Caption := 'Create restricted '+US_MYMAR_TXT+' user'; // Create restricted MySQL user
 Main.MMS_mysql_edit_user.Caption   := 'Edit restricted '+US_MYMAR_TXT+' user';   // Edit restricted MySQL user
 Main.MMS_mysql_error_log.Caption   := US_MYMAR_TXT+' error log';                 // MySQL error log

 Main.GB_mysql_utilities.Caption    := US_MYMAR_TXT+' Utilities';                 // MySQL Utilities
 Main.Btn_mysql_console.Caption     := US_MYMAR_TXT + ' Console';                 // MySQL Console

 //--MySQL option buttons
 //-- Note: Btn_opt3 Default button phpMyBackupPro (Bottom-right)
 Main.Btn_opt1.Caption := US_OPT1_BTN_TXT; // Values:user set e.g phpMyAdmin
 Main.Btn_opt2.Caption := US_OPT2_BTN_TXT; // Values:user set e.g Adminer
 Main.Btn_opt3.Caption := US_OPT3_BTN_TXT; // Values:user set e.g phpMyBackupPro,Sypex Dumper

 If FileExists(US_DOCS+'\manual\index.html') Then
   Main.Btn_documentation.Enabled  := True     // Enable docs button
 Else
   Main.Btn_documentation.Enabled  := False;   // Disable docs  button

 //Get current server state
 AP := ApacheRunning(); //Set flag if this Apache server running
 MY := MysqlRunning();  //Set flag if this MySQL server running

 //--Apache server
 If AP then   //--AP running
   begin
     apache_indicator('green');                   // Server status running
     Main.Btn_start_apache.Caption  := STOP_AP;   // Set button text
     Main.Btn_view_www.Enabled      := True;      // Enable www button

     If AP_SSL_ENABLED Then Main.Btn_view_ssl.Enabled:= True
      Else Main.Btn_view_ssl.Enabled:= False;

     //Drop down menu buttons
     Main.MMS_apache_change_ports.Enabled        := False;  // Disable Change Apache ports button
     Main.MMS_apache_change_root_folder.Enabled  := False;  // Disable Change Apache root-folders button
     Main.MMS_apache_edit_configs.Enabled        := False;  // Disable edit Apache configuration files
     Main.MMS_apache_basic_and_modules.Enabled   := False;  // Disable edit Apache basic and module config
     //Main.MMS_folder_access_passwords.Enabled    := False;  // Disable access password config
     Main.MMS_apache_ssl.Enabled                 := False;  // Disable SSL keygen and SSL enable
     Main.MMS_apache_vhosts.Enabled              := False;  // Disable Apache Vhost config

     Main.MMS_apache_server_info_status.Enabled  := True;  // Enable Apache server info status button
     Main.MMS_apache_change_root_folder.Enabled  := False; // Disable root-folder button
     Main.MMSS_apache_delete_all_logs.Enabled    := False; // Disable Apache Log delete menu item

   end
 Else        //--AP Not running
  begin
     apache_indicator('red');                     // Server status stopped
     Main.Btn_start_apache.Caption  := START_AP;  // Set button text
     Main.Btn_view_www.Enabled      := False;     // Disable www button
     Main.Btn_view_ssl.Enabled      := False;     // Disable ssl button

     //Drop down menu buttons
     Main.MMS_apache_change_ports.Enabled        := True;  // Enable Change Apache ports button
     Main.MMS_apache_change_root_folder.Enabled  := True;  // Enable Change Apache root-folders button
     Main.MMS_apache_edit_configs.Enabled        := True;  // Enable edit Apache configuration files
     Main.MMS_apache_basic_and_modules.Enabled   := True;  // Enable edit Apache basic and module config
     //Main.MMS_folder_access_passwords.Enabled    := True;  // Enable access password config
     Main.MMS_apache_ssl.Enabled                 := True;  // Enable SSL keygen and SSL enable
     Main.MMS_apache_vhosts.Enabled              := True;  // Enable Apache Vhost config

     Main.MMS_apache_server_info_status.Enabled  := False; // Disable Apache server info status button
     Main.MMS_apache_change_root_folder.Enabled  := True;  // Enable root-folder button

     //Apache delete Logs menu item at least one file must exist
     If   FileExists(USF_APACHE_ERROR_LOG)
       Or FileExists(USF_APACHE_ACCESS_LOG)
       Or FileExists(USF_APACHE_SSL_ERROR_LOG)
       Or FileExists(USF_APACHE_SSL_ACCESS_LOG)
     Then
       Main.MMSS_apache_delete_all_logs.Enabled := True   // Enable Apache Log delete menu item
     Else
       Main.MMSS_apache_delete_all_logs.Enabled := False; // Disable Apache Log delete menu item
   End;


 //--MySQL server
 If MY then   //--My running
   begin
     mysql_indicator('green');                    // Server status running
     Main.Btn_start_mysql.Caption   := STOP_MY;   // Set button text
     Main.Btn_mysql_console.Enabled := True;      // Enable console button
   end
 Else        //--My NOT running
  begin
     mysql_indicator('red');                      // Server status stopped
     Main.Btn_start_mysql.Caption   := START_MY;  // Set button text
     Main.Btn_mysql_console.Enabled := False;     // Disable console button
  End;

  //--Apache and MySQL server running. PHP and phpMyAdmin (us_opt1) installed
  If (AP and MY and Not(UENV_PHP_SELECT = 'None') and DirectoryExists(US_OPT1) ) then
     Main.Btn_opt1.Enabled := True             // Enable opt1 (phpMyAdmin) button
  Else
    Main.Btn_opt1.Enabled := False;            // Disable opt1 (phpMyAdmin) button

  //--Apache and MySQL server running. PHP and us_opt3 (phpMyBackupPro) installed
  If (AP and MY and Not(UENV_PHP_SELECT = 'None') and DirectoryExists(US_OPT3) ) then
      Main.Btn_opt3.Enabled := True   // Enable opt3 (phpMyBackupPro / sypex) button
  Else
     Main.Btn_opt3.Enabled := False;  // Disable opt3 ,phpMyBackupPro / sypex) button

   //--Apache and MySQL server running. PHP and US_OPT2 (Adminer) installed
  If (AP and MY and Not(UENV_PHP_SELECT = 'None') and DirectoryExists(US_OPT2)) then
     Main.Btn_opt2.Enabled    := True        // Enable opt2 (Adminer) button
  Else
     Main.Btn_opt2.Enabled    := False;      // Disable opt2 (Adminer) button

  //=== Main Menu ==============================================

   //###===GENERAL===###


   //=== msmtp
   If DirectoryExists(US_MSMTP) Then
    begin
     Main.MMS_php_mail_msmtp.Enabled := true;
    end
   Else
     Main.MMS_php_mail_msmtp.Enabled := false;

    //=== Edit UniServer PAC file
   If FileExists(USF_PAC) Then
    begin
     Main.MMS_edit_uniformserver_pac.Enabled := true;
    end
   Else
     Main.MMS_edit_uniformserver_pac.Enabled := false;

   //===Display pages
   If USUC_DISPLAY_PAGE_1 = 'yes' Then
     Main.MMSS_display_at_startup_page1.Checked := True
   Else Main.MMSS_display_at_startup_page1.Checked := False;

   If USUC_DISPLAY_PAGE_2 = 'yes' Then
     Main.MMSS_display_at_startup_page2.Checked := True
   Else Main.MMSS_display_at_startup_page2.Checked := False;

   //###===Extra===###

    //=== Edit hosts file
    If FileExists(USF_EDHOST_UTILITY) Then
     begin
      Main.MMS_hosts.Enabled := true;
     end
    Else
     Main.MMS_hosts.Enabled := false; 


   //###===Apache===###

   //---Set Apache drop-down menu state
   If DirectoryExists(US_APACHE) Then
    begin
      Main.MM_apache.Enabled          := true;   // Enable drop-down menu
      Main.Btn_start_apache.Enabled   := true;   // Enable start/stop button


      //--Change button text opposite to that set in config.
      If SSL_Enabled Then
         Main.MMSS_enable_disable_ssl.Caption := Btn_text_disable_ssl //Disable SSL
      Else
         Main.MMSS_enable_disable_ssl.Caption := Btn_text_enable_ssl; //Enable SSL

      //--Apache log menu items
      If FileExists(USF_APACHE_ERROR_LOG)      Then
         Main.MMSS_apache_error_log.Enabled             := true   // Enable menu item
      Else
         Main.MMSS_apache_error_log.Enabled             := false; // Disable menu item

      If FileExists(USF_APACHE_ACCESS_LOG)     Then
         Main.MMSS_apache_access_log.Enabled            := true   // Enable menu item
      Else
         Main.MMSS_apache_access_log.Enabled            := false; // Disable menu item

      If FileExists(USF_APACHE_SSL_ERROR_LOG)  Then
                Main.MMSS_apache_ssl_error_log.Enabled  := true   // Enable menu item
      Else
         Main.MMSS_apache_ssl_error_log.Enabled         := false; // Disable menu item

      If FileExists(USF_APACHE_SSL_ACCESS_LOG) Then
                Main.MMSS_apache_ssl_access_log.Enabled := true   // Enable menu item
      Else
         Main.MMSS_apache_ssl_access_log.Enabled        := false; // Disable menu item

    end
   Else //-- No Apache
    begin
      Main.MM_apache.Enabled          := false; // Disable drop-down menu
      Main.Btn_start_apache.Enabled   := false; // Enable start/stop button


    end;


   //###===MySQL===###

   //---Set MySQL drop-down menu state
   If DirectoryExists(US_MYSQL) Then
    begin
      Main.MM_mysql.Enabled          := true;   // Enable drop-down menu
      Main.Btn_start_mysql.Enabled   := true;   // Enable start/stop button

      If MY then   //--My running
       begin
        Main.MMS_mysql_change_port.Enabled  := false;  // Disable change MySQL port button
        Main.MMS_mysql_change_pwd.Enabled   := false;  // Disable change MySQL password button
        Main.MMS_mysql_edit_config.Enabled  := false;  // Disable edit config button

        Main.MMS_mysql_database_create_delete.Enabled  := true;   // Enable create-delete db button
        Main.MMS_mysql_create_user.Enabled             := true;   // Enable create restricted user button
        Main.MMS_mysql_edit_user.Enabled               := true;   // Enable edit restricted user button
        Main.MMS_mysql_database_backup.Enabled         := true;   // Enable mysql database backup button
        Main.MMS_mysql_database_restore.Enabled        := true;   // Enable mysql database restore button
       end
      Else
      begin
        Main.MMS_mysql_change_port.Enabled  := true;  // Enable change MySQL port button
        Main.MMS_mysql_change_pwd.Enabled   := true;  // Enable change MySQL password button
        Main.MMS_mysql_edit_config.Enabled  := true;  // Enable edit config button

        Main.MMS_mysql_database_create_delete.Enabled  := false;  // Disable create-delete db button
        Main.MMS_mysql_create_user.Enabled             := false;  // Disable create restricted user button
        Main.MMS_mysql_edit_user.Enabled               := false;  // Disable edit restricted user button
        Main.MMS_mysql_database_backup.Enabled         := false;  // Disable mysql database backup button
        Main.MMS_mysql_database_restore.Enabled        := false;  // Disable mysql database restore button
      end;
    end
   Else //-- No MySQL
    begin
      Main.MM_mysql.Enabled        := false; // Disable drop-down menu
      Main.Btn_start_mysql.Enabled := false; // Enable start/stop button
    end;


   //###===PHP===###

      If AP then   //--AP running
       begin
        Main.MMS_select_php_version.Enabled       := false;  // Disable select  PHP version button
        Main.MMS_select_php_config.Enabled        := false;  // Disable select PHP config button
        Main.MMS_edit_selected_php_config.Enabled := false;  // Disable edit config button
        Main.MMS_php_basic_and_modules.Enabled    := false;  // Disable basic and module config button
        Main.MMS_php_accelerators.Enabled         := false;  // Disable accelerator selection button
       end
      Else
      begin
        Main.MMS_select_php_version.Enabled       := true;  // Enable select  PHP version button
        Main.MMS_select_php_config.Enabled        := true;  // Enable select PHP config button
        Main.MMS_edit_selected_php_config.Enabled := true;  // Enable edit config button
        Main.MMS_php_basic_and_modules.Enabled    := true;  // Enable basic and module config button
        Main.MMS_php_accelerators.Enabled         := true;  // Enable accelerator selection button
      end ;


   //---Set PHP drop-down menu state one or other must exist
   If (DirectoryExists(US_PHP56) or DirectoryExists(US_PHP70) or DirectoryExists(US_PHP71) or DirectoryExists(US_PHP72) or DirectoryExists(US_PHP73)) Then
    begin
      Main.MM_php.Enabled := true;  // Enable drop-down menu

      //--PHP 56
      If DirectoryExists(US_PHP56) Then
         Main.MMSS_php56.Enabled := true      // Select version
      Else                                    // Does not exist
         Main.MMSS_php56.Enabled := false;    // Select version
      //--END PHP 56


      //--PHP 70
      If DirectoryExists(US_PHP70) Then
         Main.MMSS_php70.Enabled := true      // Select version
      Else                                    // Does not exist
         Main.MMSS_php70.Enabled := false;    // Select version
      //--END PHP 70

      //--PHP 71
      If DirectoryExists(US_PHP71) Then
         Main.MMSS_php71.Enabled := true      // Select version
      Else                                    // Does not exist
         Main.MMSS_php71.Enabled := false;    // Select version
      //--END PHP 71

      //--PHP 72
      If DirectoryExists(US_PHP72) Then
         Main.MMSS_php72.Enabled := true      // Select version
      Else                                    // Does not exist
         Main.MMSS_php72.Enabled := false;    // Select version
      //--END PHP 71

      //--PHP 73
      If DirectoryExists(US_PHP73) Then
         Main.MMSS_php73.Enabled := true      // Select version
      Else                                    // Does not exist
         Main.MMSS_php73.Enabled := false;    // Select version
      //--END PHP 73

    end

   Else //-- No PHP
      Main.MM_php.Enabled := false; // Disable drop-down menu


   //---PHP Version selected - set checked

    //===PHP56
   If UENV_PHP_SELECT = 'php56' Then
    begin
      Main.MMSS_php56.Checked:=true;
      //php ini
      If FileExists(USF_PHP_INI_TEST_56) Then
         Main.MMSS_php_ini.Enabled:=true
      Else
         Main.MMSS_php_ini.Enabled:=false;

      //php development
      If FileExists(USF_PHP_INI_DEV_56) Then
         Main.MMSS_php_development.Enabled:=true
      Else
         Main.MMSS_php_development.Enabled:=false;

      //php production
      If FileExists(USF_PHP_INI_PROD_56) Then
         Main.MMSS_php_production.Enabled:=true
      Else
         Main.MMSS_php_production.Enabled:=false;
    end
   Else
      Main.MMSS_php56.Checked:=false;
   //===End PHP56


    //===PHP70
   If UENV_PHP_SELECT = 'php70' Then
    begin
      Main.MMSS_php70.Checked:=true;
      //php ini
      If FileExists(USF_PHP_INI_TEST_70) Then
         Main.MMSS_php_ini.Enabled:=true
      Else
         Main.MMSS_php_ini.Enabled:=false;

      //php development
      If FileExists(USF_PHP_INI_DEV_70) Then
         Main.MMSS_php_development.Enabled:=true
      Else
         Main.MMSS_php_development.Enabled:=false;

      //php production
      If FileExists(USF_PHP_INI_PROD_70) Then
         Main.MMSS_php_production.Enabled:=true
      Else
         Main.MMSS_php_production.Enabled:=false;
    end
   Else
      Main.MMSS_php70.Checked:=false;
   //===End PHP70

    //===PHP71
   If UENV_PHP_SELECT = 'php71' Then
    begin
      Main.MMSS_php71.Checked:=true;
      //php ini
      If FileExists(USF_PHP_INI_TEST_71) Then
         Main.MMSS_php_ini.Enabled:=true
      Else
         Main.MMSS_php_ini.Enabled:=false;

      //php development
      If FileExists(USF_PHP_INI_DEV_71) Then
         Main.MMSS_php_development.Enabled:=true
      Else
         Main.MMSS_php_development.Enabled:=false;

      //php production
      If FileExists(USF_PHP_INI_PROD_71) Then
         Main.MMSS_php_production.Enabled:=true
      Else
         Main.MMSS_php_production.Enabled:=false;
    end
   Else
      Main.MMSS_php71.Checked:=false;
   //===End PHP71

    //===PHP72
   If UENV_PHP_SELECT = 'php72' Then
    begin
      Main.MMSS_php72.Checked:=true;
      //php ini
      If FileExists(USF_PHP_INI_TEST_72) Then
         Main.MMSS_php_ini.Enabled:=true
      Else
         Main.MMSS_php_ini.Enabled:=false;

      //php development
      If FileExists(USF_PHP_INI_DEV_72) Then
         Main.MMSS_php_development.Enabled:=true
      Else
         Main.MMSS_php_development.Enabled:=false;

      //php production
      If FileExists(USF_PHP_INI_PROD_72) Then
         Main.MMSS_php_production.Enabled:=true
      Else
         Main.MMSS_php_production.Enabled:=false;
    end
   Else
      Main.MMSS_php72.Checked:=false;
   //===End PHP72

    //===PHP73
   If UENV_PHP_SELECT = 'php73' Then
    begin
      Main.MMSS_php73.Checked:=true;
      //php ini
      If FileExists(USF_PHP_INI_TEST_73) Then
         Main.MMSS_php_ini.Enabled:=true
      Else
         Main.MMSS_php_ini.Enabled:=false;

      //php development
      If FileExists(USF_PHP_INI_DEV_73) Then
         Main.MMSS_php_development.Enabled:=true
      Else
         Main.MMSS_php_development.Enabled:=false;

      //php production
      If FileExists(USF_PHP_INI_PROD_73) Then
         Main.MMSS_php_production.Enabled:=true
      Else
         Main.MMSS_php_production.Enabled:=false;
    end
   Else
      Main.MMSS_php73.Checked:=false;
   //===End PHP73

    //---PHP Information menu button.
    If AP and Not(UENV_PHP_SELECT = 'None') Then
      begin
        Main.MMS_view_phpinfo.Enabled  := True;     // Enable PHP info menu button
        If FileExists(USF_GO_PEAR_CONFIG) Then
          Main.MMS_pear_frontend.Enabled := True    // Enable PHP pear frontend button
        else
        Main.MMS_pear_frontend.Enabled := False;    // Disable PHP pear frontend button  
      end
    Else
      begin
       Main.MMS_view_phpinfo.Enabled  := False;     // Disable PHP info menu  button
       Main.MMS_pear_frontend.Enabled := False;     // Disable PHP pear frontend button
      end;

    //---PHP Edit selected php ini file button
    If UENV_PHP_SELECT = 'None' Then
       Main.MMS_edit_selected_php_config.Enabled := False //Disable button
    Else
      If NOT AP Then
       Main.MMS_edit_selected_php_config.Enabled := True; //Enable button

    //---PHP configuration file selected - set checked
    If UENV_PHP_INI_SELECT = 'php_test.ini' Then
       Main.MMSS_php_ini.Checked:=true
    Else
       Main.MMSS_php_ini.Checked:=false;

    If UENV_PHP_INI_SELECT = 'php_production.ini' Then
       Main.MMSS_php_production.Checked:=true
    Else
       Main.MMSS_php_production.Checked:=false;

    If UENV_PHP_INI_SELECT = 'php_development.ini' Then
       Main.MMSS_php_development.Checked:=true
    Else
       Main.MMSS_php_development.Checked:=false;

   //=== PHP Accelerators

   //--Check file exists in extensions folder
   If FileExists(US_CORE+'\'+UENV_PHP_SELECT+'\extensions\'+APC_DLL) Then
      apc_file:=true Else apc_file:=False;
   If FileExists(US_CORE+'\'+UENV_PHP_SELECT+'\extensions\'+EACCELERATOR_DLL) Then
      eaccelerator_file:=true Else eaccelerator_file:=False;
   If FileExists(US_CORE+'\'+UENV_PHP_SELECT+'\extensions\'+XCACHE_DLL) Then
      xcache_file:=true Else xcache_file:=False;
   If FileExists(US_CORE+'\'+UENV_PHP_SELECT+'\extensions\'+ZENDOPCACHE_DLL) Then
      zendopcache_file:=true Else zendopcache_file:=False;

   //--Check code exists in selected configuration file
   selected_ini_file :=US_CORE+'\'+UENV_PHP_SELECT+'\'+UENV_PHP_INI_SELECT;     // Full path selected php config file
   IF FileExists(selected_ini_file) Then
     begin
       //Assume false
       apc_code := False;
       eaccelerator_code := False;
       xcache_code := False;
       zendopcache_code := False;

       sList  := TStringList.Create;            // Create object
       sList.LoadFromFile(selected_ini_file);   // Load PHP ini configuration file
       //Scan sList
       for i:=0 to sList.Count-1 do
         begin
           // APC code
           If ExecRegExpr(QuoteRegExprMetaChars(APC_DLL), sList[i]) Then
            begin
              apc_code := True;
              If ExecRegExpr('^;.*$', sList[i]) Then
                 Main.MMSS_php_acc_apc.Checked := False
              Else
                 Main.MMSS_php_acc_apc.Checked := True;
            end;

           // Eaccelerator code
           If ExecRegExpr(QuoteRegExprMetaChars(EACCELERATOR_DLL), sList[i]) Then
           begin
             eaccelerator_code := True;
             If ExecRegExpr('^;.*$', sList[i]) Then
                Main.MMSS_php_acc_eaccelerator.Checked := False
             Else
                Main.MMSS_php_acc_eaccelerator.Checked := True;
           end;


           // Xcache code
           If ExecRegExpr(QuoteRegExprMetaChars(XCACHE_DLL), sList[i]) Then
           begin
             xcache_code := True;
             If ExecRegExpr('^;.*$', sList[i]) Then
                Main.MMSS_php_acc_xcache.Checked := False
             Else
                Main.MMSS_php_acc_xcache.Checked := True;
           end;


           // Zend OpCache code
           If ExecRegExpr(QuoteRegExprMetaChars(ZENDOPCACHE_DLL), sList[i]) Then
           begin
             zendopcache_code := True;
             If ExecRegExpr('^;.*$', sList[i]) Then
                Main.MMSS_php_acc_zop.Checked := False
             Else
                Main.MMSS_php_acc_zop.Checked := True;
           end

         end; //End scan list
      sList.Free;                       // Remove from memory
     end;

   //--Enable/Disable buttons
     // APC code
     If  apc_file and apc_code Then
       Main.MMSS_php_acc_apc.Enabled:=true
     Else
        Main.MMSS_php_acc_apc.Enabled:=false;

     // Eaccelerator code
     If  eaccelerator_file and eaccelerator_code Then
        Main.MMSS_php_acc_eaccelerator.Enabled:=true
     Else
        Main.MMSS_php_acc_eaccelerator.Enabled:=false;

     // Xcache code
     If  xcache_file and xcache_code Then
       Main.MMSS_php_acc_xcache.Enabled:=true
     Else
       Main.MMSS_php_acc_xcache.Enabled:=false;

     // APC code
     If  zendopcache_file and zendopcache_code Then
       Main.MMSS_php_acc_zop.Enabled:=true
     Else
       Main.MMSS_php_acc_zop.Enabled:=false;

   //---View PHP Accelerator control panel. Enable disable
   If (AP) And (Main.MMSS_php_acc_apc.Checked
     Or Main.MMSS_php_acc_eaccelerator.Checked
     Or Main.MMSS_php_acc_xcache.Checked
     Or Main.MMSS_php_acc_zop.Checked)
   Then
     Main.MMS_php_view_accelerator_control_panel.Enabled := True
   Else
     Main.MMS_php_view_accelerator_control_panel.Enabled := False;



   //###===PERL===###
   //---Set strawberry MM_perl drop-down menu state
   If DirectoryExists(US_PERL) Then
      Main.MM_perl.Enabled := true
   Else
      Main.MM_perl.Enabled := false;

   //--Perl view test page menu button
   If (AP) And (FileExists(US_CGI_BIN + '\test.pl')) Then
     Main.MMS_perl_view_test_pl.Enabled := True    // Enable button
   Else
     Main.MMS_perl_view_test_pl.Enabled := False;  // Disable button

end;
{----------------------------------------------------------------------------}
end.

