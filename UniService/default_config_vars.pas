unit default_config_vars;

{#############################################################################
'# Name: default_config_vars.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.0.6 25-04-2014
'# V1.1.0 7-06-2014 Added support for absolute and relative doc root paths
'# V2.3.0 2-10-2019 Removed Old PHPVersions and Added New Version - SudeepJD
'#
'# Default configuration for UniService
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Dialogs,RegExpr,INIFiles;

{global vars}

const
  Btn_install_service   = 'Install service';
  Btn_uninstall_service = 'Uninstall service';
  Btn_run_service       = 'Run service';
  Btn_stop_service      = 'Stop service';

Var

 //==== Lists of config/back full file paths.
 StrAllSource : TStringList; // List of config files
 StrAllBackup : TStringList; // List of backup config files

 //==== Server Config
 APACHE_SERVICE_NAME :String; //[SERVICE] ApacheServiceName=us_apache_1
 MYSQL_SERVICE_NAME  :String; //[SERVICE] MySQLServiceName =us_mysql_1

 APACHE_EXE_NAME     :String; //Apache exe name default httpd_z.exe
 MYSQL_EXE_NAME      :String; //MySQL exe name default httpd_z.exe

 //==== User Config
 UENV_AP_PORT         :string;  // AP_PORT        Apache port environment var
 UENV_AP_SSL_PORT     :string;  // AP_SSL_PORT    Apache SSL port environment var
 UENV_US_SERVERNAME   :string;  // US_SERVERNAME  Apache server name environment var
 UENV_US_ROOTF_WWW    :string;  // US_ROOTF_WWW   Apache server root folder www environment var
 UENV_US_ROOTF_SSL    :String;  // US_ROOTF_SSL   Apache server root folder ssl environment var
 UENV_PHP_SELECT      :string;  // PHP_SELECT     PHP Selected php70, php71, php72, php73, php74, php80 or php81 environment var
 UENV_PHP_INI_SELECT  :string;  // PHP_INI_SELECT PHP configuration file php_test.ini php_development.ini php_production.ini environment var
 US_MYMAR_TXT         :String;  // Display string MySQL or MariaDB - Depends on server installed

 //===  Top level folders =====
 UniConPath        :String;  // Path to this application back slashes
 UniConPath_F      :String;  // Path to this application forward slashes

 US_CORE           :String;  // Top core folder
 US_HOME           :String;  // Top home folder
 US_TMP            :String;  // Top temp folder

 //Sub-Folders
 US_CORE_SB        :String;  // Core service back-up folder
 US_APACHE         :String;  // Apache folder
 US_APACHE_BIN     :String;  // Apache binary folder

 US_MYSQL          :String;  // MYSQL folder
 US_MYSQL_BIN      :String;  // MySQL Binary folder

 US_PHP70          :String;  // PHP 70 root folder
 US_PHP71          :String;  // PHP 71 root folder
 US_PHP72          :String;  // PHP 72 root folder
 US_PHP73          :String;  // PHP 73 root folder
 US_PHP74          :String;  // PHP 74 root folder
 US_PHP80          :String;  // PHP 80 root folder
 US_PHP81          :String;  // PHP 81 root folder

 //== FILES ===

 //-- UniServer configuration
 USF_US_CONF_INI      : String; // Server configuration uicontroller
 USF_US_USER_INI      : String; // Server configuration user

 //-- MySQL
 USF_MYSQL_INI        :String;  //  MySQL configuration (MySQL default file - command line parameter)
 USF_MYMAR_TXT_INI    :String;  // MySQL or MariaDB configuration text file


 //-- PHP
 USF_PHP_INI              :String;   // PHP selected configuration file
 USF_PHP_CLI_INI          :String;   // PHP selected cli configuration file

 //-- PHP back-up
 USFB_PHP_INI             :String;   // PHP back-up selected configuration file
 USFB_PHP_CLI_INI         :String;   // PHP back-up selected cli configuration file

 //=== End Folders and Files

  procedure us_main_init;   // Set initial values for variables and paths.

  implementation

 uses
  us_common_functions,
  main_form;


{===============================================================
  This procedure sets all global variables to a known value.

  Set default folder and file paths

  Set server variables.   get values set in configuration file.
                          If file not found use defaults.

  Set user variables.     Get values set in user configuration file.
                          If file not found use defaults.
================================================================}

procedure us_main_init;
var
 Ini1:TINIFile;       // Handle for us_config.ini configuration file
 Ini2:TINIFile;       // Handle for us_user.ini configuration file
 Ini3:TINIFile;       // Handle for MySQL or MiraDB configuration file
 AppNumber: String;
 php_valid:boolean;   // PHP installed and version selected
begin

  //==== File array for source and backup
 StrAllSource := TStringList.Create; // Create List of config files
 StrAllBackup := TStringList.Create; // Create List of backup config files

 //=== Create Top level folder -root paths. Note: Last slash removed.
 UniConPath   := ParamStr(0);                                        // Full path including name
 UniConPath   := ExtractFilePath(UniConPath);                        // Get path to this application E:\UniServerZ\
 SetLength(UniConPath, Length(UniConPath) - 1);                      // Remove last character E:\UniServerZ
 UniConPath_F := StringReplace(UniConPath, '\','/',[rfReplaceAll]);  // Path e.g E:/UniServer

 //==Folders===

 //Top level folders
 US_CORE              := UniConPath +  '\core';              // Top core folder
 US_HOME              := UniConPath +  '\home';              // Top home folder
 US_TMP               := UniConPath +  '\tmp';               // Top temp folder


 //Sub-Folders
 US_CORE_SB         := UniConPath +  '\core\service_back';  // Core service back-up folder
 US_APACHE          := UniConPath +  '\core\apache2';       // Apache folder
 US_APACHE_BIN      := UniConPath +  '\core\apache2\bin';   // Apache binary folder

 US_MYSQL           := UniConPath +  '\core\mysql';         // MYSQL folder
 US_MYSQL_BIN       := UniConPath +  '\core\mysql\bin';     // MySQL Binary folder

  US_PHP70           := UniConPath +  '\core\php70';        // PHP 70 root folder
  US_PHP71           := UniConPath +  '\core\php71';        // PHP 71 root folder
  US_PHP72           := UniConPath +  '\core\php72';        // PHP 72 root folder
  US_PHP73           := UniConPath +  '\core\php73';        // PHP 73 root folder
  US_PHP74           := UniConPath +  '\core\php74';        // PHP 74 root folder
  US_PHP80           := UniConPath +  '\core\php80';        // PHP 80 root folder
  US_PHP81           := UniConPath +  '\core\php81';        // PHP 81 root folder

 //== Files ===

 //-- UniServer configuration
 USF_US_CONF_INI     := US_HOME + '\us_config\us_config.ini';        // Server configuration
 USF_US_USER_INI     := US_HOME + '\us_config\us_user.ini';          // Server configuration user

 //-- MySQL
 USF_MYMAR_TXT_INI   := US_MYSQL     + '\us_opt.ini';     // MySQL configuration text file
 
 //Set vars
 APACHE_EXE_NAME := us_get_apache_exe(); //Get Apache exe name
 MYSQL_EXE_NAME  := us_get_mysql_exe();  //Get MySQL exe name

 //=== Unicontroller configuration us_config.ini
   //==USF_US_CONF_INI - Default server configuration
   Ini1 := TINIFile.Create(USF_US_CONF_INI); // create object
   //*** Start ***
   //[APP]
   AppNumber := Ini1.ReadString('APP','AppNumber','1');                                 // Application number

   //[SERVICE]
   MYSQL_SERVICE_NAME    := Ini1.ReadString('SERVICE','MySQLServiceName','us_mysql');   //Service name
   APACHE_SERVICE_NAME   := Ini1.ReadString('SERVICE','ApacheServiceName','us_apache'); //Service name

   //-- MySQL command line parameters override
   //[MYSQL]
   USF_MYSQL_INI  := Ini1.ReadString('MYSQL','defaults-file', US_MYSQL + '\my.ini'); // MySQL configuration MySQL default file - command line parameter


   //*** End ***
   Ini1.Free;     // Free method of object
 //=== End  Unicontroller configuration us_config.ini

  MYSQL_SERVICE_NAME  := MYSQL_SERVICE_NAME  + '_' + AppNumber;  // Build service name
  APACHE_SERVICE_NAME := APACHE_SERVICE_NAME + '_' + AppNumber;  // Build service name

 //=== Unicontroller user configuration us_user.ini
    //==USF_US_USER_INI - User server configuration

   Ini2 := TINIFile.Create(USF_US_USER_INI); // create object
   //*** Start ***
   //[USER]
   UENV_PHP_SELECT      := Ini2.ReadString('USER','PHP_SELECT','php81');                   // Version selected by user
   UENV_PHP_INI_SELECT  := Ini2.ReadString('USER','PHP_INI_SELECT','php_production.ini');  // PHP configuration file selected by user
   UENV_AP_PORT         := Ini2.ReadString('USER','AP_PORT','80');                         // Apache port environment var
   UENV_AP_SSL_PORT     := Ini2.ReadString('USER','AP_SSL_PORT','443');                    // Apache ssl port environment var
   UENV_US_SERVERNAME   := Ini2.ReadString('USER','US_SERVERNAME','localhost');            // Server name
   UENV_US_ROOTF_WWW    := Ini2.ReadString('USER','US_ROOTF_WWW',UniConPath_F+'/www');     // Apache server root folder www environment var
   UENV_US_ROOTF_SSL    := Ini2.ReadString('USER','US_ROOTF_SSL',UniConPath_F+'/ssl');     // Apache server root folder ssl environment var

   //If folders php70, php71, php72, php73, php74, php80 and php81 do not exist or
   //incorrect PHP_SELECT value override user config. Set var PHP_SELECT to None.
   php_valid := False; // Assume PHP not installed
   If (DirectoryExists(US_PHP70) And (UENV_PHP_SELECT ='php70')) Then php_valid := True;
   If (DirectoryExists(US_PHP71) And (UENV_PHP_SELECT ='php71')) Then php_valid := True;
   If (DirectoryExists(US_PHP72) And (UENV_PHP_SELECT ='php72')) Then php_valid := True;
   If (DirectoryExists(US_PHP73) And (UENV_PHP_SELECT ='php73')) Then php_valid := True;
   If (DirectoryExists(US_PHP74) And (UENV_PHP_SELECT ='php74')) Then php_valid := True;
   If (DirectoryExists(US_PHP80) And (UENV_PHP_SELECT ='php80')) Then php_valid := True;
   If (DirectoryExists(US_PHP81) And (UENV_PHP_SELECT ='php81')) Then php_valid := True;
   If Not php_valid Then UENV_PHP_SELECT :='None';

   //Convert absolute/relative root paths to absolute paths.
   UENV_US_ROOTF_WWW := RelToAbsDir(UniConPath_F,UENV_US_ROOTF_WWW); //Convert to absolute
   UENV_US_ROOTF_SSL := RelToAbsDir(UniConPath_F,UENV_US_ROOTF_SSL); //Convert to absolute

   //*** End ***
   Ini2.Free;     // Free method of object
 //=== End Unicontroller configuration us_user.ini



 //=== MySQL or MariaDB configuration file us_opt.ini
    //==USF_MYMAR_TXT_INI - User server configuration

   Ini3 := TINIFile.Create(USF_MYMAR_TXT_INI); // create object
   //*** Start ***
   //[USER]
   US_MYMAR_TXT   := Ini3.ReadString('USER','text','MySQL'); //MySQL or MariaDB


   //*** End ***
   Ini3.Free;     // Free method of object
 //=== End MySQL or MariaDB configuration file us_opt.ini



  //==== Save file paths to string arrays

  //==Source files
  //-- Apache
   StrAllSource.Add(US_APACHE + '\conf\httpd.conf');                          // Apache configuration httpd.conf
   StrAllSource.Add(US_APACHE + '\conf\extra\httpd-autoindex.conf');          // Apache autoindex configuration httpd-autoindex.conf
   StrAllSource.Add(US_APACHE + '\conf\extra\httpd-dav.conf');                // Apache dav configuration httpd-dav.conf
   StrAllSource.Add(US_APACHE + '\conf\extra\httpd-manual.conf');             // Apache manual configuration httpd-manual.conf
   StrAllSource.Add(US_APACHE + '\conf\extra\httpd-multilang-errordoc.conf'); // Apache multi error configuration httpd-multilang-errordoc.conf
   StrAllSource.Add(US_APACHE + '\conf\extra\httpd-sni.conf');                // Apache sni configuration httpd-sni.conf
   StrAllSource.Add(US_APACHE + '\conf\extra\httpd-ssl.conf');                // Apache ssl configuration httpd-ssl.conf
   StrAllSource.Add(US_APACHE + '\conf\extra\httpd-userdir.conf');            // Apache user dir configuration httpd-userdir.conf
   StrAllSource.Add(US_APACHE + '\conf\extra\httpd-vhosts.conf');             // Apache VHosts configuration httpd-vhosts.conf

   //-- PHP
   If not (UENV_PHP_SELECT = 'None') Then //Add these paths if PHP installed
     begin
      StrAllSource.Add(US_CORE + '\' + UENV_PHP_SELECT + '\' + UENV_PHP_INI_SELECT);  // PHP selected configuration file
      StrAllSource.Add(US_CORE + '\' + UENV_PHP_SELECT + '\php-cli.ini');             // PHP selected cli configuration file
      StrAllSource.Add(US_APACHE + '\conf\extra_us\' + UENV_PHP_SELECT + '.conf')     // PHP Module Load File
   end;

   //==Backup files
   //-- Apache back-up
   StrAllBackup.Add(US_CORE_SB + '\httpd.conf');                    // Apache back-up configuration httpd.conf
   StrAllBackup.Add(US_CORE_SB + '\httpd-autoindex.conf');          // Apache back-up autoindex configuration httpd-autoindex.conf
   StrAllBackup.Add(US_CORE_SB + '\httpd-dav.conf');                // Apache back-up dav configuration httpd-dav.conf
   StrAllBackup.Add(US_CORE_SB + '\httpd-manual.conf');             // Apache back-up manual configuration httpd-manual.conf
   StrAllBackup.Add(US_CORE_SB + '\httpd-multilang-errordoc.conf'); // Apache back-up multi error configuration httpd-multilang-errordoc.conf
   StrAllBackup.Add(US_CORE_SB + '\httpd-sni.conf');                // Apache back-up sni configuration httpd-sni.conf
   StrAllBackup.Add(US_CORE_SB + '\httpd-ssl.conf');                // Apache back-up ssl configuration httpd-ssl.conf
   StrAllBackup.Add(US_CORE_SB + '\httpd-userdir.conf');            // Apache back-up user dir configuration httpd-userdir.conf
   StrAllBackup.Add(US_CORE_SB + '\httpd-vhosts.conf');             // Apache back-up VHosts configuration httpd-vhosts.conf

   //-- PHP back-up
   If not (UENV_PHP_SELECT = 'None') Then //Add these paths if PHP installed
     begin
      StrAllBackup.Add(US_CORE_SB + '\' + UENV_PHP_INI_SELECT); // PHP back-up selected configuration file
      StrAllBackup.Add(US_CORE_SB + '\php-cli.ini');            // PHP back-up selected cli configuration file
      StrAllBackup.Add(US_CORE_SB + '\' + UENV_PHP_SELECT + '.conf')     // PHP Module Load File
   end;
end;
{--- End us_main_init --------------------------------------}
end.
