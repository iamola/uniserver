unit default_config_vars;

{#############################################################################
'# Name: default_config_vars.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Dialogs,RegExpr,Windows,INIFiles;

{global vars}

const
  UNICONTROLLER_VERSION = 'v2.5.5';
  ZENDOPCACHE_DLL       = 'php_opcache.dll';

  Btn_text_enable_ssl   = 'Enable SSL (Currently Disabled)';
  Btn_text_disable_ssl  = 'Disable SSL (Currently Enabled)';
  VERSION_FILE_ADDRESS  = 'http://www.uniformserver.com/system/.version';

  //Portable Web browser Pale Moon
  PALE_MOONL_EXE           = 'Palemoon-Portable.exe'; // Portable Palemoon launcher start-up exe name
  PALE_MOON_EXE            = 'palemoon.exe' ;         // Palemoon execuitable name

Var

  //==== User Config and Enironment variables
   ORIGINAL_ENV_PATH         :string;  //Original environment path when controller started
   USUC_RUN_CONSOLE          :string;  //Run console yes/no
   USUC_DISPLAY_PAGE_1       :string;  //Display page 1 yes/no
   USUC_PAGE_1               :string;  //Page to display
   USUC_DISPLAY_PAGE_2       :string;  //Display page 2 yes/no
   USUC_PAGE_2               :string;  //Page to display
   USUC_USER_EDITOR          :string;  //Editor to use for displaying text pages default notepad.exe

   UENV_AP_PORT        :string;  // AP_PORT        Apache port environment var
   UENV_AP_SSL_PORT    :string;  // AP_SSL_PORT    Apache SSL port environment var
   UENV_US_SERVERNAME  :string;  // US_SERVERNAME  Apache server name environment var
   UENV_US_ROOTF_WWW   :string;  // US_ROOTF_WWW   Apache server root folder www environment var
   UENV_US_ROOTF_SSL   :String;  // US_ROOTF_SSL   Apache server root folder ssl environment var

   UENV_MYSQL_TCP_PORT :string;  // MYSQL_TCP_PORT MySQL port environment var
   UENV_PHP_SELECT     :string;  // PHP_SELECT     PHP Selected php70, php71, php72, php73, php74, php80, php81, php82, php83 environment var
   UENV_PHP_INI_SELECT :string;  // PHP_INI_SELECT PHP configuration file php_test.ini php_development.ini php_production.ini environment var

  //==== End User Config and Enironment variables

  //==== User Config button options
   US_OPT1_BTN_TXT     :string;   // button text - phpMyAdmin
   US_OPT1_PAGE        :string;   // page to display when button clicked
   US_OPT2_BTN_TXT     :string;   // button text - Adminer
   US_OPT2_PAGE        :string;   // page to display when button clicked
   US_OPT3_BTN_TXT     :string;   // button text - phpMyBackupPro
   US_OPT3_PAGE        :string;   // page to display when button clicked
   US_MYMAR_TXT        :string;   // Display string MySQL or MariaDB - Depends on server installed
  //==== User Config button options


  //==== DtDNS Config and variables
   DTDNS_domain_name_1    :string;  // DtDns Domain name 
   DTDNS_domain_name_2    :string;  // DtDns Domain name 
   DTDNS_domain_name_3    :string;  // DtDns Domain name 
   DTDNS_domain_name_4    :string;  // DtDns Domain name 
   DTDNS_domain_name_5    :string;  // DtDns Domain name 
   DTDNS_account_password :string;  // DtDns account password 
   DTDNS_auto_run         :Boolean; // Automatically run DtDNS update

   DTDNS_logging          :Boolean; // Loging enabled or disabled in config file
   DTDNS_time_tracker     :Integer; // Ref time

  //==== End DtDNS Config and variables



  //-- UniServer configuration

   //[App]
   USC_AppNumber       : String;  // Application number
   USC_AppVersion      : String;  // Version
   USC_ServerType      : String;  // Server Type: WAMP=Default APS=Apache standalone server MYS=MySQL  standalone server
   USC_ServerTypeText1 : String;  // TrayIcon hover text
   USC_ServerTypeText2 : String;  // TrayIcon hover text
   USC_TrayIconEnabled : Boolean; // TrayIcon Enable/Disable Default true

   //[PCSTARTUP]
   USC_RunAtPcStartUpEnabled : Boolean;  // Enables running server at PC start up:  true/false
   USC_RunApacheAtStartUp    : Boolean;  // Run Apache server at PC start-up: true/false
   USC_RunMysqlAtStartUp     : Boolean;  // Run MySQL server at PC start-up: true/false

   //[CRON]
   USC_enable_cron           : Boolean;  // Enables/Disable cron
   USC_cron_logging          : Boolean;  // Enables/Disable cron logging

   //[TIMERS]
   USC_AP_StartSafetyTime     : Integer; // Maximum wait time in seconds
   USC_AP_StopSafetyTime      : Integer; // Maximum wait time in seconds
   USC_MY_StartSafetyTime     : Integer; // Maximum wait time in seconds
   USC_MY_StopSafetyTime      : Integer; // Maximum wait time in seconds
   USC_PM_StartSafetyTime     : Integer; // Pale Moon Maximum wait time in seconds
   USC_PM_StopSafetyTime      : Integer; // Palse Moon Maximum wait time in seconds
   USC_PM_ReadySafetyTime     : Integer; // Palse Moon Maximum wait time to ready in seconds

   //[GENERAL]
   USC_NAG_USER               : Boolean; // Nag user. For example change password
   USC_ZA_CHECK               : Boolean; // ZoneAlarmCheck. If user wants to disables the ZA check.

   //[UNIQUE]
   UNIQUE_TEXT_IN_TITLE_BAR   : String;  // Reference HTML page title e.g UniServer Zero

   //[HOSTS]
   USC_EditHostsFileEnabled   : Boolean; // Enable/Disable Edit Windows hosts file true/false  Default true

   //[MYSQL]
   USP_TMPDIR                    : String;  // MySQL temp folder - command line parameter 
   USP_DATADIR                   : String;  // MySQL data folder - command line parameter 
   USP_INNODB_DATA_HOME_DIR      : String;  // MySQL innodb data folder - command line parameter 
   USP_INNODB_LOG_GROUP_HOME_DIR : String;  // MySQL innodb log folder - command line parameter 

   //--End UniServer configuration

   //Button text Toggle
   START_AP          : String;  // Start Apache
   STOP_AP           : String;  // Stop Apache
   START_MY          : String;  // Start MySQL
   STOP_MY           : String;  // Stop MySQL

 //-- Apache Server
 AP_EXE_NAME       :string;   // Apache exe 
 AP_SSL_ENABLED    :Boolean;  // Apache ssl enabled in httpd.conf config file

 //-- MySQL Server
 MY_EXE_NAME       :string;   // MySQL exe
 MY_PWD            :String;   // MySQL password from file
 US_DB_HOST        :String;   // MySQL host 127.0.0.1
 MY_SQL_VER        :String;   // MySQL version

 //===  Top level folders =====
 UniConPath        :String;  // Path to this application back slashes
 UniConPath_F      :String;  // Path to this application forward slashes

 US_DOCS              :string;  // Top documentaion folder
 US_CGI_BIN           :String;  // Top cgi-bin folder
 US_TMP               :String;  // Top temp folder
 US_UTILS             :String;  // Top utils folder
 US_CORE              :String;  // Top core folder
 US_HOME              :String;  // Top home folder
 US_HTPASSWD          :String;  // Top htpasswd folder
 US_OPENSSL           :String;  // Top openssl folder
 US_WWW               :String;  // Top server root folder www
 US_SSL               :String;  // Top server root folder ssl
 US_VHOSTS            :String;  // Top vhosts folder
 US_DB_BACKUP_RESTORE :String;  // Top MySQL backup folder

 //Sub-Folders
 US_APACHE         :String;  // Apache folder
 US_APACHE_BIN     :String;  // Apache binary folder
 US_APACHE_CERTS   :String;  // Apache certificates folder
 US_APACHE_MODULES :String;  // Apache modules

 US_MYSQL          :String;  // MYSQL folder
 US_MYSQL_BIN      :String;  // MySQL Binary folder

 US_PAC            :String;  // Proxy PAC folder
 US_PALE_MOON      :String;  // Pale Moon Folder

 US_PHP70          :String;  // PHP 70 root folder
 US_PHP71          :String;  // PHP 71 root folder
 US_PHP72          :String;  // PHP 71 root folder
 US_PHP73          :String;  // PHP 73 root folder
 US_PHP74          :String;  // PHP 74 root folder
 US_PHP80          :String;  // PHP 80 root folder
 US_PHP81          :String;  // PHP 81 root folder
 US_PHP82          :String;  // PHP 82 root folder
 US_PHP83          :String;  // PHP 83 root folder

 US_PHP70_EXT      :String;  // PHP 70 extensions folder
 US_PHP71_EXT      :String;  // PHP 71 extensions folder
 US_PHP72_EXT      :String;  // PHP 72 extensions folder
 US_PHP73_EXT      :String;  // PHP 73 extensions folder
 US_PHP74_EXT      :String;  // PHP 74 extensions folder
 US_PHP80_EXT      :String;  // PHP 80 extensions folder
 US_PHP81_EXT      :String;  // PHP 81 extensions folder
 US_PHP82_EXT      :String;  // PHP 82 extensions folder
 US_PHP83_EXT      :String;  // PHP 83 extensions folder

 US_PERL           :String;  // Perl main folder
 US_PERL_BIN       :String;  // Perl binary folder
 US_PERL_LIB       :String;  // Perl library folder

 US_MSMTP          :String;  // msmtp mail utility folder

 US_OPT1           :String;  // Default phpMyAdmin folder
 US_OPT2           :String;  // Default Adminer folder
 US_OPT3           :String;  // Default phpMyBackupPro / Sypex folder


 //== FILES ===

 //-- UniServer configuration
 USF_US_CONF_INI           : String; // Server configuration uicontroller
 USF_US_USER_INI           : String; // Server configuration user
 USF_US_CLEAN_UP_INI       : String; // Server configuration delete files and clean folders

 //-- Apache
 USF_APACHE_PID            : String; // Apache PID
 USF_APACHE_CNF            : String; // Apache configuration
 USF_APACHE_DEFAULT_CNF    : String; // Apache default configuration
 USF_APACHE_SSL_CNF        : String; // Apache ssl configuration
 USF_APACHE_VHOST_CNF      : String; // Apache VHosts configuration

 USF_APACHE_ERROR_LOG      : String; // Apache error log
 USF_APACHE_ACCESS_LOG     : String; // Apache access log
 USF_APACHE_SSL_ERROR_LOG  : String; // Apache SSL error log
 USF_APACHE_SSL_ACCESS_LOG : String; // Apache SSL access log

 USF_CERT                  : String; // Server certificate
 USF_CERT_CA               : String; // CA Server

 USF_VHOST_HTACCESS        : String; // .htaccess file for new Vhost
 USF_VHOST_ICO             : String; // favicon image for new Vhost

 //-- PASSWORDS htaccess FILES

 USF_WWW_PASSWORD          :String;  // WWW Root folder password file
 USF_SSL_PASSWORD          :String;  // SSL Root folder password file

 USF_WWW_HTACCESS          :String;  // WWW Root htaccess file
 USF_SSL_HTACCESS          :String;  // SSL Root htaccess file
 USF_WWW_HTACCESS_BACK     :String;  // WWW Root htaccess backup file
 USF_SSL_HTACCESS_BACK     :String;  // SSL Root htaccess backup file

 //-- MySQL
 USF_MYSQL_PID            : String;  // MySQL PID
 USF_MYSQL_INI            : String;  // MySQL configuration (MySQL default file - command line parameter)
 USF_MYSQL_INI_SKIP_GRANT : String;  // MySQL configuration skip grant tables
 USF_MYSQL_PWD            : String;  // MySQL password file
 USF_MYSQL_ERROR_LOG      : String;  // MySQL error log
 USF_MYSQL_TEMP_SQL       : String;  // MySQL temp sql code file
 USF_MYMAR_TXT_INI        : String;  // MySQL configuration text file

 //-- PHP

 USF_PHP_INI_TEST_70  :String;   // PHP 7.0.* configuration
 USF_PHP_INI_PROD_70  :String;   // PHP 7.0.* configuration production
 USF_PHP_INI_DEV_70   :String;   // PHP 7.0.* configuration development
 USF_PHP_INI_CLI_70   :String;   // PHP 7.0.* command line config

 USF_PHP_INI_TEST_71  :String;   // PHP 7.1.* configuration
 USF_PHP_INI_PROD_71  :String;   // PHP 7.1.* configuration production
 USF_PHP_INI_DEV_71   :String;   // PHP 7.1.* configuration development
 USF_PHP_INI_CLI_71   :String;   // PHP 7.1.* command line config

 USF_PHP_INI_TEST_72  :String;   // PHP 7.2.* configuration
 USF_PHP_INI_PROD_72  :String;   // PHP 7.2.* configuration production
 USF_PHP_INI_DEV_72   :String;   // PHP 7.2.* configuration development
 USF_PHP_INI_CLI_72   :String;   // PHP 7.2.* command line config

 USF_PHP_INI_TEST_73  :String;   // PHP 7.3.* configuration
 USF_PHP_INI_PROD_73  :String;   // PHP 7.3.* configuration production
 USF_PHP_INI_DEV_73   :String;   // PHP 7.3.* configuration development
 USF_PHP_INI_CLI_73   :String;   // PHP 7.3.* command line config

 USF_PHP_INI_TEST_74  :String;   // PHP 7.4.* configuration
 USF_PHP_INI_PROD_74  :String;   // PHP 7.4.* configuration production
 USF_PHP_INI_DEV_74   :String;   // PHP 7.4.* configuration development
 USF_PHP_INI_CLI_74   :String;   // PHP 7.4.* command line config

 USF_PHP_INI_TEST_80  :String;   // PHP 8.0.* configuration
 USF_PHP_INI_PROD_80  :String;   // PHP 8.0.* configuration production
 USF_PHP_INI_DEV_80   :String;   // PHP 8.0.* configuration development
 USF_PHP_INI_CLI_80   :String;   // PHP 8.0.* command line config

 USF_PHP_INI_TEST_81  :String;   // PHP 8.1.* configuration
 USF_PHP_INI_PROD_81  :String;   // PHP 8.1.* configuration production
 USF_PHP_INI_DEV_81   :String;   // PHP 8.1.* configuration development
 USF_PHP_INI_CLI_81   :String;   // PHP 8.1.* command line config

 USF_PHP_INI_TEST_82  :String;   // PHP 8.2.* configuration
 USF_PHP_INI_PROD_82  :String;   // PHP 8.2.* configuration production
 USF_PHP_INI_DEV_82   :String;   // PHP 8.2.* configuration development
 USF_PHP_INI_CLI_82   :String;   // PHP 8.2.* command line config

 USF_PHP_INI_TEST_83  :String;   // PHP 8.3.* configuration
 USF_PHP_INI_PROD_83  :String;   // PHP 8.3.* configuration production
 USF_PHP_INI_DEV_83   :String;   // PHP 8.3.* configuration development
 USF_PHP_INI_CLI_83   :String;   // PHP 8.3.* command line config

 //-- msmtp configuration
 USF_MSMTP_EXE          :String;  // MSMTP executable
 USF_MSMTP_INI          :String;  // MSMTP config file
 USF_MSMTP_LOG          :String;  // MSMTP log file
 USF_MSMTP_BAT          :String;  // MSMTP batch file
 USF_MSMTP_MESSAGE_TEMP :String;  // MSMTP message to be piped into command line

 //-- DtDNS
 USF_DTDNS_INI      :String;       // DTDNS Config
 USF_DTDNS_LOG      :String;       // DTDNS log

 //-- Cron
 USF_CRON_INI       :String;       // Full path to Cron configuration file
 USF_CRON_LOG       :String;       // CRON log

 //-- Option buttons
 USF_OPT1_INI       :String;       // Button option 1 config file
 USF_OPT2_INI       :String;       // Button option 2 config file 
 USF_OPT3_INI       :String;       // Button option 3 config file 

 //-- PAC 
 USF_PAC            :String;       // PAC file proxy.pac

 //-- HOSTS
 USF_EDHOST_UTILITY   :String;   // Path to Uniform Server EdHost utility
 USF_HOSTS            :String;   // Path and name of Windows hosts file
 USF_HOSTS_BACK       :string;   // Windows hosts file backup (path name)

 //-- User configuration file
 USF_USER_CONFIG_BAT  :String;   // User configuration batch file

 //-- Go Pear
 USF_GO_PEAR_CONFIG   :String;   // Go-pear configuration file

 //-- Web browser Pale Moon
 USF_PALE_MOON_EXE    :String;   // Fulle path Portable Palemoon launcher start-up exe
 USF_REFERENCE_HTML   :String;   // Full path to reference html file for start-up

 //=== End Folders and Files


 US_SHEBANG           :string;  // #!
 US_UNIX_SHEBANG      :string;  // #!/usr/bin/perl
 US_WINDOWS_SHEBANG   :string;  // #!perl

 //=== Pale Moon Global variables
 PortableBrowser     :boolean;       // Portable browser installed flag
 palemoon_ready_flag :boolean;       // PaleMoon ready flag

 //=== Default browser Global variables
 DEFAULT_BROWSER_EXE:String;         // Executable name of default browser
 default_browser_ready_flag:boolean; // Default browser ready flag

  procedure us_main_init;   // Set initial values for variables and paths.

  implementation

 uses
  us_common_functions,
  us_common_procedures,
  main_unit;

{===============================================================
  This procedure sets all global variables to a known value.

  Set default folder and file paths

  Set server variables.   get values set in configuration file.
                          If file not found use defaults.

  Set user variables.     Get values set in user configuration file.
                          If file not found use defaults.

  Set environment variables.  Get values set in user configuration file.
                              If file not found use defaults.
================================================================}

procedure us_main_init;
var
 sList: TStringList;                 // String list
 i: Integer;                         // Loop counter
 sDir : array[0..MAX_PATH] of char;  // Used for host file
 Ini1:TINIFile;                      // Handle for configuration file
 Ini2:TINIFile;                      // Handle for configuration file
 Ini3:TINIFile;                      // Handle for configuration file
 Ini4:TINIFile;                      // Handle for configuration file
 Ini5:TINIFile;                      // Handle for configuration file
 Ini6:TINIFile;                      // Handle for configuration file
 Ini7:TINIFile;                      // Handle for configuration file
 php_valid:   boolean;               // Temp var

begin
 //Button text
 START_AP            := 'Start Apache';  // Start Apache
 STOP_AP             := 'Stop Apache';   // Stop Apache


 //=== Create Top level folder -root paths. Note: Last slash removed.
 UniConPath   := ParamStr(0);                                        // Full path including name
 UniConPath   := ExtractFilePath(UniConPath);                        // Get path to this application E:\UniServerZ\
 SetLength(UniConPath, Length(UniConPath) - 1);                      // Remove last character E:\UniServerZ
 UniConPath_F := StringReplace(UniConPath, '\','/',[rfReplaceAll]);  // Path e.g E:/UniServer

 //==Folders===

  //Top level folders
  US_DOCS              := UniConPath +  '\docs';              // Top documentaion folder
  US_CGI_BIN           := UniConPath +  '\cgi-bin';           // Top cgi-bin folder
  US_TMP               := UniConPath +  '\tmp';               // Top temp folder
  US_UTILS             := UniConPath +  '\utils';             // Top utils folder
  US_CORE              := UniConPath +  '\core';              // Top core folder
  US_HOME              := UniConPath +  '\home';              // Top home folder
  US_HTPASSWD          := UniConPath +  '\htpasswd';          // Top htpasswd folder
  US_OPENSSL           := UniConPath +  '\core\openssl';      // Top openssl folder
  US_WWW               := UniConPath +  '\www';               // Top server root folder www
  US_SSL               := UniConPath +  '\ssl';               // Top server root folder ssl
  US_VHOSTS            := UniConPath +  '\vhosts';            // Top vhosts folder
  US_DB_BACKUP_RESTORE := UniConPath +  '\db_backup_restore'; // Top MySQL backup folder

  //Sub-Folders
  US_APACHE          := UniConPath +  '\core\apache2';              // Apache folder
  US_APACHE_BIN      := UniConPath +  '\core\apache2\bin';          // Apache binary folder
  US_APACHE_CERTS    := UniConPath +  '\core\apache2\server_certs'; // Apache certificates folder
  US_APACHE_MODULES  := UniConPath +  '\core\apache2\modules';      // Apache modules folder

  US_MYSQL           := UniConPath +  '\core\mysql';        // MYSQL folder
  US_MYSQL_BIN       := UniConPath +  '\core\mysql\bin';    // MySQL Binary folder

  US_PAC             := UniConPath +  '\home\us_pac';       // Proxy PAC folder
  US_PALE_MOON       := UniConPath +  '\core\palemoon';     // Pale Moon Folder

  US_PHP70           := UniConPath +  '\core\php70';        // PHP 70 root folder
  US_PHP71           := UniConPath +  '\core\php71';        // PHP 71 root folder
  US_PHP72           := UniConPath +  '\core\php72';        // PHP 72 root folder
  US_PHP73           := UniConPath +  '\core\php73';        // PHP 73 root folder
  US_PHP74           := UniConPath +  '\core\php74';        // PHP 73 root folder
  US_PHP80           := UniConPath +  '\core\php80';        // PHP 80 root folder
  US_PHP81           := UniConPath +  '\core\php81';        // PHP 81 root folder
  US_PHP82           := UniConPath +  '\core\php82';        // PHP 82 root folder
  US_PHP83           := UniConPath +  '\core\php83';        // PHP 83 root folder

  US_PHP70_EXT       := UniConPath +  '\core\php70\extensions'; // PHP 70 extensions folder
  US_PHP71_EXT       := UniConPath +  '\core\php71\extensions'; // PHP 71 extensions folder
  US_PHP72_EXT       := UniConPath +  '\core\php72\extensions'; // PHP 72 extensions folder
  US_PHP73_EXT       := UniConPath +  '\core\php73\extensions'; // PHP 73 extensions folder
  US_PHP74_EXT       := UniConPath +  '\core\php74\extensions'; // PHP 74 extensions folder
  US_PHP80_EXT       := UniConPath +  '\core\php80\extensions'; // PHP 80 extensions folder
  US_PHP81_EXT       := UniConPath +  '\core\php81\extensions'; // PHP 81 extensions folder
  US_PHP82_EXT       := UniConPath +  '\core\php82\extensions'; // PHP 82 extensions folder
  US_PHP83_EXT       := UniConPath +  '\core\php83\extensions'; // PHP 83 extensions folder

  US_PERL            := UniConPath +  '\core\perl';         // Perl main folder
  US_PERL_BIN        := UniConPath +  '\core\perl\bin';     // Perl binary folder
  US_PERL_LIB        := UniConPath +  '\core\perl\lib';     // Perl library

  US_MSMTP           := UniConPath +  '\core\msmtp';        // msmtp mail utility folder

  US_OPT1            := US_HOME +'\us_opt1';            // default phpMyAdmin folder
  US_OPT2            := US_HOME +'\us_opt2';            // default Adminer folder
  US_OPT3            := US_HOME +'\us_opt3';            // phpMyBackupPro / sypex folder


 //== FILES ===

  //-- UniServer configuration
  USF_US_CONF_INI           := US_HOME + '\us_config\us_config.ini';        // Server configuration 
  USF_US_USER_INI           := US_HOME + '\us_config\us_user.ini';          // Server configuration user
  USF_US_CLEAN_UP_INI       := US_HOME + '\us_config\us_clean_up.ini';      // Server configuration delete files and clean folders

  //-- Apache
  USF_APACHE_PID            := US_APACHE + '\logs\httpd.pid';                // Apache PID
  USF_APACHE_CNF            := US_APACHE + '\conf\httpd.conf';               // Apache configuration
  USF_APACHE_DEFAULT_CNF    := US_APACHE + '\conf\extra\httpd-default.conf'; // Apache default configuration
  USF_APACHE_SSL_CNF        := US_APACHE + '\conf\extra\httpd-ssl.conf';     // Apache ssl configuration
  USF_APACHE_VHOST_CNF      := US_APACHE + '\conf\extra\httpd-vhosts.conf';  // Apache VHosts configuration

  USF_APACHE_ERROR_LOG      := US_APACHE + '\logs\error.log';                // Apache error log
  USF_APACHE_ACCESS_LOG     := US_APACHE + '\logs\access.log';               // Apache access log
  USF_APACHE_SSL_ERROR_LOG  := US_APACHE + '\logs\error_ssl.log';            // Apache SSL error log
  USF_APACHE_SSL_ACCESS_LOG := US_APACHE + '\logs\access_ssl.log' ;          // Apache SSL access log

  USF_CERT                  := US_APACHE + '\server_certs\server.crt';       // Server certificate
  USF_CERT_CA               := US_APACHE + '\server_certs\ca.crt';           // CA Server

  USF_VHOST_HTACCESS        := US_HOME   + '\us_config\.htaccess';           // .htaccess for new Vhost
  USF_VHOST_ICO             := US_HOME   + '\us_config\favicon.ico';         // favicon image for new Vhost

  //-- PASSWORDS htaccess FILES
  USF_WWW_PASSWORD          :=  US_HTPASSWD + '\www\.htpasswd';             // WWW Root folder password file
  USF_SSL_PASSWORD          :=  US_HTPASSWD + '\ssl\.htpasswd';             // SSL Root folder password file

  //-- MySQL
  USF_MYSQL_PID            := US_MYSQL     + '\data\mysql.pid';            // MySQL PID
  USF_MYSQL_INI_SKIP_GRANT := US_MYSQL     + '\my_skip_grant.ini';         // MySQL configuration skip grant tables
  USF_MYSQL_PWD            := UniConPath   + '\htpasswd\mysql\passwd.txt'; // MySQL password file
  USF_MYSQL_ERROR_LOG      := US_MYSQL     + '\data\mysql.err';            // MySQL error log
  USF_MYSQL_TEMP_SQL       := US_MYSQL_BIN + '\mysql-init.txt';            // Full path to MySQL temp sql code file
  USF_MYMAR_TXT_INI        := US_MYSQL     + '\us_opt.ini';                // MySQL configuration text file

  //-- PHP

  USF_PHP_INI_TEST_70  := US_PHP70 + '\php_test.ini';        // PHP 7.0.* configuration
  USF_PHP_INI_PROD_70  := US_PHP70 + '\php_production.ini';  // PHP 7.0.* configuration production
  USF_PHP_INI_DEV_70   := US_PHP70 + '\php_development.ini'; // PHP 7.0.* configuration development
  USF_PHP_INI_CLI_70   := US_PHP70 + '\php-cli.ini';         // PHP 7.0.* command line config

  USF_PHP_INI_TEST_71  := US_PHP71 + '\php_test.ini';        // PHP 7.1.* configuration
  USF_PHP_INI_PROD_71  := US_PHP71 + '\php_production.ini';  // PHP 7.1.* configuration production
  USF_PHP_INI_DEV_71   := US_PHP71 + '\php_development.ini'; // PHP 7.1.* configuration development
  USF_PHP_INI_CLI_71   := US_PHP71 + '\php-cli.ini';         // PHP 7.1.* command line config

  USF_PHP_INI_TEST_72  := US_PHP72 + '\php_test.ini';        // PHP 7.2.* configuration
  USF_PHP_INI_PROD_72  := US_PHP72 + '\php_production.ini';  // PHP 7.2.* configuration production
  USF_PHP_INI_DEV_72   := US_PHP72 + '\php_development.ini'; // PHP 7.2.* configuration development
  USF_PHP_INI_CLI_72   := US_PHP72 + '\php-cli.ini';         // PHP 7.2.* command line config

  USF_PHP_INI_TEST_73  := US_PHP73 + '\php_test.ini';        // PHP 7.3.* configuration
  USF_PHP_INI_PROD_73  := US_PHP73 + '\php_production.ini';  // PHP 7.3.* configuration production
  USF_PHP_INI_DEV_73   := US_PHP73 + '\php_development.ini'; // PHP 7.3.* configuration development
  USF_PHP_INI_CLI_73   := US_PHP73 + '\php-cli.ini';         // PHP 7.3.* command line config

  USF_PHP_INI_TEST_74  := US_PHP74 + '\php_test.ini';        // PHP 7.4.* configuration
  USF_PHP_INI_PROD_74  := US_PHP74 + '\php_production.ini';  // PHP 7.4.* configuration production
  USF_PHP_INI_DEV_74   := US_PHP74 + '\php_development.ini'; // PHP 7.4.* configuration development
  USF_PHP_INI_CLI_74   := US_PHP74 + '\php-cli.ini';         // PHP 7.4.* command line config

  USF_PHP_INI_TEST_80  := US_PHP80 + '\php_test.ini';        // PHP 8.0.* configuration
  USF_PHP_INI_PROD_80  := US_PHP80 + '\php_production.ini';  // PHP 8.0.* configuration production
  USF_PHP_INI_DEV_80   := US_PHP80 + '\php_development.ini'; // PHP 8.0.* configuration development
  USF_PHP_INI_CLI_80   := US_PHP80 + '\php-cli.ini';         // PHP 8.0.* command line config

  USF_PHP_INI_TEST_81  := US_PHP81 + '\php_test.ini';        // PHP 8.1.* configuration
  USF_PHP_INI_PROD_81  := US_PHP81 + '\php_production.ini';  // PHP 8.1.* configuration production
  USF_PHP_INI_DEV_81   := US_PHP81 + '\php_development.ini'; // PHP 8.1.* configuration development
  USF_PHP_INI_CLI_81   := US_PHP81 + '\php-cli.ini';         // PHP 8.1.* command line config

  USF_PHP_INI_TEST_82  := US_PHP82 + '\php_test.ini';        // PHP 8.2.* configuration
  USF_PHP_INI_PROD_82  := US_PHP82 + '\php_production.ini';  // PHP 8.2.* configuration production
  USF_PHP_INI_DEV_82   := US_PHP82 + '\php_development.ini'; // PHP 8.2.* configuration development
  USF_PHP_INI_CLI_82   := US_PHP82 + '\php-cli.ini';         // PHP 8.2.* command line config

  USF_PHP_INI_TEST_83  := US_PHP83 + '\php_test.ini';        // PHP 8.3.* configuration
  USF_PHP_INI_PROD_83  := US_PHP83 + '\php_production.ini';  // PHP 8.3.* configuration production
  USF_PHP_INI_DEV_83   := US_PHP83 + '\php_development.ini'; // PHP 8.3.* configuration development
  USF_PHP_INI_CLI_83   := US_PHP83 + '\php-cli.ini';         // PHP 8.3.* command line config

  //-- msmtp configuration
  USF_MSMTP_EXE          := US_MSMTP + '\msmtp.exe';            // MSMTP executable
  USF_MSMTP_INI          := US_MSMTP + '\msmtprc.ini';          // MSMTP config file
  USF_MSMTP_LOG          := US_MSMTP + '\msmtp.log';            // MSMTP log file
  USF_MSMTP_BAT          := US_MSMTP + '\Send_test_email.bat';  // MSMTP batch file
  USF_MSMTP_MESSAGE_TEMP := US_MSMTP + '\message_temp.txt';     // MSMTP Message to be piped. Temp file

  //-- DtDNS
  USF_DTDNS_INI          := US_HOME + '\us_dtdns\dtdns.ini';    // DTDNS Config
  USF_DTDNS_LOG          := US_HOME + '\us_dtdns\dtdns.log';    // DTDNS log

  //-- Cron
  USF_CRON_INI           := US_HOME + '\us_cron\cron.ini';      // Full path to Cron configuration file
  USF_CRON_LOG           := US_HOME + '\us_cron\cron.log';      // CRON log

 //-- Option buttons
 USF_OPT1_INI    := US_HOME + '\us_opt1\us_opt.ini';      // Button option 1 config file
 USF_OPT2_INI    := US_HOME + '\us_opt2\us_opt.ini';      // Button option 2 config file 
 USF_OPT3_INI    := US_HOME + '\us_opt3\us_opt.ini';      // Button option 3 config file 

 //-- PAC 
 USF_PAC         := US_PAC + '\proxy.pac';                // PAC file


  //-- User configuration file
  USF_USER_CONFIG_BAT := US_UTILS + '\user_configuration.bat';    // User configuration batch file

  //-- Go Pear
  USF_GO_PEAR_CONFIG  := US_HOME + '\us_pear\pear.ini'; // Go-pear configuration file

  //-- Web browser Pale Moon
  USF_PALE_MOON_EXE   := US_CORE +'\palemoon\' + PALE_MOONL_EXE;           // Portable Palemoon launcher start-up exe
  USF_REFERENCE_HTML  := UniConPath_F +'/home/us_splash/us_zero_ref.html'; // Reference html file for browser start-up

  //-- HOSTS - get hosts file path
  USF_EDHOST_UTILITY := US_UTILS + '\EdHost.exe';         // Path to Uniform Server EdHost utility
  Windows.GetSystemDirectory(sDir,MAX_PATH);              // Get system folder
  StrPas(sDir);                                           // Convert to string
  USF_HOSTS       := sDir+ '\drivers\etc\hosts';          // Create full path of Windows hosts file
  USF_HOSTS_BACK  := sDir+ '\drivers\etc\us_back_hosts';  // Backup host file

  //===COMMON VARS =====
 
  //-- Set EXE names. Get name directely from bin folders
  AP_EXE_NAME := us_get_apache_exe(); //Set Apache exe name
  MY_EXE_NAME := us_get_mysql_exe() ; //Set MySQL exe name


  //-- Set ssl state configured in Apache config file
  AP_SSL_ENABLED := False; // Assume disabled

  If FileExists(USF_APACHE_CNF) Then
    begin
      sList := TStringList.Create;           // Create object
      sList.LoadFromFile(USF_APACHE_CNF);    // Load file
      for i:=0 to sList.Count-1 do           // Scan file line by line
        begin
         if (sList[i]<>'') and ExecRegExpr('^LoadModule ssl_module modules/mod_ssl.so', sList[i]) then // Match found
           begin
             AP_SSL_ENABLED := True;         // Is enabled
             Break;                          // Nothing else to do
           end;
        end;
      sList.Free;     // remove from memory
    end;


  //-- MySQL Server
  MY_PWD            := us_get_mysql_password(); // MySQL password from file
  US_DB_HOST        := 'localhost';             // MySQL host 127.0.0.1

  //=== Perl Shebang
  US_SHEBANG             := '#!';               // #!
  US_UNIX_SHEBANG        := '#!/usr/bin/perl';  // #!/usr/bin/perl
  US_WINDOWS_SHEBANG     := '#!perl';           // #!perl

  //=== Pale Moon Global variables
  If DirectoryExists(US_PALE_MOON) Then //Portable browser
   begin
    Main.MMS_select_portable_browser.Checked := True;
    Main.MMS_select_default_browser.Checked  := False;
    Main.MMS_select_portable_browser.Enabled := True;
    Main.MMS_select_default_browser.Enabled  := True;
    PortableBrowser                          := True; // Portable browser installed
   end
  Else //Default browser
   begin
    Main.MMS_select_portable_browser.Checked := False;
    Main.MMS_select_default_browser.Checked  := True;
    Main.MMS_select_portable_browser.Enabled := False;
    Main.MMS_select_default_browser.Enabled  := False;
    PortableBrowser                          := False; // Portable browser not installed
   end;
 //--Browser


  palemoon_ready_flag:=False;         // PaleMoon ready flag set initial value
 

 //===End Pale Moon

  //=== Default browser Global variables
  DEFAULT_BROWSER_EXE := 'xxx';        // Executable name of default browser
  default_browser_ready_flag := False; // Default browser ready flag

  DTDNS_time_tracker    :=0; // Ref time
  //==== DtDNS Config and variables
   //==USF_DTDNS_INI - Default DtDNS configuration

   Ini3 := TINIFile.Create(USF_DTDNS_INI); // create object
   //*** Start ***
   //[DTDNS]
   DTDNS_logging          := strToBool(Ini3.ReadString('DTDNS','logging','false'));  // Loging enabled or disabled
   DTDNS_auto_run         := strToBool(Ini3.ReadString('DTDNS','auto_run','false')); // Automatically run DtDNS update

   //[ACCOUNTS]
   DTDNS_domain_name_1    := Ini3.ReadString('ACCOUNTS','domain_name_1','');    //DtDns Domain name
   DTDNS_domain_name_2    := Ini3.ReadString('ACCOUNTS','domain_name_2','');    //DtDns Domain name
   DTDNS_domain_name_3    := Ini3.ReadString('ACCOUNTS','domain_name_3','');    //DtDns Domain name
   DTDNS_domain_name_4    := Ini3.ReadString('ACCOUNTS','domain_name_4','');    //DtDns Domain name
   DTDNS_domain_name_5    := Ini3.ReadString('ACCOUNTS','domain_name_5','');    //DtDns Domain name
   DTDNS_account_password := Ini3.ReadString('ACCOUNTS','password','');         //DtDns account password


   //*** End ***
   Ini3.Free;     // Free method of object
  //==== End DtDNS Config and variables


 //=== Unicontroller configuration us_config.ini
   //==USF_US_CONF_INI - Default server configuration

   Ini1 := TINIFile.Create(USF_US_CONF_INI); // create object
   //*** Start ***
   //[APP]
   USC_AppNumber       := Ini1.ReadString('APP','AppNumber','1');                          // Application number
   USC_AppVersion      := Ini1.ReadString('APP','AppVersion','15.0.2');                    // Version
   USC_ServerType      := Ini1.ReadString('APP','ServerType','WAMP0');                     // Default=WAMP APS=Apache standalone server MYS=MySQL  standalone server
   USC_ServerTypeText1 := Ini1.ReadString('APP','ServerTypeText1','Uniform Server Zero');  // TrayIcon hover text
   USC_ServerTypeText2 := Ini1.ReadString('APP','ServerTypeText2','Portable WAMP Server'); // TrayIcon hover text
   USC_TrayIconEnabled := strToBool(Ini1.ReadString('APP','TrayIconEnabled','True'));      // TrayIcon enabled

   //[PCSTARTUP]
   USC_RunAtPcStartUpEnabled := strToBool(Ini1.ReadString('PCSTARTUP','RunAtPcStartUpEnabled','False')); // Enables running server at PC start up:  true/false      
   USC_RunApacheAtStartUp    := strToBool(Ini1.ReadString('PCSTARTUP','RunApacheAtStartUp','True'));     // Run Apache server at PC start-up: true/false       
   USC_RunMysqlAtStartUp     := strToBool(Ini1.ReadString('PCSTARTUP','RunMysqlAtStartUp','True'));      // Run MySQL server at PC start-up: true/false        

   //[CRON]
   USC_enable_cron           := strToBool(Ini1.ReadString('CRON','enable_cron','False'));   // Enables/Disable cron
   USC_cron_logging          := strToBool(Ini1.ReadString('CRON','cron_logging','False'));  // Enables/Disable cron logging

   //[TIMERS]
   USC_AP_StartSafetyTime    := strToInt(Ini1.ReadString('TIMERS','AP_StartSafetyTime','30')); // Maximum wait time in seconds
   USC_AP_StopSafetyTime     := strToInt(Ini1.ReadString('TIMERS','AP_StopSafetyTime', '15')); // Maximum wait time in seconds
   USC_MY_StartSafetyTime    := strToInt(Ini1.ReadString('TIMERS','MY_StartSafetyTime','30')); // Maximum wait time in seconds
   USC_MY_StopSafetyTime     := strToInt(Ini1.ReadString('TIMERS','MY_StopSafetyTime', '15')); // Maximum wait time in seconds
   USC_PM_StartSafetyTime    := strToInt(Ini1.ReadString('TIMERS','PM_StartSafetyTime','30')); // Pale Moon Maximum wait time in seconds
   USC_PM_StopSafetyTime     := strToInt(Ini1.ReadString('TIMERS','PM_StopSafetyTime', '15')); // Pale Moon Maximum wait time in seconds
   USC_PM_ReadySafetyTime    := strToInt(Ini1.ReadString('TIMERS','PM_ReadySafetyTime','30')); // Palse Moon Maximum wait time to ready in seconds


   //-- PC-Win start-up: Increase startup timers 
   If USC_RunAtPcStartUpEnabled Then
     begin
       USC_AP_StartSafetyTime  := 120; // Maximum wait time in seconds
       USC_MY_StartSafetyTime  := 120; // Maximum wait time in seconds
     end;

    //[GENERAL]
    USC_NAG_USER     := strToBool(Ini1.ReadString('GENERAL','Nag_user','True'));      // Nag user at start-up eg change MuSQL password
    USC_ZA_CHECK     := strToBool(Ini1.ReadString('GENERAL','ZoneAlarmCheck','True'));  //Check if Zone Alarm Presence is required

   //[UNIQUE]
   UNIQUE_TEXT_IN_TITLE_BAR := Ini1.ReadString('UNIQUE','Unique_text','UniServer Zero 15'); // Reference HTML page title e.g UniServer Zero 11 Pale Moon


   //[HOSTS]
   USC_EditHostsFileEnabled := strToBool(Ini1.ReadString('HOSTS','EditHostsFileEnabled','True'));   // Edit Windows hosts file eabled


   //-- PC-Win start-up: Disable nag
   If USC_RunAtPcStartUpEnabled Then
     begin
        USC_NAG_USER := false;
     end;    

   //-- MySQL command line parameters override
   //[MYSQL]

   USF_MYSQL_INI                 := Ini1.ReadString('MYSQL','defaults-file',             US_MYSQL + '\my.ini');             // MySQL configuration MySQL default file - command line parameter
   USP_TMPDIR                    := Ini1.ReadString('MYSQL','tmpdir',                    US_TMP);                           // MySQL temp folder - command line parameter 
   USP_DATADIR                   := Ini1.ReadString('MYSQL','datadir',                   UniConPath +  '\core\mysql\data'); // MySQL data folder - command line parameter 
   USP_INNODB_DATA_HOME_DIR      := Ini1.ReadString('MYSQL','innodb_data_home_dir',      USP_DATADIR);                      // MySQL innodb data folder - command line parameter
   USP_INNODB_LOG_GROUP_HOME_DIR := Ini1.ReadString('MYSQL','innodb_log_group_home_dir', USP_DATADIR);                      // MySQL innodb log folder - command line parameter

   //*** End ***
   Ini1.Free;     // Free method of object
 //=== End Unicontroller configuration us_config.ini


 //=== Unicontroller user configuration us_user.ini
    //==USF_US_USER_INI - User server configuration

   Ini2 := TINIFile.Create(USF_US_USER_INI); // create object
   //*** Start ***
   //[USER]
   UENV_AP_PORT       := Ini2.ReadString('USER','AP_PORT','80');
   UENV_AP_SSL_PORT   := Ini2.ReadString('USER','AP_SSL_PORT','443');
   UENV_US_SERVERNAME := Ini2.ReadString('USER','US_SERVERNAME','localhost');
   UENV_US_ROOTF_WWW  := Ini2.ReadString('USER','US_ROOTF_WWW','./www');
   UENV_US_ROOTF_SSL  := Ini2.ReadString('USER','US_ROOTF_SSL','./ssl');

   //Convert absolute/relative root paths to absolute paths.
   UENV_US_ROOTF_WWW := RelToAbsDir(UniConPath_F,UENV_US_ROOTF_WWW); //Convert to absolute
   UENV_US_ROOTF_SSL := RelToAbsDir(UniConPath_F,UENV_US_ROOTF_SSL); //Convert to absolute

   USF_WWW_HTACCESS          :=  UENV_US_ROOTF_WWW+'\.htaccess';              // WWW Root htaccess file
   USF_SSL_HTACCESS          :=  UENV_US_ROOTF_SSL+'\.htaccess';              // SSL Root htaccess file
   USF_WWW_HTACCESS_BACK     :=  UENV_US_ROOTF_WWW+'\.htaccess_back';         // WWW Root htaccess backup file
   USF_SSL_HTACCESS_BACK     :=  UENV_US_ROOTF_SSL+'\.htaccess_back';         // SSL Root htaccess backup file


   UENV_MYSQL_TCP_PORT := Ini2.ReadString('USER','MYSQL_TCP_PORT','3306');

   UENV_PHP_SELECT := Ini2.ReadString('USER','PHP_SELECT','php83');          // Get user selected PHP version

   //If folders php70, php71, php72, php73, php74, php80, php81, php82 or php83 do not exist
   //or incorrect PHP_SELECT value, override user config. Set var PHP_SELECT to None.
   php_valid := False; // Assume PHP not installed
   If (DirectoryExists(US_PHP70) And (UENV_PHP_SELECT ='php70')) Then php_valid := True;
   If (DirectoryExists(US_PHP71) And (UENV_PHP_SELECT ='php71')) Then php_valid := True;
   If (DirectoryExists(US_PHP72) And (UENV_PHP_SELECT ='php72')) Then php_valid := True;
   If (DirectoryExists(US_PHP73) And (UENV_PHP_SELECT ='php73')) Then php_valid := True;
   If (DirectoryExists(US_PHP74) And (UENV_PHP_SELECT ='php74')) Then php_valid := True;
   If (DirectoryExists(US_PHP80) And (UENV_PHP_SELECT ='php80')) Then php_valid := True;
   If (DirectoryExists(US_PHP81) And (UENV_PHP_SELECT ='php81')) Then php_valid := True;
   If (DirectoryExists(US_PHP82) And (UENV_PHP_SELECT ='php82')) Then php_valid := True;
   If (DirectoryExists(US_PHP83) And (UENV_PHP_SELECT ='php83')) Then php_valid := True;
   If Not php_valid Then UENV_PHP_SELECT :='None';

   UENV_PHP_INI_SELECT := Ini2.ReadString('USER','PHP_INI_SELECT','php_test.ini');

   USUC_RUN_CONSOLE    := Ini2.ReadString('USER','RUN_CONSOLE','yes');
   USUC_DISPLAY_PAGE_1 := Ini2.ReadString('USER','DISPLAY_PAGE_1','yes');
   USUC_PAGE_1         := Ini2.ReadString('USER','PAGE_1','http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/us_splash/index.php');
   USUC_DISPLAY_PAGE_2 := Ini2.ReadString('USER','DISPLAY_PAGE_2','yes');
   USUC_PAGE_2         := Ini2.ReadString('USER','PAGE_2','http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/index.php');
   USUC_USER_EDITOR    := Ini2.ReadString('USER','USER_EDITOR','notepad.exe'); //Editor to use for displaying text pages

   //*** End ***
   Ini2.Free;     // Free method of object
 //=== End Unicontroller configuration us_user.ini


 //=== Key option 1 user configuration us_opt1.ini
    //==USF_OPT1_INI - User server configuration

   Ini4 := TINIFile.Create(USF_OPT1_INI); // create object
   //*** Start ***
   //[USER]
   US_OPT1_BTN_TXT   := Ini4.ReadString('USER','btntext','opt1');
   US_OPT1_PAGE      := Ini4.ReadString('USER','page','index.php');

   //*** End ***
   Ini4.Free;     // Free method of object
 //=== End Key option 1 user configuration us_opt1.ini

 //=== Key option 2 user configuration us_opt2.ini
    //==USF_OPT2_INI - User server configuration

   Ini5 := TINIFile.Create(USF_OPT2_INI); // create object
   //*** Start ***
   //[USER]
   US_OPT2_BTN_TXT   := Ini5.ReadString('USER','btntext','opt2');
   US_OPT2_PAGE      := Ini5.ReadString('USER','page','index.php');

   //*** End ***
   Ini5.Free;     // Free method of object
 //=== End Key option 2 user configuration us_opt2.ini

 //=== Key option 3 user configuration us_opt3.ini
    //==USF_OPT3_INI - User server configuration

   Ini6 := TINIFile.Create(USF_OPT3_INI); // create object
   //*** Start ***
   //[USER]
   US_OPT3_BTN_TXT   := Ini6.ReadString('USER','btntext','opt3');
   US_OPT3_PAGE      := Ini6.ReadString('USER','page','index.php');

   //*** End ***
   Ini6.Free;     // Free method of object
 //=== End Key option 3 user configuration us_opt3.ini


 //=== MySQL or MariaDB configuration file us_opt.ini
    //==USF_MYMAR_TXT_INI - User server configuration

   Ini7 := TINIFile.Create(USF_MYMAR_TXT_INI); // create object
   //*** Start ***
   //[USER]
   US_MYMAR_TXT   := Ini7.ReadString('USER','text','MySQL'); //MySQL or MariaDB
   MY_SQL_VER   := Ini7.ReadString('USER','version','8');     //MySQL version

   //*** End ***
   Ini7.Free;     // Free method of object

 //=== End MySQL or MariaDB configuration file us_opt.ini

 //Button text
 START_MY            := 'Start '+US_MYMAR_TXT;   // Start MySQL or Start MariaDB
 STOP_MY             := 'Stop  '+US_MYMAR_TXT;   // Start MySQL or Start MariaDB

 //=== SET ENVIRONOMENT VARIABLES =======

 //===Set user environment variables - values obtained from config file
  windows.SetEnvironmentVariable('AP_PORT',PCHAR(UENV_AP_PORT));               //Set Apache port
  windows.SetEnvironmentVariable('AP_SSL_PORT',PCHAR(UENV_AP_SSL_PORT));       //Set Apache SSL port
  windows.SetEnvironmentVariable('US_SERVERNAME',PCHAR(UENV_US_SERVERNAME));   //Set Apache server name

  windows.SetEnvironmentVariable('US_ROOTF_WWW',PCHAR(UENV_US_ROOTF_WWW));     //Set Apache server root folder www
  windows.SetEnvironmentVariable('US_ROOTF_SSL',PCHAR(UENV_US_ROOTF_SSL));     //Set Apache server root folder ssl

  windows.SetEnvironmentVariable('MYSQL_TCP_PORT',PCHAR(UENV_MYSQL_TCP_PORT));  //Set MySQL port
  windows.SetEnvironmentVariable('PHP_SELECT',PCHAR(UENV_PHP_SELECT));          //PHP Select
  windows.SetEnvironmentVariable('PHP_INI_SELECT',PCHAR(UENV_PHP_INI_SELECT));  //PHP config file Select
  windows.SetEnvironmentVariable('US_PAC',PCHAR('file:///'+UniConPath_F+'/home/us_pac/proxy.pac')); //Set PAC file for Blue Moon

 //===Set common environment variables
  windows.SetEnvironmentVariable('HOME',PCHAR(UniConPath));        //Top level folder
  windows.SetEnvironmentVariable('US_ROOTF',PCHAR(UniConPath_f));  //Top level folder forward slash
  windows.SetEnvironmentVariable('MYSQL_HOME',PCHAR(US_MYSQL));    //Top level mysql folder

  //===Set strawberry perl common environment variables
   If DirectoryExists(US_PERL) Then
   begin
     windows.SetEnvironmentVariable('TERM','dumb');
     // avoid collisions with other perl stuff on your system
     windows.SetEnvironmentVariable('PERL_JSON_BACKEND','');
     windows.SetEnvironmentVariable('PERL_YAML_BACKEND','');
     windows.SetEnvironmentVariable('PERL5LIB','');
     windows.SetEnvironmentVariable('PERL5OPT','');
     windows.SetEnvironmentVariable('PERL_MM_OPT','');
     windows.SetEnvironmentVariable('PERL_MB_OPT','');
   end;

  //===Set PATH environment variable
  ORIGINAL_ENV_PATH := SysUtils.GetEnvironmentVariable('PATH');  // Get original path
  us_set_environment_path;                                       // Set new env path

  //===Set additional environment variable configured in us_config.ini
  us_set_additional_environment_vars;

 //=== END SET ENVIRONOMENT VARIABLES

end;
{--- End us_main_init --------------------------------------}
end.

