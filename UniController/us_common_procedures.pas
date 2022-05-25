unit us_common_procedures;

{#############################################################################
'# Name: us_common_procedures.inc
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Dialogs,Forms,
  default_config_vars,
  us_common_functions,
  JwaTlHelp32,windows,Process,
  Graphics,
  RegExpr, LazFileUtils, FileUtil,
  ExtCtrls, LCLType, INIFiles;

//=== General ===
 procedure us_set_environment_path;             //Set enironment path
 procedure us_set_additional_environment_vars;  //Set additional enironment variables defined in config
 procedure us_ini_set(IniFile:string;section:string;variable:string;value:string); //Set value in ini configuration file
 procedure us_clean_up;                     // Delete listed files and delete files in listed folders
 procedure us_set_icon_and_hover_text;      // Set icon displayed and hover text based on ServerType in config file
 procedure KillRunningProcess(ProcessName: string);        // Kill named process
 procedure us_display_in_editor(FileToDisplay: string);    // Display page in editor default - notepad

 //=== Apache ===
procedure us_start_apache_program;        // Start Apache as a standard program
procedure us_kill_apache_program;         // Kills Apache running as a standard program
procedure us_apache_syntax_check;         // Runs Apache with -t performs a syntax check
procedure apache_indicator(state:String); // Apache Bi-state indicator

procedure us_enable_apache_ssl;           // Enable Apache ssl in configuration file httpd.conf
procedure us_disable_apache_ssl;          // Disable Apache ssl in configuration file httpd.conf

//=== MySQL ===
procedure us_start_mysql_program;                   // Start MySQL as a standard program
procedure us_kill_mysql_program;                    // Kills MySQL running as a standard program
procedure us_clean_stop_mysql_program;              // Clean stop MySQL program. Using a name and password last resort Kills
procedure mysql_indicator(state:String);            // MySQL Bi-state indicator
procedure us_display_mysql_prompt;                  // Display MySQL command console and open MySQL prompt
procedure us_save_mysql_password(my_pwd_in:String); // Save MySQL password to file

procedure us_create_mysql_database(database_name:string); // Create MySQL Database using mysqladmin 
procedure us_drop_mysql_database(database_name:string);   // Delete(Drop) MySQL Database using mysqladmin 

//=== MySQL Change root password ===
procedure mysql_change_root_password(new_password:string);  // Runs/stops server chnages password

//=== MySQL restore root password ===
procedure mysql_restore_root_password;                      // Restore MySQL server root password.



//=== Display Server Console ===
procedure us_display_server_console;   // Display console at root folder

//=== Perl
procedure force_shebang_update(NEW_SHEBANG:string); // Update shebang in all perl scripts

//=== PHP
procedure disable_all_php_accelerators;               // Disable all enabled accelerators
procedure enable_php_accelerator(accelerator:String); // Enable named PHP accelerator in selected PHP ini configuration file
procedure view_selected_acc_control_panel;            // View selected accelerator, control pannel

//=== Message Dialogs
procedure msg_restart_apache;                  // Check Apache runnning and inform user to restart Apache
procedure msg_stop_mysql_port_change;          // Port change inform user to stop MySQL Server
procedure msg_stop_mysql_pwd_change;           // Password change inform user to stop MySQL server
procedure msg_password_file_contains_defaults; // Password file contains defaults
procedure msg_password_file_empty;             // Password file empty


//=== GO PEAR ===
procedure us_update_go_pear_config; //updates paths in go-pear configuration file

//=== PALE MOON BROWSER ===
procedure us_start_palemoon;                // Start Portable Pale Moon browser
procedure us_kill_palemoon;                 // Kills Pale Moon Browser

//=== DEFAULT BROWSER ===
procedure us_start_default_browser;        // Start Default browser

//=== DISPLAY IN BROWSER ===
procedure browser_display_url(url:string); // Display page in Pale Moon

//=== PAC ===
procedure us_add_to_pac_file(host:string);       // Add new host - domain name/IP to PAC file
procedure us_delete_from_pac_file(host:string);  // Remove host  - domain name/IP from PAC file
procedure us_proxy_file_port_update(old_port:string;new_port:string); //Update PAC file with new port

//=== HOSTS ===
procedure us_launch_edit_hosts_utility;            // Launches - Uniform Server Windows hosts file utility
procedure us_add_to_hosts_file(host:string);       // Add new host - domain name/IP to hosts file
procedure us_delete_from_hosts_file(host:string);  // Remove host  - domain name/IP from hosts file

//===END===


implementation

uses
    main_unit;

//=== General ===
 
{****************************************************************************
 This procedure sets the environment PATH.

  Used by us_main_init
  Also used by elements that change the path:
   TMain.MMSS_php70Click    - PHP select buttons
   TMain.MMSS_php71Click    - change the
   TMain.MMSS_php72Click    - environment paths
   TMain.MMSS_php73Click
   TMain.MMSS_php74Click
   TMain.MMSS_php80Click
   TMain.MMSS_php81Click

  ORIGINAL_ENV_PATH - Original environment path when controller started

  Procedure first reads additional environment path variables
  these are user definable in file home\us_config\us_config.ini
  under the section named [ENV_PATHS]
  Variable format is path=value
  If the value contains the constant %UniConPath% it is expanded
  to a full path.

============================================================================}
procedure us_set_environment_path;
var
 path:        string;                // Temp var
 Ini1:TINIFile;                      // Handle for configuration file
 sList_env_vars: TStringList;        // Raw string list of environment vars
 sList_split: TStringList;           // Temp String list
 i: Integer;                         // Loop counter
 value:String;                       // Variable value

begin
  path := '';                                           // Set initial value
  windows.SetEnvironmentVariable('PATH',PCHAR(path));   // Reset PATH variable

  // Create new paths

  //===Add additional paths set in config file
  If FileExists(USF_US_CONF_INI) Then // If file exists proceed
   begin
    Ini1           := TINIFile.Create(USF_US_CONF_INI); // create ini object
    sList_env_vars := TStringList.Create;               // Create vars object
    sList_split    := TStringList.Create;               // Create split object

    Ini1.ReadSectionRaw('ENV_PATHS'  ,sList_env_vars);  // Read section unchanged

    //Scan Environment Path section
    If Not (sList_env_vars.Count=0) Then                // Check content
      begin
       for i:=0 to sList_env_vars.Count-1 do            // Scan vars line by line
        begin
         //Split name value pair
         sList_split.Clear;                             // Clear list
         sList_split.StrictDelimiter := true;           // Force split only on defined delimiter
         sList_split.Delimiter := '=';                  // Define delimeter
         sList_split.DelimitedText := sList_env_vars[i];// Split name value pair
         value := sList_split[1];                       // Only value used

         //If the value contains a path with %UniConPath% expand this to give full path
         value := StringReplace(value, '%UniConPath%',UniConPath,[rfReplaceAll]); // Expand path
         If DirectoryExists(value) Then path := path + value + ';';               //Add to path
       end;
      end;

     //Clean-up
     Ini1.Free;            // Free method of object
     sList_env_vars.free;  // Remove
     sList_split.free;     // Remove
    end;
  //===END Add additional paths set in config file


  //--If Apache installed add to path --------
  If DirectoryExists(US_APACHE_BIN) Then
    path := path + US_APACHE_BIN + ';';                 // Apache binary folder

  //--If MySQL installed add to path --------
  If DirectoryExists(US_MYSQL_BIN) Then                 // MySQL binary folder
     path := path + US_MYSQL_BIN + ';';                 // MySQL binary

  //--If PHP installed
  If Not (UENV_PHP_SELECT = 'None') then                // PHP Installed
   begin
    If DirectoryExists(US_CORE + '\' +UENV_PHP_SELECT) Then
     begin
       path := path + US_CORE + '\' +UENV_PHP_SELECT + ';'; // PHP
     end;
   end;

  //--If Perl installed add to path --------
  If DirectoryExists(US_PERL) Then
   begin
     path := path + US_PERL + ';';     // Perl
     path := path + US_PERL_BIN + ';'; // Perl binaries
     path := path + US_PERL_LIB + ';'; // Perl library
   end;

  //--Add remaining paths
  If DirectoryExists(US_OPENSSL) Then path := path + US_OPENSSL + ';'; // Open SSL
  If DirectoryExists(US_MSMTP)   Then path := path + US_MSMTP + ';' ;  // msmtp
  If DirectoryExists(US_UTILS)   Then path := path + US_UTILS + ';';   // utils

  //-- Add original path
  path := path + ORIGINAL_ENV_PATH;                              // Add original paths

  windows.SetEnvironmentVariable('PATH',PCHAR(path));            // Set new PATH variable

end;
{--End us_set_environment_path ----------------------------------------------}


{****************************************************************************
 This procedure sets additional environment variables
 these are user definable in file home\us_config\us_config.ini
 under the section named [ENV_VARS]
 Variable format is variable_name=value
============================================================================}
procedure us_set_additional_environment_vars;
var
  Ini1:TINIFile;                      // Handle for configuration file
  sList_env_vars: TStringList;        // Raw string list of environment vars
  sList_split: TStringList;           // Temp String list
  i: Integer;                         // Loop counter
  name: String;                       // Variable name
  value:String;                       // Variable value
begin
  If FileExists(USF_US_CONF_INI) Then // If file exists proceed
   begin
    //Create objects
    Ini1           := TINIFile.Create(USF_US_CONF_INI); // create ini object
    sList_env_vars := TStringList.Create;               // Create vars object
    sList_split    := TStringList.Create;               // Create split object

    //Read raw sections
    Ini1.ReadSectionRaw('ENV_VARS'  ,sList_env_vars);   // Read section unchanged

    //Scan Environment section
    If Not (sList_env_vars.Count=0) Then                  // Check content
      begin
       for i:=0 to sList_env_vars.Count-1 do            // Scan vars line by line
        begin
         //Split name value pair
         sList_split.Clear;                             // Clear list
         sList_split.StrictDelimiter := true;           // Force split only on defined delimiter
         sList_split.Delimiter := '=';                  // Define delimeter
         sList_split.DelimitedText := sList_env_vars[i];// Split name value pair
         name  := sList_split[0];
         value := sList_split[1];

         //If the value contains a path with %UniConPath% expand this to give full path
         value := StringReplace(value, '%UniConPath%',UniConPath,[rfReplaceAll]); // Expand path
         windows.SetEnvironmentVariable(PCHAR(name),PCHAR(value));                // Set new variable
       end;
      end;

     //Clean-up
     Ini1.Free;            // Free method of object
     sList_env_vars.free;  // Remove
     sList_split.free;     // Remove
    end;
end;
{--End us_set_additional_environment_vars ------------------------------------}

 {****************************************************************************
 Set value in ini configuration file
 Note uses INIFiles
 =============================================================================}
 procedure us_ini_set(IniFile:string;section:string;variable:string;value:string);
 Var
 ini:TINIFile;
 begin
  ini := TINIFile.Create(IniFile);          // create object
  ini.WriteString(section,variable,value);  // set value
  ini.Free;                                 // Free method of object
 end;
 {--- End us_ini_set ------------------------------------------------}


{********************************************************************
This procedure:
 Deletes listed files
 Deletes listed folders
 Deletes dolders/files in listed folders

 Lists are defined in configuration file 
 UniServerZ\home\us_config\us_clean_up.ini

====================================================================}
procedure us_clean_up; 
var
 sList_files          :TStringList; // List of files in section   [FILES]
 sList_folders        :TStringList; // List of folders in section [FOLDERS]
 sList_folder_content :TStringList; // List of folders in section [FOLDERS_CONTENT]
 Ini1           :TINIFile;          // Handle for configuration file
 i              :Integer;           // Loop counter
 path           :String;            // Full path to file or folder
begin

 If FileExists(USF_US_CLEAN_UP_INI) Then // If file exists proceed
  begin
   //Create objects
   Ini1 := TINIFile.Create(USF_US_CLEAN_UP_INI); // create object
   sList_files          := TStringList.Create;   // Create object
   sList_folders        := TStringList.Create;   // Create object
   sList_folder_content := TStringList.Create;   // Create object

   //Read raw sections
   Ini1.ReadSectionRaw('FILES'  ,sList_files);   // Read section unchanged
   Ini1.ReadSectionRaw('FOLDERS',sList_folders); // Read section unchanged
   Ini1.ReadSectionRaw('FOLDERS_CONTENT',sList_folder_content); // Read section unchanged

   //Scan FILE section
   If Not (sList_files.Count=0) Then                // Check content
    begin
       for i:=0 to sList_files.Count-1 do         // Scan file list line by line
        begin
          If Not (Pos(';',sList_files[i]) <> 0) then    // Not a comment 
            begin
              path := UniConPath+'\'+sList_files[i];    // Full path
              If FileExists(path) Then DeleteFile(Pchar(path));
            end;
        end;
    end;
 


   //Scan FOLDERS section
   If Not (sList_folders.Count=0) Then                                 // Check content
    begin
       for i:=0 to sList_folders.Count-1 do                            // Scan folder list line by line
        begin
          If Not (Pos(';',sList_folders[i]) <> 0) then                 // Not a comment
          begin
            path := UniConPath+'\'+sList_folders[i];                   // Full path
            If DirectoryExists(path) Then
              Begin
              if DeleteDirectory(path,True) then RemoveDirUTF8(path);  // Delete content and folder
             end;
          end;
        end;
    end;


   //Scan FOLDERS_CONTENT section
   If Not (sList_folder_content.Count=0) Then                          // Check content
    begin
       for i:=0 to sList_folder_content.Count-1 do                     // Scan folder list line by line
        begin
          If Not (Pos(';',sList_folder_content[i]) <> 0) then          // Not a comment
          begin
            path := UniConPath+'\'+sList_folder_content[i];            // Full path
            If DirectoryExists(path) Then DeleteDirectory(path,true);  // Delete folder content
          end;
        end;
    end;

   //Clean-up
   Ini1.Free;                 // Free method of object
   sList_files.free;          // Remove
   sList_folders.free;        // Remove
   sList_folder_content.free; // Remove
 end;
end;
{--- End procedure us_clean_up --------------------------------------------}



{********************************************************************
This procedure sets icon displayed and hover text based on
ServerType in config file. Valid valus are:

User can run Apache and MySQL as standalone servers the icon is
configurable to distingush between the  servers.

 ServerType - WAMP0 to WAMP3  -DefaultWAMP0 (Apache MySQL PHP)
            - APS0  to APS3   -Apache standalone server
            - MYS0  to  MYS3  -MySQL  standalone server

 TrayIcon hover text:
 ServerTypeText1 - Default: Uniform Server
 ServerTypeText2 - Default: Portable WAMP

 Icons defined in: unicon_images.rc
 120 ICON "unicon_images/main_icon2_a0.ico"
 121 ICON "unicon_images/main_icon2_a1.ico"
 122 ICON "unicon_images/main_icon2_a2.ico"
 123 ICON "unicon_images/main_icon2_a3.ico"

 130 ICON "unicon_images/main_icon2_m0.ico"
 131 ICON "unicon_images/main_icon2_m1.ico"
 132 ICON "unicon_images/main_icon2_m2.ico"
 133 ICON "unicon_images/main_icon2_m3.ico"

 140 ICON "unicon_images/main_icon2_u0.ico"
 141 ICON "unicon_images/main_icon2_u1.ico"
 142 ICON "unicon_images/main_icon2_u2.ico"
 143 ICON "unicon_images/main_icon2_u3.ico"

 Note: Resource is added as follows to main_unit
 Note Replace [] with curly brackes - [] avoids warning Comment level 2

 implementation
 [$R *.lfm]
 [$R manifest.rc]       // Add manifest to raise privileges to admin
 [$R unicon_images.rc]  // Add image resource for tray icon

====================================================================}
procedure us_set_icon_and_hover_text;

begin

 //--Set hover text
 Application.Title:=USC_ServerTypeText1 + #13#10 + USC_ServerTypeText2;

 //-- Must clear Icon first
 Application.Icon.Clear; // Must clear Icon first

 If USC_ServerType = 'APS0'  Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(120));
 If USC_ServerType = 'APS1'  Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(121));
 If USC_ServerType = 'APS2'  Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(122));
 If USC_ServerType = 'APS3'  Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(123));

 If USC_ServerType = 'MYS0'  Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(130));
 If USC_ServerType = 'MYS1'  Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(131));
 If USC_ServerType = 'MYS2'  Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(132));
 If USC_ServerType = 'MYS3'  Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(133));

 If USC_ServerType = 'WAMP0' Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(140));
 If USC_ServerType = 'WAMP1' Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(141));
 If USC_ServerType = 'WAMP2' Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(142));
 If USC_ServerType = 'WAMP3' Then Application.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(143));

 //TrayIcon
 If USC_TrayIconEnabled Then
   begin
    //--Set hover text
    Main.SystrayIcon.Hint:=USC_ServerTypeText1 + #13#10 + USC_ServerTypeText2;

    //-- Must clear Icon first
    Main.SystrayIcon.Icon.Clear; // Must clear Icon first

    If USC_ServerType = 'APS0'  Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(120));
    If USC_ServerType = 'APS1'  Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(121));
    If USC_ServerType = 'APS2'  Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(122));
    If USC_ServerType = 'APS3'  Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(123));

    If USC_ServerType = 'MYS0'  Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(130));
    If USC_ServerType = 'MYS1'  Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(131));
    If USC_ServerType = 'MYS2'  Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(132));
    If USC_ServerType = 'MYS3'  Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(133));

    If USC_ServerType = 'WAMP0' Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(140));
    If USC_ServerType = 'WAMP1' Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(141));
    If USC_ServerType = 'WAMP2' Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(142));
    If USC_ServerType = 'WAMP3' Then Main.SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(143));

   end;


end;
{--- End us_set_icon_and_hover_text --------------------------------}

 {********************************************************************
 Kill named process.
 Uses JwaTlHelp32, windows
 ====================================================================}
  procedure KillRunningProcess(ProcessName: string);
  var
   pa: TProcessEntry32;
   RetVal: THandle;
   AHandle: THandle;
    begin

   RetVal := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
   pa.dwSize := sizeof(pa);

   //Get first process
   if Process32First(RetVal, pa) then
   begin
     //While we have process handle
     while Process32Next(RetVal, pa) do
     begin
       if pa.szExeFile = ProcessName then
         begin
           AHandle := OpenProcess(PROCESS_ALL_ACCESS,False,pa.th32ProcessID); //uses windows
           TerminateProcess(AHandle,255);
           CloseHandle(AHandle);
         end;
       end;
   end;
   CloseHandle(RetVal);
 end;
 {----------------------------------------------------------------------------}

 {****************************************************************************
 This procedure displays a file in an external text editor. This editor is
 user definable inthe user.ini file. Note: - default is notepad.exe
 If editor already on windows path use its name, otherwise use full path.
 Input Full path of file to display
 =============================================================================}
 procedure us_display_in_editor(FileToDisplay: string);
 var
  AProcess: TProcess;
  str:String;
 begin
     AProcess := TProcess.Create(nil);                     // Create process
     try
       Try
          AProcess.Executable := USUC_USER_EDITOR;              // Executable to run
          AProcess.Parameters.Add(FileToDisplay);               // Parmeters passed to exe

          AProcess.Options := AProcess.Options + [poNoConsole]; // Set options hide console
          AProcess.Execute;                                     // execute detatched process
       except
         str := 'Editor specified in us_user.ini cannot be found.';
         us_MessageDlg('Error', str, mtWarning,[mbOk],0) ; //Display message
       end;
     finally
       AProcess.Free;                                        // free memory ;
     end;
 end;
 {----------------------------------------------------------------------------}

//=== Apache ===

{****************************************************************************
Start Apache as a program
 Skip if already running.

  usapache :=  ExtractFilePath(Application.ExeName) + 'core\apache2';
  strCmd := usapache + '\bin\httpd.exe' + ' -f ' + usapache + '\conf\httpd.conf -d ' + usapache;
Note: Uses Process
=============================================================================}
procedure us_start_apache_program;
Var
 AProcess: TProcess;
 saftey_loop: Integer;

begin
 if not ApacheRunning() then
  begin
   saftey_loop :=0;

   //--Run command string.
  AProcess := TProcess.Create(nil);                         // Create new process

  AProcess.Executable := US_APACHE_BIN + '\' + AP_EXE_NAME; // Executable to run
  AProcess.Parameters.Add('-f');
  AProcess.Parameters.Add(USF_APACHE_CNF);                  // Apache configuration file
  AProcess.Parameters.Add('-d');
  AProcess.Parameters.Add(US_APACHE );                      // Apache folder
  AProcess.Options  := AProcess.Options + [poNoConsole];    // Set option no console
  //AProcess.Priority := ppHigh;                            // The process runs at higher than normal priority.
  AProcess.Execute;                                         // Run command
  AProcess.Free;                                            // Release process


  Repeat   // Wait for Apache to start
    begin
      Application.ProcessMessages; //allow message processing
      sleep(100);
      saftey_loop := saftey_loop +1;             // Increment counter
      if saftey_loop > (USC_AP_StartSafetyTime*10) then
       begin
         us_MessageDlg('Warning','Failed to start Apache', mtWarning,[mbOk],0) ; //Display warning message
         Break;
       end;
    end;
  Until ApacheRunning();

  // Wait for Apache to serve pages
  saftey_loop := 0;
  Repeat
    begin
      Application.ProcessMessages; //allow message processing
      sleep(1000);
      saftey_loop := saftey_loop +1;             // Increment counter
      if saftey_loop > (USC_AP_StartSafetyTime) then
       begin
         us_MessageDlg('Warning','Apache failed to start', mtWarning,[mbOk],0) ; //Display warning message
         Break;
       end;
    end;
  Until Not ApachePortFree; // Until Apache is listening on 127.0.0.1','80'

  end;
 end;
{----------------------------------------------------------------------------}

{****************************************************************************
Kills Apache running as a standard program
Global variables used:
  AP_EXE_NAME
=============================================================================}
procedure us_kill_apache_program;
Var
 saftey_loop: Integer;

begin
 if ApacheRunning() then
  saftey_loop:=0;
  begin
      KillRunningProcess(AP_EXE_NAME);           //Kill Apache program
  end;

  Repeat                                         // Wait for Apache to stop
    begin
      Application.ProcessMessages; //allow message processing
      sleep(100);
      saftey_loop := saftey_loop +1;             // Increment counter
      if saftey_loop > (USC_AP_StopSafetyTime*10) then
       begin
         us_MessageDlg('Warning','Failed to stop Apache', mtWarning,[mbOk],0) ; //Display warning message
         Break;
       end;
    end;
   Until not ApacheRunning();
 end;
{----------------------------------------------------------------------------}

{*****************************************************************************
Apache Syntax Check
Opens a dos window and displays syntax check result
=============================================================================}
procedure us_apache_syntax_check;
Var
 AProcess: TProcess;
begin

 AProcess := TProcess.Create(nil); // Create process

 AProcess.Executable := 'cmd';                                // Executable to run
 AProcess.Parameters.Add('/T:B0');                            // Set background colour
 AProcess.Parameters.Add('/K');                               // Keep open
 AProcess.Parameters.Add('title');                            // A title is required
 AProcess.Parameters.Add('Apache Syntax check');              // Title
 AProcess.Parameters.Add('&&');                               // Start a new command line
 AProcess.Parameters.Add(US_APACHE_BIN + '\' + AP_EXE_NAME);  // Apache exe
 AProcess.Parameters.Add('-t');                               // Syntax check

 AProcess.Execute; // execute detatched process command window remains visible
 AProcess.Free;    // free memory

end;
{----------------------------------------------------------------------------}


{*****************************************************************************
Apache Server status indicator

A bi-state indicator showing current server status.
 Red   – Server not running
 Green – Server is running
This indicator specifically targets Apache.

Inputs:
 state - Red
       - Green

Requires TImage:
 Create a new TImage
  set height = 19
  set width  = 19
  set name   = apache_img

=============================================================================}
procedure apache_indicator(state:String);
Var
  Bmp: TBitmap;
  clBorder       : TColor;
  clBack_running : TColor;
  clBack_stopped : TColor;

begin
 clBorder       := clBlack; // TColor($000000) Border colour
 clBack_running := clLime;  // TColor($00ff00) Server running
 clBack_stopped := clRed;   // TColor($0000ff) Server stopped

  // Not running Red
  If state='red' Then
   begin
    Bmp := TBitmap.Create;
    Bmp.Height := 19;
    Bmp.Width  := 19;
    Bmp.Canvas.Brush.Color := clBorder;
    Bmp.Canvas.FillRect(0,0,19,19);
    Bmp.Canvas.Brush.Color := clBack_stopped;
    Bmp.Canvas.FillRect(1,1,19,19);
    Main.apache_img.Canvas.Draw(0,0,Bmp);
    Bmp.free;
   end;

  // Running Green
  If state='green' Then
   begin
    Bmp := TBitmap.Create;
    Bmp.Height := 19;
    Bmp.Width  := 19;
    Bmp.Canvas.Brush.Color := clBorder;
    Bmp.Canvas.FillRect(0,0,19,19);
    Bmp.Canvas.Brush.Color := clBack_running;
    Bmp.Canvas.FillRect(1,1,19,19);
    Main.apache_img.Canvas.Draw(0,0,Bmp);
    Bmp.free;
   end;
end;
{----------------------------------------------------------------------------}

{*****************************************************************************
Enable Apache ssl in configuration file httpd.conf
=============================================================================}
procedure us_enable_apache_ssl;
var
  sList: TStringList;                // String list
  i: Integer;                        // Loop counter

begin

 //-- Get current configuration line from core\apache2\conf\httpd.conf and update
If FileExists(USF_APACHE_CNF) Then
 begin
   sList := TStringList.Create;           // Create object
   sList.LoadFromFile(USF_APACHE_CNF);    // Load file
   for i:=0 to sList.Count-1 do          // Scan file line by line
     begin
      if (sList[i]<>'') and ExecRegExpr('^#LoadModule ssl_module modules/mod_ssl.so', sList[i]) then      // Match found
        begin
         sList[i] := 'LoadModule ssl_module modules/mod_ssl.so'; // Update with new string
         Break;                                                  // Nothing else to do
        end;
     end;

   If FileIsWritable(USF_APACHE_CNF) Then
    begin
      sList.SaveToFile(USF_APACHE_CNF);   // Save changes back to file
      AP_SSL_ENABLED := True;             // SSL Enabled in config
    end;

  sleep(100);
  sList.Free;     // remove from memory
 end;
end;
{----------------------------------------------------------------------------}

{*****************************************************************************
Disable Apache ssl in configuration file httpd.conf
=============================================================================}
procedure  us_disable_apache_ssl;
var
  sList: TStringList;                // String list
  i: Integer;                        // Loop counter

begin
//-- Get current configuration line from core\apache2\conf\httpd.conf and update
If FileExists(USF_APACHE_CNF) Then
  begin
    sList := TStringList.Create;           // Create object
    sList.LoadFromFile(USF_APACHE_CNF);         // Load file
  for i:=0 to sList.Count-1 do          // Scan file line by line
    begin
     if (sList[i]<>'') and ExecRegExpr('^LoadModule ssl_module modules/mod_ssl.so', sList[i]) then      // Match found
       begin
        sList[i] := '#LoadModule ssl_module modules/mod_ssl.so'; // Update with new string
        Break;                                                   // Nothing else to do
       end;
    end;

    If FileIsWritable(USF_APACHE_CNF) Then
     begin
       sList.SaveToFile(USF_APACHE_CNF);   // Save changes back to file
       AP_SSL_ENABLED := False;            // SSL Disabled in config
     end;

   sleep(100);
   sList.Free;     // remove from memory
 end;

end;
{----------------------------------------------------------------------------}


//=== MySQL ===

{****************************************************************************
us_start_mysql_program: 
 Start MySQL server as a program
  Skip if already running.

 Note: Uses Process

  set my=!CD!\core\mysql\bin\mysqld.exe
  set my=!my! --defaults-file=!MYSQL_HOME!\my.ini
  set my=!my! --tmpdir=!CD!\tmp
  set my=!my! --datadir=!MYSQL_HOME!\data
  set my=!my! --innodb_data_home_dir=!MYSQL_HOME!\data
  set my=!my! --innodb_log_group_home_dir=!MYSQL_HOME!\data
=============================================================================}
procedure us_start_mysql_program;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
 mysql_exe: string;
begin
 if not MysqlRunning() then
  begin
    saftey_loop:=0;
     //--Run command string.
    AProcess := TProcess.Create(nil);                                              // Create new process

    AProcess.Executable := US_MYSQL_BIN + '\' + MY_EXE_NAME;                       // Executable to run

    //MYSQL command line parameters:
    //Parameters optionally set in configuration file us_config.ini [MYSQL] e.g
    //1) defaults-file=          No value remove parameter from command line
    //2) defaults-file=C:\fred   Use absolute path to folder fred
    //3) No parameter in us_config.ini use default path set in default_config_vars file

    IF USF_MYSQL_INI <>'' Then //If parameter with no value skip this section
     begin
      AProcess.Parameters.Add('--defaults-file="' + USF_MYSQL_INI  + '"');                  // Force use of a default file
     end;
    IF USP_TMPDIR <>'' Then //If parameter with no value skip this section
     begin
      AProcess.Parameters.Add('--tmpdir="' + USP_TMPDIR + '"');                             // Temp folder
     end;
    IF USP_DATADIR <>'' Then //If parameter with no value skip this section
     begin
      AProcess.Parameters.Add('--datadir="' + USP_DATADIR  + '"');                          // MySQL data folder
     end;
    IF USP_INNODB_DATA_HOME_DIR <>'' Then //If parameter with no value skip this section
     begin
     AProcess.Parameters.Add('--innodb_data_home_dir="' + USP_INNODB_DATA_HOME_DIR + '"');  // MySQL innodb data folder
     end;
    IF USP_INNODB_LOG_GROUP_HOME_DIR <>'' Then //If parameter with no value skip this section
     begin
      AProcess.Parameters.Add('--innodb_log_group_home_dir="' + USP_INNODB_LOG_GROUP_HOME_DIR + '"'); // MySQL innodb log folder
     end;

    AProcess.Options  := AProcess.Options + [poNoConsole];                         // Set option no console
    //AProcess.Priority := ppHigh;                                                 // The process runs at higher than normal priority.
    AProcess.Execute;                                                              // Run command
    AProcess.Free;

    Repeat                                         // Wait for MySQL server to start
     begin
       Application.ProcessMessages; //allow message processing
       sleep(100);
       saftey_loop := saftey_loop +1;              // Increment counter
       if saftey_loop > (USC_MY_StartSafetyTime*10) then
        begin
          us_MessageDlg('Warning','Failed to start '+US_MYMAR_TXT, mtWarning,[mbOk],0) ; //Display warning message
          Break;
        end;
     end;
    Until MysqlRunning();

    //=== If the MySQL version is 8 then the Console window does not close
    //=== with the pnoconsole. Use the Window ShowWindowHide to close it.

    If (US_MYMAR_TXT = 'MySQL') And (StrToInt(MY_SQL_VER) >= 8) Then
     begin
       sleep(1000);
       mysql_exe := US_MYSQL_BIN + '\' + MY_EXE_NAME;
       ShowWindow(FindWindow(nil,PChar(mysql_exe)),SW_HIDE);
     end;
  end;
end;
{--- End us_start_mysql_program --------------------------------------------}



{****************************************************************************
Kill MySQL running as a program
Global variables used:
  MY_EXE_NAME   - Exe name
=============================================================================}
procedure us_kill_mysql_program;
Var
 saftey_loop: Integer;

begin
 saftey_loop:=0;
 if MysqlRunning() then
  begin
    KillRunningProcess(MY_EXE_NAME);           // Kill MySQL server
  end;

  Repeat                                       // Wait for MySQL server to stop
    begin
      Application.ProcessMessages; //allow message processing
      sleep(100);
      saftey_loop := saftey_loop +1;           // Increment counter
      if saftey_loop > (USC_MY_StopSafetyTime*10) then
       begin
         us_MessageDlg('Warning','Failed to Kill '+US_MYMAR_TXT, mtWarning,[mbOk],0) ; //Display warning message
         Break;
       end;
    end;
   Until not MysqlRunning();
end;
{--- End us_kill_mysql_program ---------------------------------------------}


{****************************************************************************
Clean stop MySQL server - Stops MySQL program using a name and password
If this fails a user is offered the choice to Kill MySQL server (unclean).
Global variables used:
  MY_EXE_NAME   - Exe name
  USF_MYSQL_PWD      - Password file
=============================================================================}
procedure us_clean_stop_mysql_program;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
 Reply, BoxStyle: Integer;

 mysql_port: String;         //MySQL port
 pwdFile : TextFile;         // File containing mysql file
 mysql_pwd:  String;         //MySQL password

begin
  if MysqlRunning() then
   begin
    mysql_port  := UENV_MYSQL_TCP_PORT; //Get port configured
    saftey_loop :=0;

    //Get PWD from file
    AssignFile(pwdFile, USF_MYSQL_PWD);  // Text file handle
    Reset(pwdFile);                      // Open file for reading
    ReadLn(pwdFile, mysql_pwd);          // Read first line - get pid
    CloseFile(pwdFile);                  // Close file

   //--Run command string.
  AProcess := TProcess.Create(nil);                          // Create new process

  AProcess.Executable := US_MYSQL_BIN + '\mysqladmin.exe';   // Executable to run
  AProcess.Parameters.Add('--port=' + mysql_port);           // Parameter mysql server port
  AProcess.Parameters.Add('--user=root');                    // Parameter root user
  AProcess.Parameters.Add('--password=' + mysql_pwd);        // Parameter root user password
  AProcess.Parameters.Add('shutdown');                       // Parameter shutdown

  AProcess.Options     := AProcess.Options + [poNoConsole];   // Set option no console
  AProcess.Execute;                                           // Run command
  AProcess.Free;                                              // Release process

  Repeat                                          // Wait for MySQL server to stop
    begin
      Application.ProcessMessages; //allow message processing
      sleep(100);
      saftey_loop := saftey_loop +1;              // Increment counter
      if saftey_loop > (USC_MY_StopSafetyTime*10) then
       begin
        //Clean shutdown failed ask user if they want to use MySQL kill?
        BoxStyle := MB_ICONQUESTION + MB_YESNO;
        //Reply := Application.MessageBox(Pchar('Unable to perform clean shut-down. Kill process ), 'Kill MySQL', BoxStyle);
        Reply := Application.MessageBox(Pchar('Unable to perform clean shut-down. Kill process' ),Pchar('Kill '+US_MYMAR_TXT), BoxStyle);
        if Reply = IDYES then
           us_kill_mysql_program;
        Break;
       end;
     end;
   Until not MysqlRunning();
   end;
end;
{--- End us_clean_stop_mysql_program ----------------------------------------}

{*****************************************************************************
MySQL Server status indicator

A bi-state indicator showing current server status.
 Red   – Server not running
 Green – Server is running
This indicator specifically targets MySQL.

Inputs:
 state - Red
       - Green

Requires TImage:
 Create a new TImage
  set height = 15
  set width  = 15
  set name   = mysql_img

=============================================================================}
procedure mysql_indicator(state:String);
Var
  Bmp: TBitmap;
  clBorder       : TColor;
  clBack_running : TColor;
  clBack_stopped : TColor;

begin
 clBorder       := clBlack; // TColor($000000) Border colour
 clBack_running := clLime;  // TColor($00ff00) Server running
 clBack_stopped := clRed;   // TColor($0000ff) Server stopped

  // Not running Red
  If state='red' Then
   begin
    Bmp := TBitmap.Create;
    Bmp.Height := 19;
    Bmp.Width  := 19;
    Bmp.Canvas.Brush.Color := clBorder;
    Bmp.Canvas.FillRect(0,0,19,19);
    Bmp.Canvas.Brush.Color := clBack_stopped;
    Bmp.Canvas.FillRect(1,1,19,19);
    Main.mysql_img.Canvas.Draw(0,0,Bmp);
    Bmp.free;
   end;

  // Running Green
  If state='green' Then
   begin
    Bmp := TBitmap.Create;
    Bmp.Height := 19;
    Bmp.Width  := 19;
    Bmp.Canvas.Brush.Color := clBorder;
    Bmp.Canvas.FillRect(0,0,19,19);
    Bmp.Canvas.Brush.Color := clBack_running;
    Bmp.Canvas.FillRect(1,1,19,19);
    Main.mysql_img.Canvas.Draw(0,0,Bmp);
    Bmp.free;
   end;
end;
{--- ENd mysql_indicator ----------------------------------------------------}

{*****************************************************************************
Disply MySQl Command Prompt
Opens a dos window. Path selected is the MySQL root folder

Runs command:
  mysql.exe -h127.0.0.1 -P3306 -uroot -proot
=============================================================================}
procedure us_display_mysql_prompt;
Var
 AProcess: TProcess;
begin

 AProcess := TProcess.Create(nil); // Create process

 AProcess.Executable := 'cmd';                             // Executable to run
 AProcess.Parameters.Add('/T:B0');                         // Set background colour
 AProcess.Parameters.Add('/K');                            // Keep open

 AProcess.Parameters.Add('title');                         // A title is required
 AProcess.Parameters.Add('SERVER MySQL Command Console');  // Title

 AProcess.Parameters.Add('&&');                            // Start a new command line
 AProcess.Parameters.Add('cd');                            // Change directory
 AProcess.Parameters.Add(US_MYSQL_BIN + '\');              // Path to MySQL binary folder

 {Set environment variable}
 AProcess.Parameters.Add('&&');                            // Start a new command line
 AProcess.Parameters.Add('SET');                           // Set envior
 AProcess.Parameters.Add('MYSQL_HOME='+US_MYSQL);          // Path to MySQL my.ini folder

 AProcess.Parameters.Add('&&');                            // Start a new command line
 AProcess.Parameters.Add('mysql.exe');                     // run mysql.exe
 AProcess.Parameters.Add('--host=' + US_DB_HOST);          // Parameter server
 AProcess.Parameters.Add('--port=' + UENV_MYSQL_TCP_PORT); // Parameter mysql server port
 AProcess.Parameters.Add('--user=root');                   // Parameter root user
 AProcess.Parameters.Add('--password=' + MY_PWD);          // Parameter root user password

 AProcess.Execute; // execute detatched process command window remains visible
 AProcess.Free;    // free memory

end;
{--- End us_display_mysql_prompt -----------------------------------}

{====================================================================
 Save MySQL password to file.
====================================================================}
procedure us_save_mysql_password(my_pwd_in:string);
 var
  myFile : TextFile;

 begin
   AssignFile(myFile, USF_MYSQL_PWD); // Assign file handle
   ReWrite(myFile);                   // Open the file for write

   Write(myFile, my_pwd_in);          // Write password to file
   CloseFile(myFile);                 // Close the file
 end;
{--- End us_get_mysql_password() -----------------------------------}


{*****************************************************************************
us_create_mysql_database:
 Runs mysqladmin directely.
 Parameters to access MySQL server and create database are passed to mysqladmin
 Assumes the database does not exist and name is valid.
 Assumes server is running

 Note: Using mysqladmin avoids a user having to install a driver.
 Uses: Process
=============================================================================}
procedure us_create_mysql_database(database_name:string); 
Var
  AProcess: TProcess;   // Process handle
begin
  //--Run command string.
  AProcess := TProcess.Create(nil);                          // Create new process

  AProcess.Executable := US_MYSQL_BIN + '\mysqladmin.exe';   // Path to MySQL admin exe
  AProcess.Parameters.Add('--host=' + US_DB_HOST);           // Parameter server
  AProcess.Parameters.Add('--port=' + UENV_MYSQL_TCP_PORT);  // Parameter mysql server port
  AProcess.Parameters.Add('--user=root');                    // Parameter root user
  AProcess.Parameters.Add('--password=' + MY_PWD);           // Parameter root user password
  AProcess.Parameters.Add('create');                         // Parameter command to run
  AProcess.Parameters.Add(database_name);                    // Parmeter database name to delete

  AProcess.Options     := AProcess.Options + [poWaitOnExit];  // Set option wait to complete
  AProcess.ShowWindow  := swoHIDE;                            // Hide command window
  AProcess.Execute;                                           // Run command
  AProcess.Free;                                              // Release process
end;
{--- End us_create_mysql_database -------------------------------------------}

{*****************************************************************************
us_drop_mysql_database:
 Delete(Drop) MySQL Database
 Runs mysqladmin directely.
 Parameters to access MySQL server and drop database are passed to mysqladmin
 Assumes the database exists and server is running.

 Note: Using mysqladmin avoids a user having to install a driver.
 Uses: Process
=============================================================================}
procedure us_drop_mysql_database(database_name:string);
Var
  AProcess: TProcess;   // Process handle
begin
 //--Run command string.
 AProcess := TProcess.Create(nil);                          // Create new process

 AProcess.Executable := US_MYSQL_BIN + '\mysqladmin.exe';   // Executable to run
 AProcess.Parameters.Add('--host=' + US_DB_HOST);           // Parameter server
 AProcess.Parameters.Add('--port=' + UENV_MYSQL_TCP_PORT);  // Parameter mysql server port
 AProcess.Parameters.Add('--user=root');                    // Parameter root user
 AProcess.Parameters.Add('--password=' + MY_PWD);           // Parameter root user password
 AProcess.Parameters.Add('--force');                        // Parameter command to delete database
 AProcess.Parameters.Add('drop');                           // drop deletes db
 AProcess.Parameters.Add(database_name);                    // Parmeter database name to delete

 AProcess.Options     := AProcess.Options + [poWaitOnExit];  // Set option wait to complete
 AProcess.ShowWindow  := swoHIDE;                            // Hide command window
 AProcess.Execute;                                           // Run command
 AProcess.Free;                                              // Release process
end;
{--- End us_drop_mysql_database ---------------------------------------------}


//=== MySQL Change root password ===

{=============================================================================
MySQL Change root password:
 1) A new mysql-init.txt file is used for updating the old MySQL root password.
    The file is run during MSQL server start-up and contains the following
    sql instructions:

    UPDATE mysql.user SET Password = PASSWORD('newpass') WHERE User = 'root';
    UPDATE mysql.user SET Password = PASSWORD('newpass') WHERE User = 'pma';
    FLUSH PRIVILEGES;

 2) Start MySQL server

    Command line format:
    C:\UniServerZ\core\mysql\bin\mysqld_z.exe --defaults-file=C:\UniServerZ\core\mysql\my.ini

 3) Run mySQL with the user and the password providing the input file to change the password
    Command Line Format:
    mysql.exe --user=root --password=root --execute="SOURCE mysql-init.txt"

 4) Passwords changed kill the MySQL server.

  Changes:
  --------
  2019-09-07 : SudeepJD: Added in the MySQL8 support as it needs to be handled differntly
  2021-04-06 : SudeepJD: MySQL5.7+ and MySQL8 are now handled the same way using ALTER instead of UPDATE
               Updated the method of changing the password since we know the old password
=============================================================================}
procedure mysql_change_root_password(new_password:string);
var
  FileVar1      : TextFile;    // Text File handle
  AProcess      : TProcess;    // Process handle
  saftey_loop   : Integer;     // Loop counter
  failed        : Boolean;     // Failed to set password
  root_password : String;      //The existing password
begin
  failed := False;                          // Assume not failed

  root_password := us_get_mysql_password();

  //1==Create new mysql-init.txt
  Assign(FileVar1,USF_MYSQL_TEMP_SQL);      // Assign file
  ReWrite(FileVar1);                        // Create file for writing

  Writeln(FileVar1, 'FLUSH PRIVILEGES;');
  If (US_MYMAR_TXT = 'MySQL') Then
   Begin
    Writeln(FileVar1, 'ALTER USER ''root''@''localhost'' IDENTIFIED WITH mysql_native_password BY '''+new_password+''';');
    Writeln(FileVar1, 'ALTER USER ''pma''@''localhost'' IDENTIFIED WITH mysql_native_password BY '''+new_password+''';');
   End
  Else
   Begin
    Writeln(FileVar1, 'ALTER USER ''root''@''localhost'' IDENTIFIED BY '''+new_password+''';');
    Writeln(FileVar1, 'ALTER USER ''pma''@''localhost'' IDENTIFIED BY '''+new_password+''';');
   End;
  Writeln(FileVar1, 'FLUSH PRIVILEGES;');

  CloseFile(FileVar1);                     // Close file
  sleep(100);                              // Wait for file to be created

  //2==Start MySQL server
  //   File is run during start-up and passwords changed.
  AProcess := TProcess.Create(nil);                                              // Create new process

  AProcess.Executable := US_MYSQL_BIN + '\' + MY_EXE_NAME;                       // Executable to run
  AProcess.Parameters.Add('--defaults-file="' + USF_MYSQL_INI+ '"');             // Force use of default file
  //AProcess.Parameters.Add('--init-file="' + USF_MYSQL_TEMP_SQL+ '"');            // Parameter mysql mysql-init.txt file

  AProcess.Options  := AProcess.Options + [poNoConsole];                         // Set option no console
  //AProcess.Priority := ppHigh;                                                 // The process runs at higher than normal priority.
  AProcess.Execute;                                                              // Run command
  AProcess.Free;

  saftey_loop:=0;
  Repeat                                         // Wait for MySQL server to start
   begin
    Application.ProcessMessages;                 // allow message processing
    sleep(100);
     saftey_loop := saftey_loop +1;              // Increment counter
     if saftey_loop > (USC_MY_StartSafetyTime*10) then
      begin
        us_MessageDlg('Warning','Failed to start '+US_MYMAR_TXT, mtWarning,[mbOk],0) ; //Display warning message
        Failed := True;
        Break;
      end;
   end;
  Until MysqlRunning();

  //Check MySQL server is ready
  If not Failed then
   begin
     saftey_loop:=0;
     Repeat                                         // Wait for MySQL server to be ready
      begin
       Application.ProcessMessages;                 // allow message processing
       sleep(100);
        saftey_loop := saftey_loop +1;              // Increment counter
        if saftey_loop > (USC_MY_StartSafetyTime*10) then
         begin
           us_MessageDlg('Warning',US_MYMAR_TXT+' server failed to become ready', mtWarning,[mbOk],0) ; //Display warning message
           Failed := True;
           Break;
         end;
      end;
     Until MysqlServerReady(root_password);
   end;

  //3)==Run the sql file and execute the Init file
  If not Failed then
   Begin
     AProcess := TProcess.Create(nil);   // Create new process

      AProcess.Executable := US_MYSQL_BIN + '\mysql.exe';                   // Executable to run
      AProcess.Parameters.Add('--user=root');                               // User root
      AProcess.Parameters.Add('--password=' + root_password);       // User password
      AProcess.Parameters.Add('--execute="SOURCE '+USF_MYSQL_TEMP_SQL+'"'); // mysql-init.txt

      AProcess.Options  := AProcess.Options + [poNoConsole];                // Set option no console
      AProcess.Execute;                                                     // Run command
      AProcess.Free;
   End;

  //3)==MySQL running kill the MySQL server.
  Sleep(4000);
  us_kill_mysql_program;                  // Kills MySQL server

  //4)==Delete temp file mysql-init.txt
  If FileExists(USF_MYSQL_TEMP_SQL) Then
    //DeleteFile(Pchar(USF_MYSQL_TEMP_SQL));

  //5)==Update parameters
  If Failed then //Failed to change root password
   begin
     us_MessageDlg(US_MYMAR_TXT+' Info','Failed to change root password. No action taken', mtcustom,[mbOk],0) ; //Display information message
   end
  Else           //Root password changed
   begin
    us_save_mysql_password(new_password);   // Save new password to password file
    MY_PWD := new_password;                 // Updated password global variable
    us_MessageDlg(US_MYMAR_TXT+' Info',US_MYMAR_TXT+' root password changed', mtcustom,[mbOk],0) ; //Display information message
   End;
end;
{--- End mysql_change_root_password -----------------------------------------}

//=== MySQL restore root password ===

{=============================================================================
mysql_restore_root_password:
 A user may forget the root password or delete the root user or change
 root user privileges. This procedure restores password to root and
 either creates or updates privileges.

 A list of privileges is obtained by running the following using mysql.exe
 SELECT * FROM mysql.user WHERE User = 'root'\G;
 results from the above are used to create a restore file mysql-init.txt.

 To restore password and privileges MySQL server is started with no-privileges using:
 mysqld_z.exe --defaults-file="C:\UniserverZ\core\mysql\my.ini" --skip-grant-tables
 On MYSQL8 this needs to be started with --shared-memory as well or the server will
 not start

 With server running the following line is executed which runs the restore file.
 mysql.exe --user=root --execute="SOURCE C:\UniserverZ\core\mysql\bin\mysql-init.txt"

 Following steps restores the password and privileges
 
 1) Kill server if running

 2) A new mysql-init.txt file is created this is used for restoring the MySQL root user.

 3) Start MySQL server with skip grant tables
    mysqld_z.exe --defaults-file="C:\UniserverZ\core\mysql\my.ini" --skip-grant-tables

 4) Insert and Replace the root and pma if they were deleted

 5) Kill the server

 6) Update parameters

 7) Delete temp file mysql-init.txt

 Changes:
  --------
  2019-09-07 : SudeepJD: Added in the MySQL8 support as it needs to be handled differently
                         MySQL8 does not have a Password in the Database but uses the identified by
  2021-04-07 : SudeepJD: MariaDB post 10.5 has made mysql.user a view, so it cannot be modified
                         This has moved to global_privs and the restore needs to be done differently.
=============================================================================}
procedure mysql_restore_root_password;
var
  FileVar1 : TextFile;  // Text File handle
  AProcess : TProcess;  // Process handle
  str1     : string;
  str2     : string;
  str3     : string;
  str4     : string;
  str5     : string;
  str6     : string;
  str7     : string;
  str8     : string;
  str9     : string;
begin

  //1==Kill server
  us_kill_mysql_program;                    // Check server running. If running kill it

  //2===Set sql strings to write to file. This is based on the results obtained from list of privileges
  //    Specific privileges are named a set not the root password is calculated using Password(''root'') 

  If (US_MYMAR_TXT='MySQL') Then
   Begin
    str1 := 'use mysql;';
    str2 := 'INSERT IGNORE INTO user SET user = ''root'', plugin = ''mysql_native_password'', authentication_string = '''', host = ''localhost'', select_priv = ''y'', insert_priv = ''y'', update_priv = ''y'', delete_priv = ''y'', create_priv = ''y'', drop_priv = ''y'', reload_priv = ''y'', shutdown_priv = ''y'', process_priv = ''y'', file_priv = ''y'', grant_priv = ''y'', references_priv = ''y'', index_priv = ''y'', alter_priv = ''y'', show_db_priv = ''y'', super_priv = ''y'', create_tmp_table_priv = ''y'', lock_tables_priv = ''y'', execute_priv = ''y'', repl_slave_priv = ''y'', repl_client_priv = ''y'', create_view_priv = ''y'', show_view_priv = ''y'', create_routine_priv = ''y'', alter_routine_priv = ''y'', create_user_priv = ''y'', Event_priv = ''Y'', Trigger_priv = ''Y'', Create_tablespace_priv = ''Y'', ssl_type = '''', ssl_cipher ='''', x509_issuer = '''', x509_subject = '''', max_questions = ''0'', max_updates = ''0'', max_connections = ''0'', max_user_connections = ''0'';';
    str3 := 'REPLACE INTO user SET       user = ''root'', plugin = ''mysql_native_password'', authentication_string = '''', host = ''localhost'', select_priv = ''y'', insert_priv = ''y'', update_priv = ''y'', delete_priv = ''y'', create_priv = ''y'', drop_priv = ''y'', reload_priv = ''y'', shutdown_priv = ''y'', process_priv = ''y'', file_priv = ''y'', grant_priv = ''y'', references_priv = ''y'', index_priv = ''y'', alter_priv = ''y'', show_db_priv = ''y'', super_priv = ''y'', create_tmp_table_priv = ''y'', lock_tables_priv = ''y'', execute_priv = ''y'', repl_slave_priv = ''y'', repl_client_priv = ''y'', create_view_priv = ''y'', show_view_priv = ''y'', create_routine_priv = ''y'', alter_routine_priv = ''y'', create_user_priv = ''y'', Event_priv = ''Y'', Trigger_priv = ''Y'', Create_tablespace_priv = ''Y'', ssl_type = '''', ssl_cipher ='''', x509_issuer = '''', x509_subject = '''', max_questions = ''0'', max_updates = ''0'', max_connections = ''0'', max_user_connections = ''0'' ;';
    str4 := 'INSERT IGNORE INTO user SET user = ''pma'', plugin = ''mysql_native_password'', authentication_string = '''', host = ''localhost'', select_priv = ''y'', insert_priv = ''y'', update_priv = ''y'', delete_priv = ''y'', create_priv = ''y'', drop_priv = ''y'', reload_priv = ''y'', shutdown_priv = ''y'', process_priv = ''y'', file_priv = ''y'', grant_priv = ''y'', references_priv = ''y'', index_priv = ''y'', alter_priv = ''y'', show_db_priv = ''y'', super_priv = ''y'', create_tmp_table_priv = ''y'', lock_tables_priv = ''y'', execute_priv = ''y'', repl_slave_priv = ''y'', repl_client_priv = ''y'', create_view_priv = ''y'', show_view_priv = ''y'', create_routine_priv = ''y'', alter_routine_priv = ''y'', create_user_priv = ''y'', Event_priv = ''y'', Trigger_priv = ''Y'', Create_tablespace_priv = ''Y'', ssl_type = '''', ssl_cipher ='''', x509_issuer = '''', x509_subject = '''', max_questions = ''0'', max_updates = ''0'', max_connections = ''0'', max_user_connections = ''0'';';
    str5 := 'REPLACE INTO user SET       user = ''pma'', plugin = ''mysql_native_password'', authentication_string = '''', host = ''localhost'', select_priv = ''y'', insert_priv = ''y'', update_priv = ''y'', delete_priv = ''y'', create_priv = ''y'', drop_priv = ''y'', reload_priv = ''y'', shutdown_priv = ''y'', process_priv = ''y'', file_priv = ''y'', grant_priv = ''y'', references_priv = ''y'', index_priv = ''y'', alter_priv = ''y'', show_db_priv = ''y'', super_priv = ''y'', create_tmp_table_priv = ''y'', lock_tables_priv = ''y'', execute_priv = ''y'', repl_slave_priv = ''y'', repl_client_priv = ''y'', create_view_priv = ''y'', show_view_priv = ''y'', create_routine_priv = ''y'', alter_routine_priv = ''y'', create_user_priv = ''y'', Event_priv = ''y'', Trigger_priv = ''Y'', Create_tablespace_priv = ''Y'', ssl_type = '''', ssl_cipher ='''', x509_issuer = '''', x509_subject = '''', max_questions = ''0'', max_updates = ''0'', max_connections = ''0'', max_user_connections = ''0'';';
    str6 := 'FLUSH PRIVILEGES;';
    str7 := 'ALTER USER ''root''@''localhost'' IDENTIFIED WITH mysql_native_password BY ''root'';';
    str8 := 'ALTER USER ''pma''@''localhost'' IDENTIFIED WITH mysql_native_password BY ''root'';';
    str9 := 'FLUSH PRIVILEGES;';
   End
  Else
   Begin
    str1 := 'use mysql;';
    str2 := 'INSERT IGNORE INTO global_priv (Host, User, Priv) VALUES (''localhost'', ''root'', ''{"access":0,"plugin":"mysql_native_password","authentication_string":"","password_last_changed":0}'');';
    str3 := 'INSERT IGNORE INTO global_priv (Host, User, Priv) VALUES (''localhost'', ''pma'', ''{"access":0,"plugin":"mysql_native_password","authentication_string":"","password_last_changed":0}'');';
    str4 := 'FLUSH PRIVILEGES;';
    str5 := 'ALTER USER ''root''@''localhost'' IDENTIFIED BY ''root'';';
    str6 := 'ALTER USER ''pma''@''localhost'' IDENTIFIED BY ''root'';';
    str7 := 'GRANT ALL ON *.* TO ''root''@''localhost'';';
    str8 := 'GRANT ALL ON *.* TO ''pma''@''localhost'';';
    str9 := 'FLUSH PRIVILEGES;';
   End;

  //2==Write above strings to file mysql-init.txt this is used to restore user
   Assign(FileVar1,USF_MYSQL_TEMP_SQL);      // Assign file
   ReWrite(FileVar1);                        // Create file for writting
   Writeln(FileVar1, str1);
   Writeln(FileVar1, str2);
   Writeln(FileVar1, str3);
   Writeln(FileVar1, str4);
   Writeln(FileVar1, str5);
   Writeln(FileVar1, str6);
   Writeln(FileVar1, str7);
   Writeln(FileVar1, str8);
   Writeln(FileVar1, str9);

   CloseFile(FileVar1);                     // Close file
   Sleep(100);                              // Wait for file to be created

  //3)== Start MySQL server with skip Grant tables
  If us_start_mysql_skip_grants() Then
    Begin   // Server running

      //==4)Run sql file mysql-init.txt
      AProcess := TProcess.Create(nil);   // Create new process

      AProcess.Executable := US_MYSQL_BIN + '\mysql.exe';                   // Executable to run
      AProcess.Parameters.Add('--execute="SOURCE '+USF_MYSQL_TEMP_SQL+'"'); // mysql-init.txt

      AProcess.Options  := AProcess.Options + [poNoConsole];                // Set option no console
      AProcess.Execute;                                                     // Run command
      AProcess.Free;     

      //5)==Wait a short time and kill server
      Sleep(4000);
      us_kill_mysql_program;                 // Kills MySQL server     

      //6)==Root password changed
      us_save_mysql_password('root');        // Save 'root' password to password file
      MY_PWD := 'root';                      // Updated password global variable
      us_MessageDlg(US_MYMAR_TXT+' Info',US_MYMAR_TXT+' root user restored', mtcustom,[mbOk],0) ; //Display information message
    End
  Else      // Failed to start server
    Begin
      us_MessageDlg(US_MYMAR_TXT+' Info','Failed to restore root user. No action taken', mtcustom,[mbOk],0) ; //Display information message
    End;

  //7)==Delete temp file mysql-init.txt
  If FileExists(USF_MYSQL_TEMP_SQL) Then
    DeleteFile(Pchar(USF_MYSQL_TEMP_SQL));

end;
{---End mysql_restore_root_password -----------------------------------------}


//=== Display Server Console ===

{*****************************************************************************
Display Server Command Console
Opens a CMD (dos) window.
=============================================================================}
procedure us_display_server_console;
Var
 AProcess: TProcess;
begin
 AProcess := TProcess.Create(nil); // Create process

 AProcess.Executable := 'cmd';                              // Executable to run
 AProcess.Parameters.Add('/T:B0');                          // Set background colour
 AProcess.Parameters.Add('/K');                             // Keep open

 AProcess.Parameters.Add('title');                          // A title is required
 AProcess.Parameters.Add('SERVER Command Console');         // Title

 AProcess.Parameters.Add('&&');                             // Start a new command line
 AProcess.Parameters.Add('cd');                             // Change directory
 AProcess.Parameters.Add(UniConPath);                       // to server root e.g C:\UniServer

 //Execute command
 AProcess.Execute; // execute detatched process command window remains visible
 AProcess.Free;    // free memory

end;
{----------------------------------------------------------------------------}


//=== Perl

{*****************************************************************************
 This procedure updates Perl shebang to either a Windows or Unix shebang.
 For the following files in folder cgi-bin
   *.pl
   *.pm
   *.cgi

 Input:
 US_WINDOWS_SHEBANG - Replaces Unix shebang with Windows version
 US_UNIX_SHEBANG    - Replaces Windows shebang with Unix version

 // Perl Shebangs
 US_SHEBANG             := '#!';               // #!
 US_UNIX_SHEBANG        := '#!/usr/bin/perl';  // #!/usr/bin/perl
 US_WINDOWS_SHEBANG     := '#!perl';           // #!perl

 FindAllFiles: Uses  FileUtil
 ExecRegExpr : Uses  RegExpr
=============================================================================}
procedure force_shebang_update(NEW_SHEBANG:string);

var
  sList_Files   :TStringList;    // Save list of files
  sList         :TStringList;    // Save file to list
  i             :integer;        // Loop counter
begin
  If DirectoryExists(US_CGI_BIN) Then   // CGI Folder exists
   begin
    sList        := TStringList.Create;  // Create object

  //==Get all files of type *.pl
  sList_Files  := FindAllFiles(US_CGI_BIN, '*.pl',true);  //Create list. Search subs

   //Scan file list
   for i:=0 to sList_Files.Count-1 do
     begin
       sList.Clear;                                   // Remove old list content
       sList.LoadFromFile(sList_Files[i]);            // Load file
       If (sList[i]<>'') and ExecRegExpr('^'+US_SHEBANG, sList[0])  then // Shebang found in first line
         begin
            sList[0] := NEW_SHEBANG;                  // Set new shebang

            If FileIsWritable(sList_Files[i]) Then
               sList.SaveToFile(sList_Files[i]);      // Save new values to file

            sleep(100);
         end;
     end;//End scan list

   //==Get all files of type *.pm
   sList_Files.Clear;                                       // Remove old content
   sList_Files   := FindAllFiles(US_CGI_BIN, '*.pm',true);  // Search subs

   //Scan file list
   for i:=0 to sList_Files.Count-1 do
     begin
       sList.Clear;                                  // Remove old list content
       sList.LoadFromFile(sList_Files[i]);           // Load file
       If (sList[i]<>'') and ExecRegExpr('^'+US_SHEBANG, sList[0]) then // Shebang found in first line
         begin
            sList[0] := NEW_SHEBANG;                // Set new shebang

            If FileIsWritable(sList_Files[i]) Then
               sList.SaveToFile(sList_Files[i]);      // Save new values to file

            sleep(100);
         end;
     end;//End scan list

   //==Get all files of type *.cgi
   sList_Files.Clear;                                       // Remove old content
   sList_Files  := FindAllFiles(US_CGI_BIN, '*.cgi',true);  // Search subs

   //Scan file list
   for i:=0 to sList_Files.Count-1 do
     begin
       sList.Clear;                                   // Remove old list content
       sList.LoadFromFile(sList_Files[i]);            // Load file
       If (sList[i]<>'') and ExecRegExpr('^'+US_SHEBANG, sList[0]) then  // Shebang found in first line
         begin
            sList[0] := NEW_SHEBANG;                  // Set new shebang

            If FileIsWritable(sList_Files[i]) Then
               sList.SaveToFile(sList_Files[i]);      // Save new values to file

            sleep(100);
         end;
     end;//End scan list

   //==Clean up
   sList.Free;       //Release memory
   sList_Files.Free; //Release memory

  end; //DirectoryExists(US_CGI_BINL)


end;
{----------------------------------------------------------------------------}

//=== PHP

{*****************************************************************************
Disable all PHP accelerators in selected PHP ini configuration file
=============================================================================}
procedure disable_all_php_accelerators;
var
 sList                  :TStringList;  // String list
 i                      :integer;      // Loop counter
 selected_ini_file      :String;       // Full path selected php config file
begin
 selected_ini_file :=US_CORE+'\'+UENV_PHP_SELECT+'\'+UENV_PHP_INI_SELECT;     // Full path selected php config file

 sList  := TStringList.Create;            // Create object
 sList.LoadFromFile(selected_ini_file);   // Load PHP ini configuration file

 //Scan sList. add ; to disable accelerator
 for i:=0 to sList.Count-1 do
   begin
     // Zend OpCache code
     If (sList[i]<>'') and ExecRegExpr('^[^;].*'+QuoteRegExprMetaChars(ZENDOPCACHE_DLL)+'.*$', sList[i]) Then  sList[i] := ';'+sList[i];
   end; //End scan list

  If FileIsWritable(selected_ini_file) Then
     sList.SaveToFile(selected_ini_file);  // Save new values to file


sList.Free;                                // Remove from memory
end;
{--End disable_all_php_accelerators -----------------------------------------}

{*****************************************************************************
Enable named PHP accelerator in selected PHP ini configuration file
Input:
  apc
  eaccelerator
  xcache
  zop
=============================================================================}
procedure enable_php_accelerator(accelerator:String);
var
 sList                  :TStringList;  // String list
 i                      :integer;      // Loop counter
 selected_ini_file      :String;       // Full path selected php config file
begin
 selected_ini_file :=US_CORE+'\'+UENV_PHP_SELECT+'\'+UENV_PHP_INI_SELECT; // Full path selected php config file

 sList  := TStringList.Create;            // Create object
 sList.LoadFromFile(selected_ini_file);   // Load PHP ini configuration file

 //Scan sList. add ; to disable accelerator

  // Zend OpCache
  If accelerator = 'zop' Then
  begin
   for i:=0 to sList.Count-1 do
      begin
         // Zend OpCache code
        If (sList[i]<>'') and ExecRegExpr('^[;].*'+QuoteRegExprMetaChars(ZENDOPCACHE_DLL)+'.*$', sList[i]) Then
          sList[i] := ReplaceRegExpr('^;', sList[i], '', False); //Remove ; to enable
      end; //End scan list
  end;

 If FileIsWritable(selected_ini_file) Then
    sList.SaveToFile(selected_ini_file);  // Save new values to file

 sList.Free;                              // Remove from memory
end;
{--End enable_php_accelerator -----------------------------------------------}

{*****************************************************************************
View selected PHP Accelerator, control panel
=============================================================================}
procedure view_selected_acc_control_panel;
var
  url :string;
begin

   // Zend OpCache code
   If Main.MMSS_php_acc_zop.Checked Then
    Begin
      url := 'http://' + UENV_US_SERVERNAME + ':' + UENV_AP_PORT + '/us_extra/zend_optimizer_control.php';
      browser_display_url(url); //Display URL in default/portable browser
    End;
end;
{--End view_selected_acc_control_panel --------------------------------------}

//=== Message Dialogs

{*****************************************************************************
 Reminder dialog - Restart apache.
=============================================================================}
procedure msg_restart_apache;
var
 title:string;
 str:string;
begin
 If ApacheRunning() then
  begin
    title     := 'Apache running';
    str:='';
    str := str + 'Apache is running' + sLineBreak;
    str := str + 'For changes to take effect restart the Apache server.';

    us_MessageDlg(title, str, mtInformation,[mbOk],0) ;  //Display message
  end;
end;
{-- End msg_restart_apache --------------------------------------------------}


{*****************************************************************************
MySQL port change
Warn user MySQL server is running and to stop server.
=============================================================================}
procedure msg_stop_mysql_port_change;
var
 title:string;
 str:string;
begin
    title     := US_MYMAR_TXT+' running';
    str:='';
    str := str + US_MYMAR_TXT+' is running! You need to stop the '+US_MYMAR_TXT+' server' + sLineBreak;
    str := str + 'before its port can be changed.' + sLineBreak+ sLineBreak;

    str := str + 'a) Stop the '+US_MYMAR_TXT+' server.' + sLineBreak;
    str := str + 'b) Run this menu item again to change port.';

    us_MessageDlg(title, str, mtInformation,[mbOk],0) ;  //Display message
end;
{--End msg_stop_mysql_port_change -------------------------------------------}


{*****************************************************************************
MySQL root password change
Warn user MySQL server is running and to stop server.
=============================================================================}
procedure msg_stop_mysql_pwd_change;
var
 title:string;
 str:string;
begin
    title     := US_MYMAR_TXT+' running';
    str:='';
    str := str + US_MYMAR_TXT+' is running! You need to stop the '+US_MYMAR_TXT+' server' + sLineBreak;
    str := str + 'before you can change the '+US_MYMAR_TXT+' root password.' + sLineBreak+ sLineBreak;

    str := str + 'a) Stop the '+US_MYMAR_TXT+' server.' + sLineBreak;
    str := str + 'b) Run this menu item again to change root password.';

    us_MessageDlg(title, str, mtInformation,[mbOk],0) ;  //Display message
end;
{--End msg_stop_mysql_pwd_change -------------------------------------------}


{*****************************************************************************
Password file contains_defaults
Warn user file contains root:root default name and password.
=============================================================================}
procedure msg_password_file_contains_defaults;
var
 title:string;
 str:string;
begin
    title     := 'Defaults detected';
    str:='';
    str := str + 'Password file contains root:root default name and password.' + sLineBreak + sLineBreak;
    str := str + 'To enable password protection delete these defaults.';

    us_MessageDlg(title, str, mtInformation,[mbOk],0) ;  //Display message
end;
{--End msg_password_file_contains_defaults ----------------------------------}

{*****************************************************************************
Password file empty
Warn user no enteries in file
=============================================================================}
procedure msg_password_file_empty;
var
 title:string;
 str:string;
begin
    title     := 'No Entries';
    str:='';
    str := str + 'Password file is empty.' + sLineBreak + sLineBreak;
    str := str + 'One or more name password pairs are required.';

    us_MessageDlg(title, str, mtInformation,[mbOk],0) ;  //Display message
end;
{--End msg_password_file_empty -------------------------------------------}


//=== GO PEAR ===

{===============================================================
 update_go_pear_config:
 This procedure updates paths in go-pear configuration file.
 Inaddition updates PHP selected

 For portability paths upto 'home' and 'core' require updating.
 Paths are not tracked hence this procedure is always run at start-up.
 In addition run if a user selects a different PHP version.
 Path updates use a regex to isolate sections of a string and
 paths chaged accordingly.
----------------------------------------------------------------}
procedure us_update_go_pear_config;
var
  sList:    TStringList; // String list
  sList2:   TStringList; // String list
  i:        integer;     // Loop counter
  s_value:  Integer;     // Length
  str:      string;
  RegexObj: TRegExpr;    // Object
  pt1:String; //Path component parts
  pt2:String;
  pt3:String;
  pt4:String;
  pt5:String;
  pt6:String;
  pt7:String;
  pt8:String;

begin
 If FileExists(USF_GO_PEAR_CONFIG) Then
  begin

    //==Get go-pear configuration file
    sList       := TStringList.Create;       // Create object
    sList2      := TStringList.Create;       // Create object
    sList.LoadFromFile(USF_GO_PEAR_CONFIG);  // Load file

    //=== Serialize string (sList[1]) save to sList2 which splits the string
    sList2.StrictDelimiter := true;   // Use only the defined delimiter
    sList2.Delimiter := ';';          // Define delimiter - split at ;
    sList2.DelimitedText := sList[1]; // Assign serialized string and split

    //== Update paths using regex selection
    RegexObj := TRegExpr.Create;           // Create regex obj

    //== Update home path
    RegexObj.Expression := '^(s:)(\d+)(:")(.+)(\\home\\)(.+)"$'; // Set search pattern

    for i:=0 to sList2.Count-1 do // Scan list
    begin
     if (sList2[i]<>'') and RegexObj.Exec(sList2[i]) then                            // Match found
       begin
        //Build new line:
        pt1 := 's:';              // s:
        pt2 := RegexObj.Match[2]; // digits for s
        pt3 := ':"';              // :"
        pt4 := UniConPath;        // Set new path e.g C:\fred\UniServerZ
        pt5 := '\home\';          // \home\  folder
        pt6 := RegexObj.Match[6]; // remainder of path us_pear\PEAR\docs
        pt7 := '"';               // Last quote "
        s_value  := Length(pt4) + Length(pt5)+Length(pt6);    // Length of string
        pt2      := IntToStr(s_value);                        // Convert to string
        sList2[i] := pt1 + pt2 + pt3 + pt4 + pt5 + pt6 + pt7; // Create new line
       end;
    end;
    //== Update core path note this also includes php version selected.
    RegexObj.Expression := '^(s:)(\d+)(:")(.+)(\\core\\)(php\d+)(.+)"$'; // Set search pattern

    for i:=0 to sList2.Count-1 do      // Scan list
    begin
     if (sList2[i]<>'') and RegexObj.Exec(sList2[i]) then  // Match found
       begin
        //Build new line:
        pt1 := 's:';              // s:
        pt2 := RegexObj.Match[2]; // digits for s
        pt3 := ':"';              // :"
        pt4 := UniConPath;        // Set new path e.g C:\fred\UniServerZ
        pt5 := '\core\';          // \core\  folder
        pt6 := UENV_PHP_SELECT;   // PHP Version selected
        pt7 := RegexObj.Match[7]; // remainder of path us_pear\PEAR\docs
        pt8 := '"';               // Last quote "
        s_value := Length(pt4) + Length(pt5)+Length(pt6)+Length(pt7); // Length of string
        pt2     := IntToStr(s_value);                                 // Convert to string
        sList2[i] := pt1 + pt2 + pt3 + pt4 + pt5 + pt6 + pt7 + pt8;   // Create new line
       end;
    end;
     RegexObj.Free; // Release memory

    //===Build new configuration string
    str :='';                           // Clear string
    for i:=0 to sList2.Count-2 do       // Built new string
      begin
        str := str + sList2[i] +';' ;   // Build string re-insert ;
      end;
    str := str + sList2[sList2.Count-1]; // Last line note no ';'

    //Save new conig string
    sList[1] := str;    // Assign new config string

    //Save new file
    sList.SaveToFile(USF_GO_PEAR_CONFIG);  // Save new values to file

    //Clean-up
    sList.Free;    // Remove from memory
    sList2.Free;   // Remove from memory
  end; //If file exists
end; 

{--- END us_update_go_pear_config ---------------------------------}

//=== PALE MOON BROWSER ===

{****************************************************************************
Start PaleMoon Browser
 Skip if already running.
 PALE_MOONL_EXE  Palemoon-Portable.exe Portable Palemoon launcher start-up exe
Note: Uses Process
=============================================================================}
procedure us_start_palemoon;
Var
 AProcess: TProcess;
 saftey_loop: Integer;

begin
 If DirectoryExists(US_PALE_MOON) Then
  begin
   if not PaleMoonPortableRunning() then
    begin
    //--Run command string.
    AProcess := TProcess.Create(nil);                         // Create new process

    AProcess.Executable := USF_PALE_MOON_EXE;                 // Executable to run
    AProcess.Parameters.Add('file:///'+USF_REFERENCE_HTML);   // Parameter ref html file
    AProcess.Options  := AProcess.Options + [poNoConsole];    // Set option no console
    AProcess.Execute;                                         // Run command
    AProcess.Free;                                            // Release process

    saftey_loop :=0;
    Repeat   // Wait for Pale moon to start
      begin
        Application.ProcessMessages;      // allow message processing
        sleep(100);
        saftey_loop := saftey_loop +1;    // Increment counter
        if saftey_loop > (USC_PM_StartSafetyTime*10) then
         begin
           us_MessageDlg('Warning','Failed to start Pale Moon Browser', mtWarning,[mbOk],0) ; //Display warning message
           Break;
         end;
      end;
    Until PaleMoonPortableRunning();

    //Pale Moon now running confirm it is ready
    //Wait for window and read unique title bar
    saftey_loop :=0;                  // Reset flag

    Repeat   // Wait for Pale moon - ready
      begin
        Application.ProcessMessages;      // allow message processing
        sleep(1000);
        saftey_loop := saftey_loop +1;    // Increment counter
        if saftey_loop > (USC_PM_ReadySafetyTime) then
         begin
           us_MessageDlg('Warning','Failed to start Pale Moon Browser', mtWarning,[mbOk],0) ; //Display warning message
           Break;
         end;
      end;
    Until palemoon_ready();
    end;
  end;//DirectoryExists(US_PALE_MOON)
 end;
{--- End us_start_palemoon -------------------------------------------------}



{****************************************************************************
Kill Pale Moon Browser
Global variables used:
  PALE_MOON_EXE  palemoon.exe   Palemoon execuitable
=============================================================================}
procedure us_kill_palemoon;
Var
 saftey_loop: Integer;

begin
 if PaleMoonPortableRunning() then
  saftey_loop:=0;
  begin
      KillRunningProcess(PALE_MOON_EXE);    //Kill Pale Moon program
  end;

  Repeat                                   // Wait for Pale Moon to stop
    begin
      Application.ProcessMessages;         //allow message processing
      sleep(100);
      saftey_loop := saftey_loop +1;       // Increment counter
      if saftey_loop > (USC_PM_StopSafetyTime*10) then
       begin
         us_MessageDlg('Warning','Failed to stop Pale Moon Browser', mtWarning,[mbOk],0) ; //Display warning message
         Break;
       end;
    end;
   Until not PaleMoonPortableRunning();
 end;
{--- END us_kill_palemoon  -----------------------------------------------------}


//=== DEFAULT BROWSER ===
{****************************************************************************
us_start_default_browser
 Skip if already running.
 Start with a reference html page allowing default exe name to be determined.
 Initial value for default browser DEFAULT_BROWSER_EXE xxx
Note: Uses Process
=============================================================================}
procedure us_start_default_browser;
Var
 saftey_loop: Integer;

begin
   if not DefaultBrowserRunning() then //Default browser is not running
    begin
    //--Run default browser with the reference html page.
    //URL Format eg: URL:='file:///G:/us_traymenu/docs/manual/index.html'
    ShellExecute(0, 'open', PCHAR('file:///'+USF_REFERENCE_HTML), Nil,  Nil,  SW_SHOWNORMAL);

    //--Wait for window and read unique title bar
    saftey_loop :=0;                  // Reset flag

    Repeat   // Wait for Default browser  - ready
      begin
        Application.ProcessMessages;      // allow message processing
        sleep(1000);
        saftey_loop := saftey_loop +1;    // Increment counter
        if saftey_loop > (USC_PM_ReadySafetyTime) then
         begin
           us_MessageDlg('Warning','Failed to start default browser', mtWarning,[mbOk],0) ; //Display warning message
           Break;
         end;
      end;
    Until default_browser_ready();
    end;
 end;
{--- End us_start_default_browser --------------------------------------------}


//=== DISPLAY IN BROWSER ===

{****************************************************************************
browser_display_url();
 Display page in portable browser - Pale Moon browser
 Or display in default browser
 Selection performed by variable PortableBrowser - Set if Pale Moon installed
 Note: Uses Process

General note:
 Display URL in default/portable browser.
     eg: URL:='http://www.lazarus.freepascal.org';
     eg: URL:='file:///G:/docs/English/index.html'

 Input
  URL : String  - Full URL to page
 Uses   windows

 This procedure uses the ShellExecute function to display the requested URL
 in the default browser. If the default browser is not running it is started.

 Mirosoft format:
  ShellExecute(NULL, 'open', 'http://www.microsoft.com', NULL, NULL, SW_SHOWNORMAL);
 Lazarus format:
  ShellExecute(0,    'open', 'http://www.microsoft.com', nil,  nil,  SW_SHOWNORMAL);

=============================================================================}
procedure browser_display_url(url:string);
Var
 AProcess: TProcess;
begin

 If PortableBrowser Then // Portable browser installed
  begin
    //us_start_palemoon;  // Ensure browser is running. If running no action taken. 
    //SudeepJD - Commeted the above line, The TProcess will start up Palemoon directly
    //  so a check that it is already running is not required, since it will start up anyways.

    //--Run command string.
    AProcess := TProcess.Create(nil);                         // Create new process
    AProcess.Executable := USF_PALE_MOON_EXE;                 // Executable to run
    AProcess.Parameters.Add(url);                             // Pass parameter to run
    AProcess.Options  := AProcess.Options + [poNoConsole];    // Set option no console
    AProcess.Execute;                                         // Run command
    AProcess.Free;                                            // Release process
  end
 Else   // Use default browser
  begin
    //us_start_default_browser; // Ensure browser is running. If running no action taken.
    //SudeepJD - Commented the above line since ideally a ShellExecute should start anyway, starting
    // with a default page, does not seem to be required.
    ShellExecute(0, 'open', PCHAR(URL), Nil,  Nil,  SW_SHOWNORMAL); //Display in default
  end;
end;

{--- End browser_display_url ----------------------------------------------}

//=== PAC ===

{****************************************************************************
us_add_to_pac_file
 Add domain (e.g fred.com) name to UniServer PAC file
=============================================================================}
procedure us_add_to_pac_file(host:string);
Var
  newHost :String;                   //New host to inser
  endPac  :String;                   // End of pac file
  sList: TStringList;                // String list
  i: Integer;                        // Loop counter
begin
If FileExists(USF_PAC) Then               // Check proxy.pac exists
 begin
   //Create new host string
   newHost := 'if (shExpMatch(host, "*'+host+'")) return "PROXY 127.0.0.1:'+UENV_AP_PORT+'";';
   host := QuoteRegExprMetaChars(host);   // Escape to regular string for regex
   endPac  := '^return "DIRECT";}';       // Convert to regex

   Try
    begin
     sList := TStringList.Create;         // Create object
     sList.LoadFromFile(USF_PAC);         // Load file
     for i:=0 to sList.Count-1 do         // Scan file line by line

       begin
        if (sList[i]<>'') and ExecRegExpr(host, sList[i]) then break; // host exists nothing else to do
        if (sList[i]<>'') and ExecRegExpr(endPac, sList[i]) then     // host not found
          begin
            sList.Insert(i,newHost);               // add new host before to list
            sList.SaveToFile(USF_PAC);             // Save new host to PAC file
          end;
       end;
    end;//End try
   Finally
    sList.Free;     // remove from memory
 end;
end;
end;
{--- End us_add_to_pac_file ------------------------------------------------}

{****************************************************************************
us_delete_from_pac_file
 Delete domain (e.g fred.com) name from UniServer PAC file
=============================================================================}
procedure us_delete_from_pac_file(host:string);
Var
  sList: TStringList;                // String list
  i: Integer;                        // Loop counter
begin
If FileExists(USF_PAC) Then          // Check proxy.pac exists
 begin
   host := QuoteRegExprMetaChars(host);   // Escape to regular string for regex

   Try
    begin
     sList := TStringList.Create;           // Create object
     sList.LoadFromFile(USF_PAC);           // Load file
     for i:=0 to sList.Count-1 do           // Scan file line by line

       begin
        if (sList[i]<>'') and ExecRegExpr(host, sList[i]) then  // host found
          begin
            sList.Delete(i);                 // Remove host from list
            sList.SaveToFile(USF_PAC);       // Save pac file
            break;                           // Exit for nothing else to do
          end;
       end;
    end;//End try
   Finally
    sList.Free;     // remove from memory
 end;
end;
end;
{--- End us_delete_from_pac_file -------------------------------------------}

{****************************************************************************
us_proxy_file_port_update
 Replace old port with new port for all domains
=============================================================================}
procedure us_proxy_file_port_update(old_port:string;new_port:string);
Var
  sList: TStringList;                // String list
  i: Integer;                        // Loop counter
  old_str:string;                    // "PROXY 127.0.0.1:port"
  new_str:string;                    // "PROXY 127.0.0.1:new_port"
begin
If FileExists(USF_PAC) Then          // Check proxy.pac exists
 begin
   old_str:= '"PROXY 127.0.0.1:'+old_port+'"';
   new_str:= '"PROXY 127.0.0.1:'+new_port+'"';

   Try
    begin
     sList := TStringList.Create;         // Create object
     sList.LoadFromFile(USF_PAC);         // Load file
     for i:=0 to sList.Count-1 do         // Scan file line by line
       begin
         sList[i] := StringReplace(sList[i], old_str,new_str,[rfReplaceAll]);   // Update port to new value
       end;
     sList.SaveToFile(USF_PAC);           // Save new pac to file
    end;//End try
   Finally
    sList.Free;     // remove from memory
 end;
 end;
end;
{--- End us_proxy_file_port_update ------------------------------------------}


//=== HOSTS ===


{****************************************************************************
us_launch_edit_hosts_utility
 Launches the Uniform Server Windows hosts file utility
 Uses a seperate cmd window forces user to confirm changes to hosts file.
=============================================================================}
procedure us_launch_edit_hosts_utility;
Var
 AProcess: TProcess;
begin
 If USC_EditHostsFileEnabled Then          // Check user has enabled Edit Windows hosts file
  begin
   If FileExists(USF_EDHOST_UTILITY) Then  // Check edit host utility EdHost.exe exists
   begin
    AProcess := TProcess.Create(nil); // Create process

    AProcess.Executable := 'cmd';                           // Executable to run
    AProcess.Parameters.Add('/T:B0');                       // Set background colour
    AProcess.Parameters.Add('/C');                          // Close on exit

    AProcess.Parameters.Add('title');                       // A title is required
    AProcess.Parameters.Add('Edit hosts');                  // Title

    AProcess.Parameters.Add('&&');                          // Start a new command line
    AProcess.Parameters.Add('start');                       // Command to run
    AProcess.Parameters.Add(USF_EDHOST_UTILITY);            // Run EdHost utility

    AProcess.Options := AProcess.Options + [poNoConsole];   // Set option no console
    AProcess.Execute;                                       // execute detatched process
    AProcess.Free;                                          // free memory

   end;
  end;
end;
{--- End us_launch_edit_hosts_utility ----------------------------------------}


{****************************************************************************
us_add_to_hosts_file
 Add domain (e.g fred.com) name to Windows hosts
 Uses a seperate cmd window forces user to confirm changes to hosts file.
=============================================================================}
procedure us_add_to_hosts_file(host:string);
Var
 AProcess: TProcess;
begin
 If USC_EditHostsFileEnabled Then          // Check user has enabled Edit Windows hosts file
  begin
   If FileExists(USF_EDHOST_UTILITY) Then  // Check edit host utility EdHost.exe exists
   begin
    AProcess := TProcess.Create(nil); // Create process

    AProcess.Executable := 'cmd';                           // Executable to run
    AProcess.Parameters.Add('/T:B0');                       // Set background colour
    AProcess.Parameters.Add('/C');                          // Close on exit

    AProcess.Parameters.Add('title');                       // A title is required
    AProcess.Parameters.Add('Edit hosts');                  // Title

    AProcess.Parameters.Add('&&');                          // Start a new command line
    AProcess.Parameters.Add('start');                       // Command to run
    AProcess.Parameters.Add(USF_EDHOST_UTILITY);            // Run EDHost utility
    AProcess.Parameters.Add('add');                         // Utility Command parameter
    AProcess.Parameters.Add(host);                          // Domain name to add

    AProcess.Options := AProcess.Options + [poNoConsole];   // Set option no console
    AProcess.Options := AProcess.Options + [poWaitOnExit];  // Wait for command to run
    AProcess.Execute;                                       // execute detatched process
    AProcess.Free;                                          // free memory

   end;
  end;
end;
{--- End us_add_to_hosts_file ------------------------------------------------}

{****************************************************************************
us_delete_from_hosts_file
 Delete domain (e.g fred.com) name from Windows hosts file
 Uses a seperate cmd window forces user to confirm changes to hosts file.
=============================================================================}
procedure us_delete_from_hosts_file(host:string);
Var
 AProcess: TProcess;
begin
 If USC_EditHostsFileEnabled Then          // Check user has enabled Edit Windows hosts file
  begin
   If FileExists(USF_EDHOST_UTILITY) Then  // Check edit host utility EdHost.exe exists
   begin

   AProcess := TProcess.Create(nil); // Create process

   AProcess.Executable := 'cmd';                           // Executable to run
   AProcess.Parameters.Add('/T:B0');                       // Set background colour
   AProcess.Parameters.Add('/C');                          // Close on exit

   AProcess.Parameters.Add('title');                       // A title is required
   AProcess.Parameters.Add('Edit hosts');                  // Title

   AProcess.Parameters.Add('&&');                          // Start a new command line
   AProcess.Parameters.Add('start');                       // Command to run
   AProcess.Parameters.Add(USF_EDHOST_UTILITY);            // Run EDHost utility
   AProcess.Parameters.Add('del');                         // Utility Command parameter
   AProcess.Parameters.Add(host);                          // Domain name to delete

   AProcess.Options := AProcess.Options + [poNoConsole];   // Set option no console
   AProcess.Options := AProcess.Options + [poWaitOnExit];  // Wait for command to run
   AProcess.Execute;                                       // execute detatched process
   AProcess.Free;                                          // free memory

   end;
  end;
end;
{--- End us_delete_from_hosts_file -------------------------------------------}


//--END----
end.

