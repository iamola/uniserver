unit us_common_procedures;

{#############################################################################
'# Name: us_common_procedures.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#
'# Common procedures for UniService
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,default_config_vars,Process,
  us_common_functions,
  Forms, Controls, Graphics, Dialogs,
  ExtCtrls;


//=== General ===
procedure us_update_status;      // Update button text and indicators

//=== Apache ===
procedure us_install_apache_service;   // Install Apache as a service
procedure us_uninstall_apache_service; // UnInstall Apache service
procedure us_start_apache_service;     // Start Apache service
procedure us_stop_apache_service;      // Stop Apache service

//=== MySQL ===
procedure us_install_mysql_service;     // Install MySQL as a service
procedure us_uninstall_mysql_service;   // UnInstall MySQL as a service
procedure us_start_mysql_service;       // Start MySQL service
procedure us_stop_mysql_service;        // Stop MySQL service


//=== Configuration files ===
procedure us_restore_files;       // Restore backup files
procedure us_update_config_files; // Update variables in config files.

//=== General ===
procedure us_file_search_replace(FileToSearch:string;search_str:string;replace_str:string);   //Search and replace string in file

//=== Apache service test ===
procedure us_apache_service_test; // Run command line service test

implementation

uses
  main_form;

//=== General ===

{****************************************************************************
us_update_status
 Set intial button text and indicators
=============================================================================}
procedure us_update_status;
begin
 //=== Apache

 If DirectoryExists(US_APACHE) Then //Apache installed
   Begin

    Main.GB_apache.Caption := 'Apache Service name:  '+ APACHE_SERVICE_NAME; //Set group box

    If us_ServiceInstalled(Pchar(APACHE_SERVICE_NAME)) Then
     begin  //Installed
      Main.Btn_apache_install_uninstall.Enabled := True;                  //Enable button
      Main.Btn_apache_install_uninstall.Caption := Btn_uninstall_service; //Set button text
      Main.P_apache_install_indicator.Color := clLime;                    //Set indicator installed

      Main.Btn_apache_start_stop.Enabled := True;                       //Enable button
      Main.Btn_apache_start_stop.Caption := Btn_run_service;            //Set button text
      Main.P_apache_start_indicator.Color := clRed;                     //Set indicator not running

      //Check service running
       If us_IsServiceRunning(APACHE_SERVICE_NAME) Then
        begin  //Running
         Main.Btn_apache_install_uninstall.Enabled := False;                 //Disable button
         Main.Btn_apache_install_uninstall.Caption := Btn_uninstall_service; //Set button text
         Main.P_apache_install_indicator.Color := clLime;                    //Set indicator installed

         Main.Btn_apache_start_stop.Enabled := True;                       //Enable button
         Main.Btn_apache_start_stop.Caption := Btn_stop_service;           //Set button text
         Main.P_apache_start_indicator.Color := clLime;                    //Set indicator running
        end
        Else
         begin
          Main.Btn_apache_install_uninstall.Enabled := True;                  //Enable button
          Main.Btn_apache_install_uninstall.Caption := Btn_uninstall_service; //Set button text
          Main.P_apache_install_indicator.Color := clLime;                    //Set indicator installed

          Main.Btn_apache_start_stop.Enabled := True;                       //Enable button
          Main.Btn_apache_start_stop.Caption := Btn_run_service;            //Set button text
          Main.P_apache_start_indicator.Color := clRed;                     //Set indicator not running
         end;
     end
    Else
     begin //Not installed
      Main.Btn_apache_install_uninstall.Enabled := True;                //Enable button
      Main.Btn_apache_install_uninstall.Caption := Btn_install_service; //Set button text
      Main.P_apache_install_indicator.Color     := clRed;               //Set indicator not installed

      Main.Btn_apache_start_stop.Enabled := False;                      //Disable button
      Main.Btn_apache_start_stop.Caption := Btn_run_service;            //Set button text
      Main.P_apache_start_indicator.Color := clRed;                     //Set indicator not running
     end;
   end
   Else  //Apache not installed
    begin
       Main.GB_apache.Caption := 'Apache server NOT installed'; //Set group box
       Main.Btn_apache_install_uninstall.Enabled := False;                //Disable button
       Main.Btn_apache_start_stop.Enabled := False;                       //Disable button
       Main.P_apache_install_indicator.Color  := clGray;                  //Set indicator
       Main.P_apache_start_indicator.Color    := clGray;                  //Set indicator
    end;

  //=== MySQL
  //-US_MYMAR_TXT server type MySQL or MariaDB

 If DirectoryExists(US_MYSQL) Then //Apache installed
   Begin
     Main.GB_mysql.Caption := US_MYMAR_TXT+' Service name:  '+ MYSQL_SERVICE_NAME; //Set group box

     If us_ServiceInstalled(Pchar(MYSQL_SERVICE_NAME)) Then
      begin  //Installed
       Main.Btn_mysql_install_uninstall.Enabled := True;                  //Enable button
       Main.Btn_mysql_install_uninstall.Caption := Btn_uninstall_service; //Set button text
       Main.P_mysql_install_indicator.Color := clLime;                    //Set indicator installed

       Main.Btn_mysql_start_stop.Enabled := True;                       //Enable button
       Main.Btn_mysql_start_stop.Caption := Btn_run_service;            //Set button text
       Main.P_mysql_start_indicator.Color := clRed;                     //Set indicator not running

       //Check service running
        If us_IsServiceRunning(MYSQL_SERVICE_NAME) Then
         begin  //Running
          Main.Btn_mysql_install_uninstall.Enabled := False;                 //Disable button
          Main.Btn_mysql_install_uninstall.Caption := Btn_uninstall_service; //Set button text
          Main.P_mysql_install_indicator.Color := clLime;                    //Set indicator installed

          Main.Btn_mysql_start_stop.Enabled := True;                       //Enable button
          Main.Btn_mysql_start_stop.Caption := Btn_stop_service;           //Set button text
          Main.P_mysql_start_indicator.Color := clLime;                    //Set indicator running
         end
         Else
          begin
           Main.Btn_mysql_install_uninstall.Enabled := True;                  //Enable button
           Main.Btn_mysql_install_uninstall.Caption := Btn_uninstall_service; //Set button text
           Main.P_mysql_install_indicator.Color := clLime;                    //Set indicator installed

           Main.Btn_mysql_start_stop.Enabled := True;                       //Enable button
           Main.Btn_mysql_start_stop.Caption := Btn_run_service;            //Set button text
           Main.P_mysql_start_indicator.Color := clRed;                     //Set indicator not running
         end;
      end
     Else
      begin //Not installed
       Main.Btn_mysql_install_uninstall.Enabled := True;                //Enable button
       Main.Btn_mysql_install_uninstall.Caption := Btn_install_service; //Set button text
       Main.P_mysql_install_indicator.Color     := clRed;               //Set indicator not installed

       Main.Btn_mysql_start_stop.Enabled := False;                      //Disable button
       Main.Btn_mysql_start_stop.Caption := Btn_run_service;            //Set button text
       Main.P_mysql_start_indicator.Color := clRed;                     //Set indicator not running
     end;
   end
  Else
    begin
     Main.GB_mysql.Caption := 'MySQL or MariaDB server NOT installed'; //Set group box
     Main.Btn_mysql_install_uninstall.Enabled := False;                //Disable button
     Main.Btn_mysql_start_stop.Enabled := False;                       //Disable button
     Main.P_mysql_install_indicator.Color  := clGray;                  //Set indicator
     Main.P_mysql_start_indicator.Color    := clGray;                  //Set indicator
    end;

 //=== Apache service test button
 //-- If folder exists already installed as service hence
 //-- no need to perform Apache test
 If DirectoryExists(US_CORE_SB) and DirectoryExists(US_APACHE) Then  // Check directory exists
  begin    // Directory exists
   Main.Btn_apache_service_test.Enabled := False; // Disable test button
  end
 Else
  begin   // Directory does not exists
    Main.Btn_apache_service_test.Enabled := True;  // Enable test button
  end;

end;
{End us_set_text_indicators_init --------------------------------------------}

//=== Apache ===
{****************************************************************************
Install Apache Service
Uses  process, windows
=============================================================================}
procedure us_install_apache_service;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
begin
   saftey_loop :=0;

   //--Run command string.
  AProcess := TProcess.Create(nil);                              // Create new process

  AProcess.Executable := US_APACHE_BIN + '\' + APACHE_EXE_NAME;  // Executable to run
  AProcess.Parameters.Add('-k');
  AProcess.Parameters.Add('install');
  AProcess.Parameters.Add('-n');
  AProcess.Parameters.Add('"'+APACHE_SERVICE_NAME+'"' );         // Service name

  AProcess.Options     := AProcess.Options + [poNoConsole]; // Set option no console
  AProcess.Execute;                                         // Run command
  AProcess.Free;                                            // Release process

    While not us_ServiceInstalled(Pchar(APACHE_SERVICE_NAME)) do
    begin
      sleep(1000);
      saftey_loop := saftey_loop +1;                   // Increment counter
      if saftey_loop > 60 then                         // Check limit exceeded
       begin
         Showmessage('Failed to Install Apache as a service'); // Failed to Install Apache as a service
         Break;
       end;
    end;
end;
{----------------------------------------------------------------------------}


{****************************************************************************
UnInstall Apache Service
Uses  process, windows
=============================================================================}
procedure us_uninstall_apache_service;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
begin
   saftey_loop :=0;

   //--Run command string.
  AProcess := TProcess.Create(nil);                         // Create new process

  AProcess.Executable := US_APACHE_BIN + '\' + APACHE_EXE_NAME;  // Executable to run
  AProcess.Parameters.Add('-k');
  AProcess.Parameters.Add('uninstall');
  AProcess.Parameters.Add('-n');
  AProcess.Parameters.Add('"'+APACHE_SERVICE_NAME+'"' );     // Service name

  AProcess.Options     := AProcess.Options + [poNoConsole]; // Set option no console
  AProcess.Execute;                                         // Run command
  AProcess.Free;                                            // Release process

    While us_ServiceInstalled(PChar(APACHE_SERVICE_NAME)) do
    begin
      sleep(1000);
      saftey_loop := saftey_loop +1;                    // Increment counter
      if saftey_loop > 60 then                          // Check limit exceeded
       begin
         Showmessage('Failed to Uninstall Apache service');      // Failed to Uninstall Apache service
         Break;
       end;
    end;
end;
{----------------------------------------------------------------------------}


{*****************************************************************************
Start Apache Service
Uses  process, windows
=============================================================================}
procedure us_start_apache_service;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
begin
  saftey_loop :=0;

   //--Run command string.
  AProcess := TProcess.Create(nil);                         // Create new process

  AProcess.Executable := US_APACHE_BIN + '\' + APACHE_EXE_NAME;  // Executable to run
  AProcess.Parameters.Add('-k');
  AProcess.Parameters.Add('start');
  AProcess.Parameters.Add('-n');
  AProcess.Parameters.Add('"'+APACHE_SERVICE_NAME+'"' );         // Service name

  AProcess.Options     := AProcess.Options + [poNoConsole]; // Set option no console
  AProcess.Execute;                                         // Run command
  AProcess.Free;                                            // Release process

    sleep(2000);
    While Not us_IsServiceRunning(APACHE_SERVICE_NAME) do
    begin
      sleep(1000);
      saftey_loop := saftey_loop +1;                   // Increment counter
      if saftey_loop > 60 then                         //Limit exceeded
       begin
         Showmessage('Failed to start Apache service');// Failed to start Apache service
         Break;
       end;
    end;
end;
{----------------------------------------------------------------------------}


{*****************************************************************************
Stop Apache Service
Uses  process, windows
=============================================================================}
procedure us_stop_apache_service;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
begin
   saftey_loop :=0;

   //--Run command string.
  AProcess := TProcess.Create(nil);                         // Create new process

  AProcess.Executable := US_APACHE_BIN + '\' + APACHE_EXE_NAME;  // Executable to run
  AProcess.Parameters.Add('-k');
  AProcess.Parameters.Add('stop');
  AProcess.Parameters.Add('-n');
  AProcess.Parameters.Add('"'+APACHE_SERVICE_NAME+'"' );         // Service name

  AProcess.Options     := AProcess.Options + [poNoConsole]; // Set option no console
  AProcess.Execute;                                         // Run command
  AProcess.Free;                                            // Release process

    While us_IsServiceRunning(APACHE_SERVICE_NAME) do
    begin
      sleep(1000);
      saftey_loop := saftey_loop +1;                   // Increment counter
      if saftey_loop > 60 then                         // Limit exceeded
       begin
         Showmessage('Failed to stop Apache service'); // Failed to stop Apache service
         Break;
       end;
    end;
end;
{---------------------------------------------------------------------------}

//=== MySQL ===

{****************************************************************************
Install MySQL Service
Uses  process, windows
=============================================================================}
procedure us_install_mysql_service;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
begin
    saftey_loop :=0;

    AProcess := TProcess.Create(nil);                            // Create process
    AProcess.Executable := US_MYSQL_BIN + '\' + MYSQL_EXE_NAME;  // Executable to run
    AProcess.Parameters.Add('--install');                        // Command to install service
    AProcess.Parameters.Add('"'+ MYSQL_SERVICE_NAME +'"');       // serveice name
    AProcess.Parameters.Add('--defaults-file="' + USF_MYSQL_INI  + '"'); // Force use of a specific default config file

    AProcess.Options := AProcess.Options + [poNoConsole];        // Set options hide console
    AProcess.Execute;                                            // execute detatched process
    AProcess.Free;                                               // free memory

  While not us_ServiceInstalled(PChar(MYSQL_SERVICE_NAME)) do
    begin
      sleep(1000);
      saftey_loop := saftey_loop +1;                           // Increment counter
      if saftey_loop > 60 then                                 // Limit excceded
       begin
         Showmessage(' Failed to Install MYSQL as a service'); // Failed to Install MYSQL as a service
         Break;
       end;
   end;
end;
{---------------------------------------------------------------------------}


{****************************************************************************
UnInstall MySQL Service
Uses  process, windows
=============================================================================}
procedure us_uninstall_mysql_service;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
begin
    saftey_loop :=0;

    AProcess := TProcess.Create(nil);                           // Create process
    AProcess.Executable := US_MYSQL_BIN + '\' + MYSQL_EXE_NAME; // Executable to run
    AProcess.Parameters.Add('--remove');                        // Command to remove service
    AProcess.Parameters.Add('"'+ MYSQL_SERVICE_NAME +'"');      // serveice name
    AProcess.Options := AProcess.Options + [poNoConsole];       // Set options hide console
    AProcess.Execute;                                           // execute detatched process
    AProcess.Free;                                              // free memory

    While us_ServiceInstalled(MYSQL_SERVICE_NAME) do
    begin
      sleep(1000);
      saftey_loop := saftey_loop +1;                       // Increment counter
      if saftey_loop > 60 then                             // Limit exceeded
       begin
         Showmessage('Failed to Uninstall MySQL service'); // Failed to Uninstall MySQL service
         Break;
       end;
    end;
end;
{----------------------------------------------------------------------------}


{****************************************************************************
Start MySQL Service
Uses  process, windows
=============================================================================}
procedure us_start_mysql_service;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
begin
    saftey_loop :=0;

    AProcess := TProcess.Create(nil);                      // Create process
    AProcess.Executable := 'sc.exe';                       // Executable to run
    AProcess.Parameters.Add('start');                      // Issue service start command
    AProcess.Parameters.Add('"'+ MYSQL_SERVICE_NAME +'"'); // serveice name
    AProcess.Options := AProcess.Options + [poNoConsole];  // Set options hide console
    AProcess.Execute;                                      // execute detatched process
    AProcess.Free;                                         // free memory

    While Not us_IsServiceRunning(MYSQL_SERVICE_NAME) do
    begin
      sleep(1000);
      saftey_loop := saftey_loop +1;                   // Increment counter
      if saftey_loop > 60 then                        // Limit exceeded
       begin
         Showmessage('Failed to start MySQL service');         // Failed to start MySQL service
         Break;
       end;
    end;
end;
{-----------------------------------------------------------------------------}


{*****************************************************************************
Stop MySQL Service
Uses  process, windows
=============================================================================}
procedure us_stop_mysql_service;
Var
 AProcess: TProcess;
 saftey_loop: Integer;
begin
    saftey_loop :=0;

    AProcess := TProcess.Create(nil);                      // Create process
    AProcess.Executable := 'sc.exe';                       // Executable to run
    AProcess.Parameters.Add('stop');                       // Issue service stop command
    AProcess.Parameters.Add('"'+ MYSQL_SERVICE_NAME +'"'); // serveice name
    AProcess.Options := AProcess.Options + [poNoConsole];  // Set options hide console
    AProcess.Execute;                                      // execute detatched process
    AProcess.Free;                                         // free memory

 While us_IsServiceRunning(MYSQL_SERVICE_NAME) do
    begin
      sleep(1000);
      saftey_loop := saftey_loop +1;                  // Increment counter
      if saftey_loop > 60 then                        // Limit exceeded
       begin
         Showmessage('Failed to stop MySQL service'); // Failed to stop MySQL service
         Break;
       end;
    end;
end;
{----------------------------------------------------------------------------}

//=== Configuration files ===


{****************************************************************************
us_restore_files
 If backup directory exists restore files and delete directory.
============================================================================}
procedure us_restore_files;
var
 i:integer; // loop counter
begin
 If DirectoryExists(US_CORE_SB) Then  // Check directory exists
  Begin
    //Restore files
    for i:=0 to StrAllSource.Count-1 do
      begin
        CopyFile(StrAllBackup[i],StrAllSource[i]); // Save backup files to original location
      end;

    //Remove all files in back-up folder and remove back-up folder
    DeleteDirectory(US_CORE_SB,true);  // Delete folder content
    DeleteDirectory(US_CORE_SB,false); // Delete folder
 
 end;//Check directory exixts
end;
{--End us_backup_files -----------------------------------------------------}

{****************************************************************************
us_update_config_files
 Update variables in config files.
 Note Update file even if varial does not exist Makes for easy coding.
============================================================================}
procedure us_update_config_files; // Update variables in config files.
var
 i:integer; //loop counter
begin
 // us_file_search_replace(FileToSearch,search_str,replace_str);

    //Update variables in original configuration files
    for i:=0 to StrAllSource.Count-1 do
      begin
        us_file_search_replace(StrAllSource[i],'${US_ROOTF}',UniConPath_F);              // Replace ${US_ROOTF} with UniConPath_F
        us_file_search_replace(StrAllSource[i],'${HOME}',UniConPath);                    // Replace ${HOME} with US_HOME
        us_file_search_replace(StrAllSource[i],'${SRVROOT}',US_APACHE);                  // Replace ${SRVROOT} with US_APACHE
        us_file_search_replace(StrAllSource[i],'${AP_PORT}',UENV_AP_PORT);               // Replace ${AP_PORT} with UENV_AP_PORT
        us_file_search_replace(StrAllSource[i],'${AP_SSL_PORT}', UENV_AP_SSL_PORT);      // Replace ${AP_SSL_PORT} with UENV_AP_SSL_PORT
        us_file_search_replace(StrAllSource[i],'${PHP_SELECT}',UENV_PHP_SELECT);         // Rreplace ${PHP_SELECT}  with UENV_PHP_SELECT
        us_file_search_replace(StrAllSource[i],'${PHP_INI_SELECT}',UENV_PHP_INI_SELECT); // Replace ${PHP_INI_SELECT} with UENV_PHP_INI_SELECT
        us_file_search_replace(StrAllSource[i],'${US_SERVERNAME}',UENV_US_SERVERNAME);   //Replace ${US_SERVERNAME} with UENV_US_SERVERNAME 
        us_file_search_replace(StrAllSource[i],'${US_ROOTF_WWW}',UENV_US_ROOTF_WWW);     // Replace ${US_ROOTF_WWW} with UENV_US_ROOTF_WWW
        us_file_search_replace(StrAllSource[i],'${US_ROOTF_WWW}', UENV_US_ROOTF_WWW);    // Replace ${US_ROOTF_WWW} with UENV_US_ROOTF_WWW
        us_file_search_replace(StrAllSource[i],'${US_ROOTF_SSL}', UENV_US_ROOTF_SSL);    // Replace ${US_ROOTF_SSL} with UENV_US_ROOTF_SSL
    end;

end;
{--End us_update_config_files ------------------------------------------------}


//=== General ===
{****************************************************************************
Search and replace all occurences of a string in file.
FileToSearch: Full path of file to search
search_str  : String to search for
replace_str : Replacment string
=============================================================================}
procedure us_file_search_replace(FileToSearch:string;search_str:string;replace_str:string);
 var
   Str: TStringList;
   i: Integer;
begin
 if FileExists(FileToSearch) Then
  begin
   Str := TStringList.Create;      // Create class
   Str.LoadFromFile(FileToSearch); // Read file into string list
   for i:=0 to Str.Count-1 do
     begin
       Str[i]:=StringReplace(Str[i],search_str,replace_str,[rfReplaceAll]);
     end;
   Str.SaveToFile(FileToSearch);   // Save changes string list to file
   Str.Clear;                      // remove all enteries
   Str.Free;                       // remove from memory
  end;
end;
{-------------------------------------------------------------}

//=== Apache service test ===
{****************************************************************************
us_apache_service_test
 Runs standard command line to install and uninstall Apache service
 Command window displays any errors.
=============================================================================}
procedure us_apache_service_test;
 var
   AProcess: TProcess;
begin
 AProcess := TProcess.Create(nil); // Create process

 AProcess.Executable := 'cmd';                              // Executable to run
 AProcess.Parameters.Add('/T:B0');                          // Set background colour
 AProcess.Parameters.Add('/K');                             // Keep open

 AProcess.Parameters.Add('title');                          // A title is required
 AProcess.Parameters.Add('Apache service test');            // Title

 //== Attempt to install service
 AProcess.Parameters.Add('&&');                                  // Start a new command line
 AProcess.Parameters.Add(US_APACHE_BIN + '\' + APACHE_EXE_NAME); // Executable to run
 AProcess.Parameters.Add('-k');
 AProcess.Parameters.Add('install');
 AProcess.Parameters.Add('-n');
 AProcess.Parameters.Add('"'+APACHE_SERVICE_NAME+'"' );          // Service name

 //== Attempt to Un-install service
 AProcess.Parameters.Add('&&');                                  // Start a new command line
 AProcess.Parameters.Add(US_APACHE_BIN + '\' + APACHE_EXE_NAME); // Executable to run
 AProcess.Parameters.Add('-k');
 AProcess.Parameters.Add('uninstall');
 AProcess.Parameters.Add('-n');
 AProcess.Parameters.Add('"'+APACHE_SERVICE_NAME+'"' );          // Service name

 //Execute command
 AProcess.Options  := AProcess.Options + [poWaitOnExit];   // Wait for console window to close
 AProcess.Execute; // execute detatched process command window remains visible
 AProcess.Free;    // free memory

end;
{----------------------------------------------------------------------------}
end.

