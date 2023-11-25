unit are_servers_runable; 

{#############################################################################
'# Name: are_servers_runable.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,Dialogs,LCLType,
  default_config_vars,
  us_common_functions,
  RegExpr,
  JwaTlHelp32,
  windows;


function us_application_is_runable:boolean;  // Check path for spaces and correct location
procedure us_check_servers_are_runable;      // Port in use and other killers

implementation

{=================================================================
 WinUniqueInstance:
 This function provides an easy way to determine only a single
 instance of the application is running.

 WinUniqueInstance returns true if the application is unique
 otherwise returns false if more than one instance is running.

 Uses: JwaTlHelp32 and windows
 Note: When application is terminated may cause screen to flicker.
       No big deal a user is unlikey to start another instance!
==================================================================}
function WinUniqueInstance:boolean;
var
  ProcessName:string;            // This application
  total:integer;                 // Number of this application running
  hProcessSnap: THandle;         // Snapshot
  pe          : TProcessEntry32; // Process entry
  Continue    : BOOL;            // Flag
begin
 WinUniqueInstance := True;                   // Assume Unique
 total             := 0;                      // Reset counter
 ProcessName       := ApplicationName+'.exe'; // Name of this exe file

 hProcessSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); //Take snapshot
 if (hProcessSnap=0) or (hProcessSnap=INVALID_HANDLE_VALUE) then Exit;
   try
     pe.dwSize := SizeOf(pe);                       // Set-up structure size
     Continue := Process32First(hProcessSnap, pe);  // Set flag first (first tobe processed)

     while Continue do
     begin
       //Walk snapshot and count number of ProcessNames
       if pe.szExeFile = ProcessName then total:= total+1; // Count instances
       If total >= 2 then                          // Test two or more not unique
         begin
           WinUniqueInstance := False;             // Set False not unique
           exit;                                   // Nothing else to do
         end;
      Continue := Process32Next(hProcessSnap, pe); // Set flag next (more to process)
     end;

   finally
     CloseHandle(hProcessSnap); // Close handle
   end;
end;

{=================================================================
 us_application_is_runable:
 This procedure is the first to run. It is draconian killing
 the application (Controller) in the following situations:
 1) If the aplication is already running.
 2) If a space is detected in path to this application.
 3) If application is located in wrong folder.
    (Designed to run from folder UniServerZ)
 User is informed as appropriate before closing application.
==================================================================}
function us_application_is_runable:boolean;
var
 str_title :String;
 str       :string;
 path      :string;
 ret       :boolean;
begin
  ret := True; // Assume is runable
  //=== Check only this instance is running
  If not WinUniqueInstance then
   begin

   //Not Unique Instance
   str_title := 'UniController not unique instance';
   str       :=       'An instance of UniController is already running.'+ sLineBreak + sLineBreak;
   str       := str + 'This new UniController instance will close.';

   //--Inform user another instance is running
   Application.ShowMainForm  := False;             // Hide application
    us_MessageDlg(str_title, str, mtError,[mbOk],0) ; // Display message
    ret := False;                                  // Not runable
   end;

  //===Check for spaces in path to this executable
  path :=  ExtractFilePath(Application.ExeName); // Path to this application e.g E:\UniServerZ\

  If Pos(' ',path) <> 0 then //Check for space
  begin
   str_title := 'Error in path';
   str  :=      'A space in the path to this application was detected. Spaces are not allowed.'+ sLineBreak + sLineBreak;
   str  :=      'Please move folder UniServerZ, making sure new path does not contain spaces.'+ sLineBreak + sLineBreak;
   str  := str+ 'To prevent problems, UniController will close.';

   //--Inform user a space was detected
   Application.ShowMainForm  := False;            // Hide application
   us_MessageDlg(str_title, str, mtError,[mbOk],0) ; // Display message
   ret := False;                                  // Not runable
  end;

  //===Check for correct location of UniController
  //Reference file for location, us_config.ini is a good choice to use 
  if not FileExists(ExtractFilePath(Application.ExeName)+'home\us_config\us_config.ini') Then
    begin
     str_title := 'Error: Incorrect location!';
     str       :=      'Incorrect location! Move UniController to folder UniServerZ.'+ sLineBreak + sLineBreak;
     str       := str+ 'To prevent problems, UniController will close.';

     //--Inform User inorrect location of this application
     Application.ShowMainForm  := False;            // Hide application
     us_MessageDlg(str_title, str, mtError,[mbOk],0) ; // Display message
     ret := False;                                  // Not runable
    end;
  us_application_is_runable := ret;                 // Resturn result
end;
{--End us_application_is_runable-------------------------------------}

{=====================================================================
us_check_servers_are_runable:
 This procedure checks for elements that prevent servers running'
 1) Zone Alarm causes a blue screen of death when Apache stopped.
 2) Ports in use prevent servers starting
    Display a warning message. The port is in use and the application
    name using it.
    Note: On W7 and W8 otaining full path to the application is a real
          pain hence, only the name is provided.  
======================================================================}
procedure us_check_servers_are_runable;

var
 display_warning    :boolean; //Flag
 str_title          :string;
 str                :String;
 ThisApacheRunning  :Boolean; //Flag perform check once
 ThisMySQLRunning   :Boolean; //Flag perform check once
 pid                :string;  //Pid of application using port
 appname            :string;  //Name of application using port
 app_path           :string;  //Path of application using port

  begin

  //=== Check for Zone Alarm
  //Note: No point in displaying at PC-Win start-up
  If Not USC_RunAtPcStartUpEnabled Then 
    begin
      If USC_ZA_CHECK Then
        begin
          If us_IsProcessRunning('zatray.exe') Then
            begin
              str_title := 'Warning: ZoneAlarm detected';
              str :=      'The ZoneAlarm application may conflict with the Apache Server'+ sLineBreak;
              str := str+ 'and produce a blue screen of death when Apache is closed.'      + sLineBreak + sLineBreak;
              str := str+ 'Recommendation: EXIT ZoneAlarm before closing Apache'          + sLineBreak;
              str := str+ 'or preferably upgrade ZoneAlarm.';

              us_MessageDlg(str_title, str, mtWarning,[mbOk],0) ;
            end;
        end;
    end;

  //=== Check ports in use
  str_title := 'Warning: ports in use by another application';
  str       :='';
  str       := str + 'The following ports are in use by another application.' + sLineBreak ;
  str       := str + 'In order to start the servers, these ports must be free.' + sLineBreak ;
  str       := str + 'Preferably stop the application and/or change the ports.'+ sLineBreak + sLineBreak ;
  str       := str + 'Alternatively, you can change the UniServer ports.';

  appname   := '';  //Set initial value

  display_warning    := False; // Reset flag
  ThisApacheRunning  := False; // Not running
  ThisMySQLRunning   := False; // Not running

  If ApacheRunning() Then ThisApacheRunning := True;
  If MysqlRunning()  Then ThisMySQLRunning  := True;


  //--Apache standard port not this server

  if not ApachePortFree then
    begin
     If not ThisApacheRunning Then
       begin
         display_warning := True;               // Set display flag
         pid      := GetPortPID(UENV_AP_PORT);  // Get pid of app using port
         appname  := GetAppName(pid);           // Get app name using pid
         app_path := GetAppPath(pid);           // Get app path using pid

         str := str + sLineBreak+ sLineBreak;
         str := str + 'Apache port       = '+ UENV_AP_PORT + sLineBreak;
         str := str + 'Application PID   = '+ pid          + sLineBreak;
         str := str + 'Application Name  = '+ appname      + sLineBreak;
         str := str + 'Application Path  = '+ app_path;
       end;
    end;

  if not ApacheSSLPortFree then
    begin
      If not ThisApacheRunning Then
        begin
          display_warning := True;                   // Set display flag
          pid      := GetPortPID(UENV_AP_SSL_PORT);  // Get pid of app using port
          appname  := GetAppName(pid);               // Get app name using pid
          app_path := GetAppPath(pid);               // Get app path using pid

          str := str + sLineBreak+ sLineBreak;
          str := str + 'Apache SSL port   = '+ UENV_AP_SSL_PORT + sLineBreak;
          str := str + 'Application PID   = '+ pid              + sLineBreak;
          str := str + 'Application Name  = '+ appname          + sLineBreak;
          str := str + 'Application Path  = '+ app_path;
        end;
     end;

  //--MySQL standard port not this server
   if not MySqlPortFree then
     begin
        If not ThisMySQLRunning Then
          begin
            display_warning := True;                     // Set display flag
            pid      := GetPortPID(UENV_MYSQL_TCP_PORT); // Get pid of app using port
            appname  := GetAppName(pid);                 // Get app name using pid
            app_path := GetAppPath(pid);                 // Get app path using pid

            str := str + sLineBreak+ sLineBreak;
            str := str + 'MySQL port        = '+ UENV_MYSQL_TCP_PORT + sLineBreak;
            str := str + 'Application PID   = '+ pid                 + sLineBreak;
            str := str + 'Application Name  = '+ appname             + sLineBreak;
            str := str + 'Application Path  = '+ app_path;

          end;
     end;

   if  display_warning then
    begin
      us_MessageDlg(str_title, str, mtWarning,[mbOk],0) ; //Display message
    end;

end;


end.

