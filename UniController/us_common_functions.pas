unit us_common_functions;

{#############################################################################
'# Name: us_common_functions.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'# V1.1.2 11-05-2014 valid_admin_email updated ExecRegExpr
'#
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  JwaTlHelp32,
  JwaPsApi,
  windows,
  process,
  RegExpr,
  default_config_vars,
  Dialogs,
  LazFileUtils,
  FileUtil,
  Controls,
  httpsend,
  blcksock,
  INIFiles,
  forms,
  message_dlg_form;

//=== GENERAL ===
function us_ini_get(IniFile:string;section:string;variable:string):string;    // Get value from config file
function PortAvailable(Host:String;Port:String):boolean;                      // Check port is available returns true=available
function GetAppName(PIDS:String):string;                                      // Returns application name given its PID
function GetAppPath(PIDS:String):string;                                      // Returns application path given its PID
function GetPortPID(port:string):string;                                      // Get pid of application that is using server port

function DeleteDirectoryEx(DirectoryName: string): boolean;                             // Deletes the named folder and ALL its content.
function us_IsProcessRunning(ProcessName: string): boolean;                             // Check process running
function us_get_new_port(old_port:string;var new_port:string;standard:string): boolean; // Get port from user
function us_get_valid_web_page(var web_page:string): boolean;                           // Get valid page from user
function us_get_new_root_folder(var server_root:string;root:string): boolean;           // User input get new root folder
function us_get_word(var ALine: String; Delimiter: String): String;                     // Returns the first word from a string

function RelToAbsDir(BaseDirIn: string; DirIn:string): string; //Convert absolute/relative to absolute paths

//=== Apache ====
function ApachePortFree:boolean;                        // This function checks the specified Apache host and port are free to use
function ApacheSSLPortFree:boolean;                     // This function checks the specified Apache host and ssl port are free to use
function us_get_apache_exe():string;                    // Returns Apache executable file name
function ApacheRunning():boolean;                       // Is this Apache server running
function SSL_Enabled:boolean;                           // Returns true if SSL enabled

//=== MySQL ===
function MySqlPortFree:boolean;                           // This function checks the specified MySQL host and port are free to use
function us_get_mysql_exe():string;                       // Returns MySQL executable file name
function MysqlRunning():boolean;                          // Is this MySQL server running
function MysqlServerReady(root_password:string):boolean;  // MySQL server is ready to serve data
function us_get_mysql_password(): string;                 // Get MySQL password from file
function us_get_new_pwd(old_pwd:string;var new_pwd:string): boolean; //Get password from user string

function us_mysql_batch(mysql_str:string;var mysql_result_list:TStringList):boolean; //Runs SQL query on Server result returned in Tstringlist

function us_valid_mysql_user_name(str:string):boolean;        // Test for valid format
function us_valid_mysql_password(str:string):boolean;         // Test for valid format
function us_valid_mysql_db_name(str:string):boolean;          // Test for valid format
function us_valid_mysql_backup_file_name(str:string):boolean; // Test for valid format
function us_start_mysql_skip_grants():boolean;                // Start MySQL no grants resturns true if started and ready.

//=== USER INPUT VALIDATION ===
function valid_root_folder_name(root_folder_name:string;display_str:string): boolean;
function valid_server_name(server_name:string;display_str:string): boolean;
function valid_server_port(server_port:string;display_str:string): boolean;
function valid_admin_email(admin_email:string;display_str:string): boolean;
function valid_directory_index_files(directory_index:string;display_str:string): boolean;
function valid_ssi_file_extensions(ssi_extensions:string;display_str:string): boolean;
function valid_server_signature(server_signature:string;display_str:string): boolean;
function valid_ip_address(ip_address:string;display_str:string): boolean;

//=== ACCESS AND PASSWORD ===
function us_www_htaccess_check_create: boolean;  // Check htaccess file exists or create default htaccess file
function us_ssl_htaccess_check_create: boolean;  // Check htaccess file exists or create default htaccess file
function password_file_contains_defaults(password_file:string):boolean; //Check password file for root:root
function password_file_empty(password_file:string):boolean; //Check password file empty

//=== GET WEB DATA ===
function get_ip_address(var ip:string):boolean;                //Get ip-address s seen from internet
function get_version_information(var version:string):boolean;  //Get current version from UniServer
function server_accessible_from_internet:boolean;              //Is this server accessible from internet

//=== PALE MOON BROWSER ===
function PaleMoonPortableRunning():boolean; // Is Pale Moon Portable browser running. Returns true running
function EnumWindowsProc(WHandle: HWND; LParM: LParam): LongBool;StdCall;Export; // enum. windows set flag
function palemoon_ready():boolean; // Check browser ready, uses above enumeration function

//=== DEFAULT BROWSER ===
function DefaultBrowserRunning():boolean; // Is Default browser running. Returns true running
function EnumWindowsProc2(WHandle: HWND; LParM: LParam): LongBool;StdCall;Export; // enum. get pid/name set flag
function default_browser_ready():boolean; // Check default browser ready, uses above enumeration function

//=== MESSAGE DIALOG ===
function us_MessageDlg(aCaption:string; aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

//===END===

implementation

uses
  us_common_procedures;

 //=== GENERAL ===

 {====================================================================
 Get ini configuration file value
 Note uses INIFiles
 =====================================================================}
 function us_ini_get(IniFile:string;section:string;variable:string):string;
 Var
   ini:TINIFile;
 begin
  ini := TINIFile.Create(IniFile);                    // create object
  us_ini_get := ini.ReadString(section,variable,'');  // get value
  Ini.Free;                                           // Free method of object
 end;
 {--- End us_ini_get ------------------------------------------------}


{====================================================================
PortAvailable: This function checks the speciied host and port are
               available (not in use)
Input:         Host: Apache 0.0.0.0 (all) MySQL 127.0.0.1 (local)
Output:        True:  Port is free to use
               False: Port in use
Uses:          blcksock           - Synapse libary
Used by        ApachePortFree     - Standard none ssl port
functions:     ApacheSSLPortFree  - Standard ssl port
               MySqlPortFree      - MySQL port
=====================================================================}
function PortAvailable(Host:String;Port:String):boolean;
var
  svr : TTCPBlockSocket;
begin
  svr := TTCPBlockSocket.Create;
  try
    svr.Bind(host,Port);
    svr.Listen;
    result := svr.LastError = 0;
    Svr.CloseSocket;
  finally
    svr.Free;
  end;
end;
{--- End PortAvailable ---------------------------------------------}

{====================================================================
GetAppName: This function returns: Application name given its PID

Uses:  Windows,JwaTlHelp32
Note:  Uses only Module32First no call to Module32Next
       because executable is always the first module.
=====================================================================}
function GetAppName(PIDS:String):string;
var
  hProcessSnap: THandle;            // Snapshot
  pe          : TProcessEntry32;    // Process entry
  Continue    : BOOL;               // Flag
  PID         : DWORD;              // PID for snapshot is a DWORD
begin
  GetAppName := '';                 // Set initial value
  PID := DWORD(StrToInt(PIDS));     // Convert string to DWORD

  hProcessSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); //Take snapshot
  if (hProcessSnap=0) or (hProcessSnap=INVALID_HANDLE_VALUE) then Exit;
   try
     pe.dwSize := SizeOf(pe);                       // Set-up structure size
     Continue := Process32First(hProcessSnap, pe);  // Set flag first (first tobe processed)

     while Continue do
     begin
       //Walk snapshot for PID
       if (pe.th32ProcessID = PID) then    // Match found
         begin
          GetAppName := pe.szExeFile;      // Return file name.exe
          exit;                            // Nothing else to do
         end;
      Continue := Process32Next(hProcessSnap, pe); //Set flag next (more to process)
     end;

   finally
     CloseHandle(hProcessSnap);              // Close handle
   end;
end;
{--- End GetAppName ------------------------------------------------}

{====================================================================
GetAppPath: This function returns: Application path given its PID

Uses:  JwaTlHelp32,JwaPsApi,windows,Process
Note:  Delphi TlHelp32 > Laz JwaTlHelp32
       Delphi PsAPI    > JwaPsApi
       GetModuleFileNameEx - Extended module information
=====================================================================}
function GetAppPath(PIDS:String):string;
var
  hProcess: THandle;   //Get handle to process
  PID     : DWORD;     // PID for openprocess is a DWORD
begin
  PID := DWORD(StrToInt(PIDS));     // Convert string to DWORD
  //Get handle of open process
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,PID);
  if hProcess <> 0 then
  begin
    try
      SetLength(Result,MAX_PATH);
      FillChar(Result[1],Length(Result) * SizeOf(Char), 0);
      if GetModuleFileNameEx(hProcess,0,PChar(Result),Length(Result)) > 0 then
        Result := Trim(Result)               // Obtained process path
      else                                   // Failed to obtain path
        Result := 'Unable to obtain path';
    finally
      CloseHandle(hProcess)
    end;
  end
  else // Failed to obtain handle
    Result := 'Unable to obtain path';
end;
{--- End GetAppPath ------------------------------------------------}

{====================================================================
 GetPortPID: This function returns PID of application that is using
             the specified port.
 Uses the following utility: Netstat -anop tcp to otain port/pid.
  Note 1: Uses: process and RegExpr
  Note 2: Buffers result of -anop this can exceed 2K
====================================================================}

function GetPortPID(port:string):string;
 const
   READ_BYTES = 2048;           // set buffer size

 var
   sList:       TStringList;   // StringList
   MemStream:   TMemoryStream; // Memory buffer
   AProcess:    TProcess;      // Process object
   NumBytes:    LongInt;       // Number of bytes
   BytesRead:   LongInt;       // Bytes read
   i:           Integer;       // Loop counter
   RegexObj: TRegExpr;         // Object

begin
   GetPortPID := '0';                 // Set default
   BytesRead := 0;                    // Tracker
   MemStream := TMemoryStream.Create; // Temp Memorystream - buffer output

   //--Get netstat -anop tcp as list - command string to run.
   AProcess := TProcess.Create(nil);   // Create new process

   AProcess.Executable := 'netstat';  // Executable command to run
   AProcess.Parameters.Add('-a');     // Pass parameter to run
   AProcess.Parameters.Add('-n');     // Pass parameter to run
   AProcess.Parameters.Add('-o');     // Pass parameter to run
   AProcess.Parameters.Add('-p');     // Pass parameter to run
   AProcess.Parameters.Add('tcp');    // Pass parameter to run

   AProcess.Options := AProcess.Options + [poUsePipes];   // Note cannot use poWaitOnExit
   AProcess.Options := AProcess.Options + [poNoConsole];  // Set option no console
   AProcess.Execute;                                      // Run program

   while AProcess.Running do
    begin
     MemStream.SetSize(BytesRead + READ_BYTES);  // make sure we have enough memory space
     NumBytes := AProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES); // try reading it
     if NumBytes > 0 then       // Data to read
       Inc(BytesRead, NumBytes)
     else                       // no data, wait 100 ms
       Sleep(100);
    end;

    // read last part
    repeat
      MemStream.SetSize(BytesRead + READ_BYTES); // make sure we have enough memory space
      NumBytes := AProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES); // try reading it
      if NumBytes > 0  then Inc(BytesRead, NumBytes); // Data to read
    until NumBytes <= 0;

    MemStream.SetSize(BytesRead);     // Set memory size to bytes actually read

    sList := TStringList.Create;      // Create string list and
    sList.LoadFromStream(MemStream);  // Load with data read


    AProcess.Free;  // Free process
    MemStream.Free; // Free memory buffer


    //---Set search pattern - look for port number and extract pid
    RegexObj := TRegExpr.Create; // Create regx object
    RegexObj.Expression := '^\s*TCP\s.*:'+port+'\s.*:\d*\s*LISTENING\s*(\d*)$';  //Set search pattern

    //--- Scan list
    for i:=0 to sList.Count-1 do
     begin
       if RegexObj.Exec(sList[i]) then      // run regex - if match found
         begin
           GetPortPID := RegexObj.Match[1]; // Set Pid found
           break;                           // Nothing else to do
         end;
     end; //end Scan list

    RegexObj.Free;       // release object
    sList.Free;          // Release process
   end;
{--- End GetPortPID ------------------------------------------------}


{======================================================================
This Function: Deletes the named folder and ALL its content.
  Performs same function as:
    a) deltree <directory>, rmdir /s /q <directory>
    b) rm -rf <directory>

  Removes read-only files/directories Note: DeleteDirectory doesn't
  Removes directory itself
  Folder format G:\somepath\test will delete folder and all its content
----------------------------------------------------------------------}
function DeleteDirectoryEx(DirectoryName: string): boolean;

var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if FindFirstUTF8(CurSrcDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then
  begin
    repeat
      // Ignore directories and files without name:
      if (FileInfo.Name<>'.') and (FileInfo.Name<>'..') and (FileInfo.Name<>'') then
      begin
        // Remove all files and directories in this directory:
        CurFilename:=CurSrcDir+FileInfo.Name;
        // Remove read-only file attribute so we can delete it:
        if (FileInfo.Attr and faReadOnly)>0 then
           FileSetAttrUTF8(CurFilename, FileInfo.Attr-faReadOnly);
        if (FileInfo.Attr and faDirectory)>0 then
        begin
          // Directory; exit with failure on error
          if not DeleteDirectoryEx(CurFilename) then exit;
        end
        else
        begin
          // File; exit with failure on error
          if not DeleteFileUTF8(CurFilename) then exit;
        end;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  // Remove "root" directory; exit with failure on error:
  if (not RemoveDirUTF8(DirectoryName)) then exit;
  Result:=true;
end;
{------------------------------------------------------------------}

{====================================================================
 us_IsProcessRunning: Check named process is running.
                      Returns True if running otherwise False
 Uses:                JwaTlHelp32, windows
====================================================================}
 function us_IsProcessRunning(ProcessName: string): boolean;
 var
  hProcessSnap: THandle;            // Snapshot
  pe          : TProcessEntry32;    // Process entry
  Continue    : BOOL;               // Flag

 begin
  us_IsProcessRunning := False;    // Assume not running

  hProcessSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); //Take snapshot
  if (hProcessSnap=0) or (hProcessSnap=INVALID_HANDLE_VALUE) then Exit;
   try
     pe.dwSize := SizeOf(pe);                       // Set-up structure size
     Continue := Process32First(hProcessSnap, pe);  // Set flag first (first tobe processed)

     while Continue do
     begin
       //Walk snapshot for process name
       if pe.szExeFile = ProcessName then // Match found
        begin
         us_IsProcessRunning := True;     // Set return value
         exit;                            // Nothing else to do
        end;
      Continue := Process32Next(hProcessSnap, pe); //Set flag next (more to process)
     end;

   finally
     CloseHandle(hProcessSnap);           // Close handle
   end;
end;
{---End us_IsProcessRunning ----------------------------------------}


{====================================================================
 Get port value from user.
  Input:
   old_port - Current value of port to be changed
   new_port - variable pointer for return value
   standard - Standard port for server
  Output:
   true  - Returns true if user enter a valid port
   false - Returns false if user canceled/closed window.

  Uses Forms, LCLType, Dialogs, Controls;
====================================================================}
function us_get_new_port(old_port:string;var new_port:string;standard:string): boolean;
 var
   port_title : string;
   port_text  : string;
   s1         : string;
   s2         : string;
   s3         : string;

   valid_input: Boolean;
   have_port  : Boolean;
   UserStr    : string;

 begin
  UserStr     :='';        // Set initial value
  new_port    := old_port; // Set initial value to old port
  valid_input := False;    // Assume input is invalid or cancelled

  //Set text to use
  port_title  := ' Server Port - current value '+ old_port;
  port_text   := ' Enter new server port or press cancel: (standard port '+standard+')';
  s1 := ' Invalid port! ';
  s3 := sLineBreak + sLineBreak +' Please enter a valid server port or press cancel:';

  while Not valid_input do
   begin
    If InputQuery(port_title, port_text, UserStr) Then
     begin
       valid_input := True;            // Assume valid input -set flag

       //-- Check for blank, a space, all digits
       //-- only 5 digits range 0-65535 ,

       If valid_input then
        begin
          If Not valid_server_port(UserStr,'Server port') Then
           begin
            valid_input := False;
            s2 := ' This port already in use by this server.';
            port_text := s1 + s3;
           end;
        end;

        //-- Check for port clashes in this server
        If valid_input then
        begin
           If (UserStr =UENV_AP_PORT) or (UserStr = UENV_AP_SSL_PORT) or (UserStr =UENV_MYSQL_TCP_PORT)  Then
            begin
             valid_input := False;
             s2 := ' This port already in use by this server.';
             port_text := s1 + s2 + s3;
            end;
        end;

        //==END Check data entered by user

        //==Set new port value
        If valid_input then
         begin
          have_port := True;    // Have a valid value for port
          new_port  := UserStr; // Set port value
         end;
     end
    Else
      begin
        valid_input := True;     // User cancelled - valid input
        have_port   := False;    // No new port
      end;
    end;

   us_get_new_port := have_port;     // Return state
 end;
{---End us_get_new_port --------------------------------------------}

{====================================================================
 Get a valid web page to display.
 This function returns a web page to display at server start-up.
 Where appropriate maps the selected file to a server alias.

 Out:    Returns True if a valid page is selected.
 Output: Valid page is returned in variable web_page

 Example of valid page formats:

  file:///G:\us_traymenu\docs\English\index.html
  http://UENV_US_SERVERNAME:UENV_AP_PORT/index.php
  http://UENV_US_SERVERNAME:UENV_AP_PORT/us_splash/index.php
  http://UENV_US_SERVERNAME:UENV_AP_PORT/us_opt1/index.php      - phpMyAdmin
  http://UENV_US_SERVERNAME:UENV_AP_PORT/us_opt2/?username=     - Adminer
  http://UENV_US_SERVERNAME:UENV_AP_PORT/us_extra/phpinfo.php

  Configured Aliases:
    Alias:                Absolute path:
    Alias /              "US_ROOTF/www/"  - Server root
    Alias /              "US_ROOTF/ssl/"  - Server root

    Alias /cgi-bin       "US_ROOTF/cgi-bin/
    Alias /us_docs       "US_ROOTF/docs/"
    Alias /us_extra      "US_ROOTF/home/us_extra/"
    Alias /us_mongoadmin "US_ROOTF/home/us_mongoadmin/"
    Alias /us_pear       "US_ROOTF/home/us_pear/"
    Alias /us_opt1       "US_ROOTF/home/us_opt1/"        - us_phpmyadmin
    Alias /us_opt2       "US_ROOTF/home/us_opt2/"
    Alias /us_splash     "US_ROOTF/home/us_splash/"
    Alias /test_access   "US_ROOTF/home/us_access/www/"
====================================================================}
function us_get_valid_web_page(var web_page:string): boolean;
 var
   openDialog : TOpenDialog; // Open dialog variable
   str1      :String;  // Initial Title displayed
   str2      :String;  // Invalid selection Title display
   valid     :boolean; // Is file selected valid - loop until valid user selection
   ret_state :boolean; // Resturn value. If file is valided and not blank return true
   file_name :String;  // User selected file - Full path returned - back slashes
   www_temp  :string;  // Temp containg path with back slash
   ssl_temp  :string;  // Temp containg path with back slash
 begin
   valid     :=false; // Assume invlid selection - Used in loop until a valid selection made
   ret_state :=true;  // Assume valid web_page selected
   file_name :='';    // User selected full path (back slashes)

   str1      := 'Select file to display at start-up';
   str2      := 'Invalid selection! ';

   while not valid do                        //Repeat until user makes a valid selection or clicks Cancel
    begin
      openDialog := TOpenDialog.Create(nil);                // Create the open dialog object
      openDialog.InitialDir :=UniConPath;                    // Set initial to top-level server
      openDialog.Options := [ofFileMustExist];               // Only allow existing files to be selected
      openDialog.Title:=str1;                                // Set initial title string
      openDialog.Filter := 'Web Pages .php .htm .html .pl .cgi|*.php;*.htm;*.html;*.pl;*.cgi'; // Set file filter

      if openDialog.Execute then                                // File selected or canceled
        begin
         file_name := openDialog.Filename;     // Full path of selected file

         //Set correct slash for server-root test - Back slashes required.
         www_temp  := StringReplace(UENV_US_ROOTF_WWW, '/','\',[rfReplaceAll]);  // Path e.g replace / with \
         ssl_temp  := StringReplace(UENV_US_ROOTF_SSL, '/','\',[rfReplaceAll]);  // Path e.g replace / with \

         //Check the path contains a valid folder - Server-root or installation-root
         If ExecRegExpr(QuoteRegExprMetaChars(www_temp),   file_name) Then valid := true; // Valid input
         If ExecRegExpr(QuoteRegExprMetaChars(ssl_temp),   file_name) Then valid := true; // Valid input
         If ExecRegExpr(QuoteRegExprMetaChars(US_HOME),    file_name) Then valid := true; // Valid input
         If ExecRegExpr(QuoteRegExprMetaChars(US_CGI_BIN), file_name) Then valid := true; // Valid input

         If not valid Then
           begin
              openDialog.Free;    // Release dialog
              str1:=str2;         // Set title string to invalid
           end;
        end
      else                        // User clicked Cancel
        begin
          openDialog.Free;        // Release dialog
          ret_state := false;     // Not a valid web page
          valid     := true;      // Cancel is a valid input exit loop
        end;
     end;

   //=== Convert page selected to Alias ===========
   //- Converts file absolute-paths to server reltive-paths
   If ret_state Then        // A valid page was selected
     begin
      openDialog.Free;    // Release dialog
      web_page := StringReplace(file_name, '\','/',[rfReplaceAll]); //Server uses forward slashes

      //Set server-root Alias which is just a forward slash
      web_page := StringReplace(web_page, UENV_US_ROOTF_WWW+'/','/',[rfReplaceAll]);
      web_page := StringReplace(web_page, UENV_US_ROOTF_SSL+'/','/',[rfReplaceAll]);

      //Set server configuraed Alias
      web_page := StringReplace(web_page,UniConPath_F+'/cgi-bin/',            '/cgi-bin/',      [rfReplaceAll]);
      web_page := StringReplace(web_page,UniConPath_F+'/docs/',               '/us_docs/',      [rfReplaceAll]);
      web_page := StringReplace(web_page,UniConPath_F+'/home/us_extra/',      '/us_extra/',     [rfReplaceAll]);
      web_page := StringReplace(web_page,UniConPath_F+'/home/us_mongoadmin/', '/us_mongoadmin/',[rfReplaceAll]);
      web_page := StringReplace(web_page,UniConPath_F+'/home/us_pear/',       '/us_pear/',      [rfReplaceAll]);
      web_page := StringReplace(web_page,UniConPath_F+'/home/us_opt1/',       '/us_opt1/',      [rfReplaceAll]); //phpMyAdmin
      web_page := StringReplace(web_page,UniConPath_F+'/home/us_opt2/',       '/us_opt2/',      [rfReplaceAll]); //Adminer
      web_page := StringReplace(web_page,UniConPath_F+'/home/us_splash/',     '/us_splash/',    [rfReplaceAll]);
      web_page := StringReplace(web_page,UniConPath_F+'/home/us_access/www/', '/test_access/',  [rfReplaceAll]);
     end
   Else
      web_page := ''; //Not used, returns a dummy string

   //--Returns true - page selected false - User clicked Cancel button
   us_get_valid_web_page := ret_state;
 end;
{---End us_get_valid_web_page --------------------------------------}


{====================================================================
 Get new root-folder (www).
 This function returns a new root-folder.

 Input:  root either www or ssl

 Out:    Returns True if a valid folder is selected.
 Output: Valid folder is returned in variable server_root

====================================================================}
function us_get_new_root_folder(var server_root:string;root:string): boolean;
 var
   SelectDirDialog : TSelectDirectoryDialog; // Folder dialog variable
   str1            :String;  // Initial Title displayed
   str2            :String;  // Invalid selection Title display
   valid           :boolean; // Is folder selected valid - loop until valid user selection
   ret_state       :boolean; // Resturn value. If folder is valided and not blank return true
   folder_name     :String;  // User selected folder - Full path returned - back slashes
   current_folder  :string;  // Current root-folder
 begin
   valid       :=false; // Assume invlid selection - Used in loop until a valid selection made
   ret_state   :=true;  // Assume valid folder selected
   folder_name :='';    // User selected full path (back slashes)

   str1 := '';
   str1 := str1 + 'Select new server-root folder ('+root+')'+ sLineBreak;
   str1 := str1 + 'Note: For portability folder must be below folder UniServerZ';

   str2 := '';
   str2 := str2 + 'Invalid selection for ('+root+')!'+ sLineBreak;
   str2 := str2 + 'Note: For portability folder must be below folder UniServerZ';

   while not valid do                        //Repeat until user makes a valid selection or clicks Cancel
    begin
      SelectDirDialog := TSelectDirectoryDialog.Create(nil);  // Create the dir dialog object
      SelectDirDialog.Title:=str1;                            // Set initial title string

      //Set Initial path
      If root='www' Then
        begin
         current_folder :=  StringReplace(UENV_US_ROOTF_WWW, '/','\',[rfReplaceAll]); // Replace forward with back slashes
        end
      Else    //root=ssl
        begin
         current_folder :=  StringReplace(UENV_US_ROOTF_SSL, '/','\',[rfReplaceAll]); // Replace forward with back slashes
        end;

      //If current_folder starts with \ add the application drive e.g C:
      If Pos('\',current_folder) = 1 then
       begin
         current_folder := ExtractFileDrive(UniConPath)+current_folder;        //Add C:
       end;

      SelectDirDialog.InitialDir :=current_folder;                             // Set initial to current folder selected

      //Run dialog
      if SelectDirDialog.Execute then                        // File selected or canceled
        begin
         folder_name := SelectDirDialog.Filename;            // Full path of selected file

         //Check the path does not contain spaces - Server-root
         If Not ExecRegExpr(' ', folder_name) Then valid := true; // Valid input

         If not valid Then           // Not a valid folder
           begin
              SelectDirDialog.Free;  // Release dialog
              str1:=str2;            // Set title string to invalid
           end;
        end
      else                           // User clicked Cancel
        begin
          SelectDirDialog.Free;     // Release dialog
          ret_state := false;       // Not a valid folder
          valid     := true;        // Cancel is a valid input exit loop
        end;
     end;

   //=== Valid folder ===========

   If ret_state Then               // A valid page was selected
     begin
      SelectDirDialog.Free;        // Release dialog
      server_root := folder_name;  //
     end
   Else
      server_root := ''; //Not used, returns a dummy string

   //--Returns true - page selected false - User clicked Cancel button
   us_get_new_root_folder := ret_state;
 end;
{---End us_get_new_root_folder --------------------------------------}


{====================================================================
This function returns the first word from a string.
The first word is removed from the string which is directely updated.
The word boundry (Delimiter) can be a string such as #13#10 CRLF or
a single chracter such as a ' ' space.

GetWord   - Return value first-word of input string.
ALine     - Input string directely updated by this function
Delimiter - Any character or string.
====================================================================}
function us_get_word(var ALine: String; Delimiter: String): String;
var
  Delimiter_Position: Integer;
begin
  Delimiter_Position := Pos(Delimiter, ALine);    //Look for delimiter
  if (Delimiter_Position > 0) Then                //Delimiter found
   begin
     //Return word and remove from line
     us_get_word := Copy(ALine, 1, Delimiter_Position-1);
     ALine:= Copy(ALine,Delimiter_Position + Length(Delimiter),Length(ALine));
   end
   else                                           //Delimiter not found
   begin
     us_get_word := ALine;                       //Return line as last word
     ALine := '';                                //Set line to nothing.
  end;
end;
{--- End us_get_word -----------------------------------------------}

{====================================================================
RelToAbsDir:
 Convert a relative path to absolute
 If the path is already absolute no change
 Relative paths are referenced to application UniController.exe folder
c:  Disk                     - Absolute
/   Root of the current disk - Absolute Is on the current drive
                               either hard or USB drive
.   Current directory        - Relative Current folder UniServerZ
..  Parent directory         - Relative to folder UniServerZ
=====================================================================}
function RelToAbsDir(BaseDirIn: string; DirIn:string): string;
var
  BaseDir: string;     //Base dir application folder
  Dir: string;         //User set dir
  DirTemp: string;     //Tem results
  StrLst: TStringList; //List to hold split dir
  i:Integer;           //Loop counter
begin
 DirTemp := '';  // Set initial value

 //Replace \ with /
 BaseDir := StringReplace(BaseDirIn, '\','/',[rfReplaceAll]);
 Dir     := StringReplace(DirIn, '\','/',[rfReplaceAll]);
 Result := Dir;  // None of the below Return unchanged

 //Check already absolute and return unchanged.
 If Pos('/',Dir) = 1 then Result := Dir; // Return absolute path unchanged
 If Pos(':',Dir) = 2 then Result := Dir; // Return absolute path unchanged

 //Check relative path. Relative to application folder e.g UniServerZ.
 If Pos('./',Dir) = 1 then
  begin
    Dir    := StringReplace(Dir, '.','',[]); //Current folder UniServerZ
    Dir    := BaseDir + Dir;                 //Add dir-folder to base e.g c:/.../UniServerZ/somefolder
    Result := Dir;                           //Return Absolute path
  end;

 //Check for parent directory markers '..'
 If Pos('..',Dir) = 1 then             //Markers detected relative path to folder UniServerZ
   Begin
     StrLst := TStringList.Create;     //Create string list
     StrLst.Delimiter := '/';          //Set delimiter to '/' for splitting BaseDir string
     StrLst.DelimitedText := BaseDir;  //Split BaseDir at each '/'

      While Pos('..',Dir) = 1 do                  //Traverse up BaseDir-tree
        Begin
          Dir := StringReplace(Dir, '../','',[]); //Remove relative reference '../'
          StrLst.Delete(StrLst.Count-1);          //Remove last entry. Moves up one dir-level
          If (StrLst.Count = 1) Then Break;       //No more levels exit while loop
        end;

      //Check content of stringlist
      If (StrLst.Count = 1) Then          //No more levels. Hence at root e.g C:
       Begin
         Result := StrLst[0] +'/' +Dir;   //Is top-level (Drive letter) add Dir e.g C:/somefolder
       end
      Else   //StrLst contains additional path folder elements. Re-assemble Dir from this list
       begin
          for i := 0 to StrLst.Count-1 do      // Scan list for all additional folders
          DirTemp := DirTemp + StrLst[i] +'/'; // Assemble dir include '/' dir seperator

          DirTemp := DirTemp + Dir;            // Add the relative-dir nolonger has markers '../'
          Result := DirTemp;                   // Return resulting absolute path string
      end;
     StrLst.Free;    // Release the memory
   end;
end;
{--- End RelToAbsDir ------------------------------------------------}


//=== Apache ====

{====================================================================
ApachePortFree: This function checks the specified Apache host and
                port are free to use
Input:          Host: Apache 0.0.0.0 (all)
                Port: UENV_AP_PORT
Output:         True:  Port is free to use
                False: Port in use
=====================================================================}
function ApachePortFree:boolean;
begin
  //result := PortAvailable('0.0.0.0','80');
  result := PortAvailable('0.0.0.0',UENV_AP_PORT);

end;
{--- End ApachePortFree --------------------------------------------}

{====================================================================
ApacheSSLPortFree: This function checks the specified Apache host and
                   port are free to use
Input:             Host: Apache 0.0.0.0 (all))
                   Port: UENV_AP_SSL_PORT
Output:            True:  Port is free to use
                   False: Port in use
=====================================================================}
function ApacheSSLPortFree:boolean;
begin
  //result := PortAvailable('0.0.0.0','443');
  result := PortAvailable('0.0.0.0',UENV_AP_SSL_PORT);
end;
{--- End ApacheSSLPortFree -----------------------------------------}

{====================================================================
us_get_apache_exe: Get Apache executable file name
 Gets server exe name directely from bin folder
 Returns exe name or default httpd_z.exe
====================================================================}
function us_get_apache_exe(): string;

var
  sList_Files  :TStringList;   // List of files in folder
  i            :Integer;       // Loop counter
  exe_name     :string;        // name.exe
begin
  us_get_apache_exe := 'httpd_z.exe';     // Set default name

  //Get all exe files in Apache bin folder
  sList_Files  := FindAllFiles(US_APACHE_BIN, '*.exe',false); // Do not search subs

  //Scan list of files
  for i:=0 to sList_Files.Count-1 do              // Scan file line by line
   begin
     exe_name := ExtractFileName(sList_Files[i]); // Get File name
     if ExecRegExpr('^httpd_[\d|\w]+\.exe$',  exe_name) Then
      begin
         us_get_apache_exe := exe_name; // Set exe name
         Break;
      end;
    end;

  //Clean
  sList_Files.Free; // Remove from memory  
end;
{---End us_get_apache_exe------------------------------------------}



{====================================================================
 ApacheRunning():
 This function checks Apache server is running
 Returns true if running otherwise returns false
 ====================================================================}
 function ApacheRunning():boolean;
   begin
    ApacheRunning := us_IsProcessRunning(AP_EXE_NAME);
   end;
{--- End ApacheRunning() -------------------------------------------}


{*****************************************************************************
Is SSL enabled in Apache config file.
  Returns true if SSL enabled
=============================================================================}
function SSL_Enabled:boolean;          
var
  sList: TStringList;                // String list
  i: Integer;                        // Loop counter

begin
  SSL_Enabled := False; // Assume not enabled
 //-- Get current configuration from core\apache2\conf\httpd.conf
If FileExists(USF_APACHE_CNF) Then
 begin
   sList := TStringList.Create;           // Create object
   sList.LoadFromFile(USF_APACHE_CNF);    // Load file
   for i:=0 to sList.Count-1 do           // Scan file line by line
     begin
      if ExecRegExpr('^LoadModule ssl_module modules/mod_ssl.so', sList[i]) then  // Match found
        begin
         SSL_Enabled := True;            // SSL is enabled
         Break;                          // Nothing else to do
        end;
     end;
  sList.Free;     // remove from memory
 end;
end;
{----------------------------------------------------------------------------}


//=== MySQL ====

{====================================================================
MySqlPortFree: This function checks the specified MySQL host and
               port are free to use
Input:         Host:  MySQL 127.0.0.1 (local)
               Port:  ENV_MTSQL_TCP_PORT
Output:        True:  Port is free to use
               False: Port in use
=====================================================================}
function MySqlPortFree:boolean;
begin
  //result := PortAvailable('127.0.0.1','3306');
  result := PortAvailable('127.0.0.1',UENV_MYSQL_TCP_PORT);
end;
{--- End MySqlPortFreee ---------------------------------------------}

{====================================================================
us_get_mysql_exe:  Get MySQL executable file name
 Gets server exe name directely from bin folder
 Returns exe name or default mysqld_z.exe
====================================================================}
function us_get_mysql_exe():string;
var
  sList_Files  :TStringList;   // List of files in folder
  i            :Integer;       // Loop counter
  exe_name     :string;        // name.exe
begin
  us_get_mysql_exe := 'mysqld_z.exe';  // Set default name

  //Get all exe files in MySQL bin folder
  sList_Files  := FindAllFiles(US_MYSQL_BIN, '*.exe',false); // Do not search subs

  //Scan list of files
  for i:=0 to sList_Files.Count-1 do          // Scan file line by line
    begin
     exe_name := ExtractFileName(sList_Files[i]);  // Get File name
     if ExecRegExpr('^mysqld_[\d|\w]+\.exe$',  exe_name) Then
      begin
         us_get_mysql_exe := exe_name; // Set exe name
         Break;
      end;
    end;

  //Clean
  sList_Files.Free; // Remove from memory
end;
{---End us_get_mysql_exe--------------------------------------------}

{====================================================================
 MysqlRunning():
 This function checks MySQL server is running
 Returns true if running otherwise returns false
====================================================================}
function MysqlRunning():boolean;
 begin
   MysqlRunning := us_IsProcessRunning(MY_EXE_NAME);
 end;
{--- End MysqlRunning() -------------------------------------------}


{====================================================================
 MysqlServerReady():
 This function checks MySQL server is ready to server data
 Returns true if ready otherwise returns false
 Uses mysqladmin ping

 Changes
 -------
 2019-09-07 : mysqld is alive may not be the first line, added in a loop for this
====================================================================}
function MysqlServerReady(root_password:string):boolean;
var
  sList       : TStringList; // Save output from ping
  AProcess    : TProcess;    // Process handle
  i           : integer;     // Loop counter
begin
  MysqlServerReady := False;                                        // Assume not ready
  sList     := TStringList.Create;                                  // Create the TStringList object.
  AProcess  := TProcess.Create(nil);                                // Create new process

  AProcess.Executable := US_MYSQL_BIN + '\' + 'mysqladmin.exe';       // Executable to run
  AProcess.Parameters.Add('--host=' + US_DB_HOST);                    // Parameter server e.g 127.0.0.1
  AProcess.Parameters.Add('--port=' + UENV_MYSQL_TCP_PORT);           // Parameter mysql server port
  AProcess.Parameters.Add('--user=root');                             // User is root
  AProcess.Parameters.Add('--password='+root_password);               // Use new password
  AProcess.Parameters.Add('ping');                                    // Ping server returns "mysqld is alive" it is ready

  AProcess.Options  := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Options  := AProcess.Options + [poNoConsole];              // Set option no console
  AProcess.Execute;                                                   // Run command

  sList.LoadFromStream(AProcess.Output);     // Read ping output
    if sList.Count > 0 then                  // Check data was read
      for i:=0 to sList.Count-1 do
      begin
       if (sList[i] = 'mysqld is alive') then  // Check returned message
         MysqlServerReady := True;             // MySQL server is ready
         break;
      end;
  sList.Free;    // Free memory
  AProcess.Free; // Free memory

end;
{--- End MysqlServerReady() ----------------------------------------}


{====================================================================
 Get MySQL password from file.

====================================================================}
function us_get_mysql_password(): string;
 var
  myFile : TextFile;
  text   : string;
 begin
 text := 'root';                    // Assume default password
 If FileExists(USF_MYSQL_PWD) Then
    begin
      AssignFile(myFile, USF_MYSQL_PWD); // Assign file handle
      Reset(myFile);                     // Open the file for reading

       while not Eof(myFile) do           // Read to end of file
         ReadLn(myFile, text);            // Read file contents line

      CloseFile(myFile);                 // Close the file
    end;

 us_get_mysql_password := text;     // Return password
 end;
{---End us_get_mysql_password() ------------------------------------}

{====================================================================
 Get new MySQL password from user.
  Input:
   old_pwd - Current value of password to be changed
   new_pwd - variable pointer for return value
  Output:
   true  - Returns true if user enter a valid port
   false - Returns false if user canceled/closed window.

  Uses Forms, LCLType, Dialogs, Controls;
====================================================================}
function us_get_new_pwd(old_pwd:string;var new_pwd:string): boolean;
 var
   pwd_title : string;
   pwd_text  : string;
   s1         : string;
   s2         : string;
   s3         : string;

   valid_input: Boolean;
   have_pwd   : Boolean;
   UserStr    : string;

 begin
  UserStr     :='';        // Set initial value
  new_pwd     := old_pwd;  // Set initial value to old pwd
  valid_input := False;    // Assume input is invalid or cancelled

  //Set text to use
  pwd_title   := US_MYMAR_TXT+' root password - current value '+ old_pwd;
  pwd_text    := ' Enter new '+US_MYMAR_TXT+' root password or press cancel:';
  s1 := ' Invalid password! ';
  s3 := sLineBreak + sLineBreak +' Please enter a valid password or press cancel:';

  while Not valid_input do
   begin
    If InputQuery(pwd_title, pwd_text, UserStr) Then
     begin
       valid_input := True;            // Assume valid input -set flag

       //==Check data entered by user

       //-- Check for blank 
       If valid_input then
        begin
           If UserStr = '' Then
             begin
               valid_input := False;
               s2 := ' Cannot be blank.';
               pwd_text := s1 + s2 + s3;
             end;
        end;

       //-- Check for a space - Although valid can cause problems!!
        If valid_input then
        begin
           If ExecRegExpr('^.*\s.*$', UserStr) Then
             begin
               valid_input := False;
               s2 := ' Cannot contain a space.';
               pwd_text := s1 + s2 + s3;
             end;
        end;


       //-- Check for valid characters
        If valid_input then
        begin
           //^[\x21\x23-\x26\x28-\x5D\x5F\x61-\x7E]*$ <- Old Regex Code before modification
           If Not ExecRegExpr('^[\x21-\x7E]*$', UserStr) Then
             begin
               valid_input := False;
               s2 := sLineBreak + ' Valid characters: Any printable character excluding' + sLineBreak;
               s2 :=         s2 + ' space, quotes and some special characters.';
               pwd_text := s1 + s2 + s3;
             end;
        end;

        //-- Check pwd contains upto 41 characters
        If valid_input then
        begin
           If Length(UserStr) > 41 Then
            begin
             valid_input := False;
             s2 := ' Maximum number of characters is 41';
             pwd_text := s1 + s2 + s3;
            end;
        end;

        //==END Check data entered by user

        //==Set new pwd value
        If valid_input then
         begin
          have_pwd := True;    // Have a valid value for pwd
          new_pwd  := UserStr; // Set pwd value
         end;
     end
    Else
      begin
        valid_input := True;     // User cancelled - valid input
        have_pwd    := False;    // No new pwd
      end;
    end;

   us_get_new_pwd := have_pwd;     // Return state
 end;
{---End us_get_new_pwd -------------------------------------------------------}

{=============================================================================
us_mysql_batch
 Runs myql in batch mode.
 Input:  SQL statments (mysql_str) to run.
 Output: Results returned in mysql_result_list.
 Note 1: Uses: process and RegExpr
 Note 2: Buffers result of mysql.exe this can exceed 2K pipe
 Note 3: Using batch mode avoids a user having to install a driver.

 Example of command-line string (displays list of databases):-
 mysql.exe --no-defaults --host=127.0.0.1 --port=3306 --user=root --password=root --silent mysql --execute="show databases"
 In the above mysql_str = "show databases"
=============================================================================}
function us_mysql_batch(mysql_str:string;var mysql_result_list:TStringList):boolean;
const
  READ_BYTES = 2048;          // set buffer size
var
  sList:       TStringList;   // StringList
  MemStream:   TMemoryStream; // Memory buffer
  AProcess:    TProcess;      // Process handle object
  NumBytes:    LongInt;       // Number of bytes
  BytesRead:   LongInt;       // Bytes read

begin
  BytesRead := 0;                    // Tracker
  MemStream := TMemoryStream.Create; // Temp Memorystream - buffer output

  //--Run command string.
  AProcess := TProcess.Create(nil);   // Create new process

  AProcess.Executable := US_MYSQL_BIN + '\mysql.exe';        // Path to MySQL exe
  AProcess.Parameters.Add('--no-defaults');                  // Parameter no default file
  AProcess.Parameters.Add('--host=' + US_DB_HOST);           // Parameter server e.g 127.0.0.1
  AProcess.Parameters.Add('--port=' + UENV_MYSQL_TCP_PORT);  // Parameter mysql server port
  AProcess.Parameters.Add('--user=root');                    // Parameter root user
  AProcess.Parameters.Add('--password=' + MY_PWD);           // Parameter root user password
  AProcess.Parameters.Add('--silent');                       // Set batch mode - remove formatting
  AProcess.Parameters.Add('mysql');                          // Select database to use
  AProcess.Parameters.Add('--execute="' + mysql_str + '"');  // SQL command/s to run

  AProcess.Options := AProcess.Options + [poUsePipes];   // Note cannot use poWaitOnExit
  AProcess.Options := AProcess.Options + [poNoConsole];  // Set option no console
  AProcess.Execute;                                      // Run program

  while AProcess.Running do
   begin
    MemStream.SetSize(BytesRead + READ_BYTES);  // make sure we have enough memory space
    NumBytes := AProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES); // try reading it
    if NumBytes > 0 then       // Data to read
      Inc(BytesRead, NumBytes)
    else                       // no data, wait 100 ms
      Sleep(100);
   end;

   // read last part
   repeat
     MemStream.SetSize(BytesRead + READ_BYTES); // make sure we have enough memory space
     NumBytes := AProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES); // try reading it
     if NumBytes > 0  then Inc(BytesRead, NumBytes); // Data to read
   until NumBytes <= 0;

   MemStream.SetSize(BytesRead);     // Set memory size to bytes actually read

   sList := TStringList.Create;      // Create string list and
   sList.LoadFromStream(MemStream);  // Load with data read

   AProcess.Free;  // Free process
   MemStream.Free; // Free memory buffer

  //Set return status
  If (sList.Count=0) Then
   us_mysql_batch := False            // Failed to get result from MySQL server
  Else
   begin
     us_mysql_batch    := True;       // Result from MySQL server
     mysql_result_list.Assign(sList); // Assign list
   end;

   sList.Free;     // Release process

 end;
{--- End us_mysql_batch -----------------------------------------------------}
 


{****************************************************************************
Validate MySQL user name
=============================================================================}
function us_valid_mysql_user_name(str:string):boolean;
begin
  if ExecRegExpr('^[A-Za-z][\d\w@_-]+$', str) then  // Match found
    us_valid_mysql_user_name := True   // Match found hence valid
  else
    us_valid_mysql_user_name := False; // Does not match
end;
{--- End us_valid_mysql_user_name ------------------------------------------}

{****************************************************************************
Validate MySQL user password
=============================================================================}
function us_valid_mysql_password(str:string):boolean;
begin
  if ExecRegExpr('^[A-Za-z][\d\w@_-]+$', str) then  // Match found
    us_valid_mysql_password := True  // Match found hence valid
  else
    us_valid_mysql_password := False; // Does not match
end;
{--- End us_valid_mysql_password --------------------------------------------}

{****************************************************************************
Validate MySQL user database name
=============================================================================}
function us_valid_mysql_db_name(str:string):boolean; 
begin
  if ExecRegExpr('^[A-Za-z][\d\w@_-]+$', str) then  // Match found
   us_valid_mysql_db_name := True // Match found hence valid
  else
   us_valid_mysql_db_name := False; // Does not match
end;
{--- End us_valid_mysql_db_name --------------------------------------------}

{****************************************************************************
Validate MySQL database backup file name
=============================================================================}
function us_valid_mysql_backup_file_name(str:string):boolean; 
begin
  if ExecRegExpr('^[A-Za-z][\d\w@_-]+\.sql$', str) then  // Match found
   us_valid_mysql_backup_file_name := True // Match found hence valid
  else
   us_valid_mysql_backup_file_name := False; // Does not match
end;
{--- End us_valid_mysql_backup_file_name ------------------------------------}


{****************************************************************************
us_start_mysql_skip_grants:
  // Start MySQL no grants returns true if started and ready.

  mysqld_z.exe --defaults-file="C:\UniserverZ\core\mysql\my.ini" --skip-grant-tables

  Changes:
  --------
  2019-09-07 : SudeepJD: Add in the --shared-memory for MySQL8
=============================================================================}
function us_start_mysql_skip_grants():boolean;

Var
 AProcess: TProcess;
 saftey_loop: Integer;
 Failed:Boolean;
begin
    Failed:=False;   //Assume not failed

    saftey_loop:=0;
    //--Run command string.
    AProcess := TProcess.Create(nil);                                   // Create new process

    AProcess.Executable := US_MYSQL_BIN + '\' + MY_EXE_NAME;            // Executable to run
    AProcess.Parameters.Add('--defaults-file="' + USF_MYSQL_INI+ '"');  // Force use of default file
    AProcess.Parameters.Add('--skip-grant-tables');                     // Temp folder
    If StrtoInt(MY_SQL_VER)>=8 Then
      AProcess.Parameters.Add('--shared-memory');                       //Shared Memory Start for MySQL8


    AProcess.Options  := AProcess.Options + [poNoConsole];              // Set option no console
    AProcess.Execute;                                                   // Run command
    AProcess.Free;                                                      // Release memory

    Repeat                                         // Wait for MySQL server to start
     begin
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
       sleep(100);
        saftey_loop := saftey_loop +1;              // Increment counter

        If saftey_loop > (USC_MY_StartSafetyTime*10) Then
         begin
           us_MessageDlg('Warning',US_MYMAR_TXT+' server failed to become ready', mtWarning,[mbOk],0) ; //Display warning message
           Failed := True;
           Break;
         end;
      end;
     Until MysqlServerReady('root'); //Note password not required this is just a dummy
   end;

 If Failed Then
   us_start_mysql_skip_grants := False  // Unable to start or not ready
 Else
   us_start_mysql_skip_grants := True;  // MySQL running and ready
end;
{--- End us_start_mysql_skip_grants -----------------------------------------}



//=== USER INPUT VALIDATION ===

{=============================================================================
 Root Folder Name
   Input: root_folder_name - Name to validate
          display_str      - String to display

   Ouput: Returns true for valid data
   Alerts user, states problem detected.
=============================================================================}
function valid_root_folder_name(root_folder_name:string;display_str:string): boolean;
 var
  valid_input :boolean;
 begin
  valid_input := True;    // Assume input is invalid

  //-- Check for blank
  If root_folder_name = '' Then
    begin
      valid_input := False;
      SysUtils.Beep; // Alert user
      us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be blank.', mtError,[mbOk],0) ; //Display error message
    end;

  //-- Check for a space
  If valid_input Then
    begin
     If ExecRegExpr('^.*\s.*$', root_folder_name) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
         us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot contain a space.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

  //-- Check folder name looks resonable e.g fred.com
  If valid_input Then
    begin
     If Not ExecRegExpr('^[a-zA-Z0-9]+[a-zA-Z0-9\-_]*$', root_folder_name) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
         us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Incorrect format.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

  valid_root_folder_name := valid_input;
end;
{---End valid_root_folder_name ----------------------------------------------}

{=============================================================================
 Server Name
   Input: server_name - Name to validate
          display_str - String to display

   Ouput: Returns true for valid data
   Alerts user, states problem detected.
=============================================================================}
function valid_server_name(server_name:string;display_str:string): boolean;
 var
  valid_input :boolean;
 begin
  valid_input := True;    // Assume input is invalid

  //-- Check for blank
  If server_name = '' Then
    begin
      valid_input := False;
      SysUtils.Beep; // Alert user
      us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be blank.', mtError,[mbOk],0) ; //Display error message
    end;

  //-- Check for a space
  If valid_input Then
    begin
     If ExecRegExpr('^.*\s.*$', server_name) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
         us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot contain a space.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

  //-- Check domain name looks resonable e.g fred.com
  If valid_input then
   begin
      If server_name = 'localhost' Then
        begin
          valid_input := True;
        end
      Else
        begin
         If Not ExecRegExpr('^[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,4}(/\S*)?$', server_name) Then
          begin
            valid_input := False;
            SysUtils.Beep; // Alert user
            us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Incorrect format.', mtError,[mbOk],0) ; //Display error message
          end;
        end;
   end;
  valid_server_name := valid_input;
end;
{---End valid_server_name ---------------------------------------------------}

{=============================================================================
 Server Port
   Input: server_port - Port to validate
          display_str - String to display

   Ouput: Returns true for valid data
   Alerts user, states problem detected.
=============================================================================}
function valid_server_port(server_port:string;display_str:string): boolean;
 var
  valid_input :boolean;
 begin
  valid_input := True;    // Assume input is invalid

  //-- Check for blank
  If server_port = '' Then
    begin
      valid_input := False;
      SysUtils.Beep; // Alert user
      us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be blank.', mtError,[mbOk],0) ; //Display error message
    end;

  //-- Check for a space
  If valid_input Then
    begin
     If ExecRegExpr('^.*\s.*$', server_port) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
         us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot contain a space.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

  //-- Check for all digits
   If valid_input then
   begin
      If Not ExecRegExpr('^[0-9]*$', server_port) Then
        begin
          valid_input := False;
          SysUtils.Beep; // Alert user
          us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Only digits 0-9 allowed.', mtError,[mbOk],0) ; //Display error message
        end;
   end;

   //-- Check port contains only 5 digits range 0-65535
   If valid_input then
   begin
      If Length(server_port) > 5 Then
       begin
        valid_input := False;
         us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Only 5 digits allowed 0-65535', mtError,[mbOk],0) ; //Display error message
       end;
   end;

   //-- Check port vaild range 0-65535
   If valid_input then
   begin
      If StrToInt(server_port) > 65535 Then
       begin
        valid_input := False;
        us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Valid port values 0-65535', mtError,[mbOk],0) ; //Display error message
       end;
   end;

  valid_server_port := valid_input;
end;
{---End valid_server_port ---------------------------------------------------}

{=============================================================================
 Admin E-mail
   Input: admin_email - Port to validate
          display_str - String to display

   Ouput: Returns true for valid data
   Alerts user, states problem detected.
=============================================================================}
function valid_admin_email(admin_email:string;display_str:string): boolean;
 var
  valid_input :boolean;
 begin
  valid_input := True;    // Assume input is invalid

  //-- Check for blank
  If admin_email = '' Then
    begin
      valid_input := False;
      SysUtils.Beep; // Alert user
      us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be blank.', mtError,[mbOk],0) ; //Display error message
    end;

  //-- Check for a space
  If valid_input Then
    begin
     If ExecRegExpr('^.*\s.*$', admin_email) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
        us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot contain a space.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

   //-- Check for valid format
  If valid_input Then
    If ExecRegExpr('^\w*@localhost$', admin_email) Then
      begin
        valid_input := True;
      end
    Else
     begin
      If Not ExecRegExpr('^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}$', admin_email) Then
        begin
          valid_input := False;
          SysUtils.Beep; // Alert user
          us_MessageDlg('Error',display_str + ':' + sLineBreak + 'E-mail invalid format', mtError,[mbOk],0) ; //Display error message
        end;
      end;

  valid_admin_email := valid_input;
end;
{---End valid_admin_email ---------------------------------------------------}


{=============================================================================
 Directory Index Files
   Input: directory_index - Directory Index Files to validate
          display_str - String to display

   Ouput: Returns true for valid data
   Alerts user, states problem detected.
=============================================================================}
function valid_directory_index_files(directory_index:string;display_str:string): boolean;
 var
  valid_input :boolean;
 begin
  valid_input := True;    // Assume input is invalid

  //-- Check for blank
  If directory_index = '' Then
    begin
      valid_input := False;
      SysUtils.Beep; // Alert user
        us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be blank.', mtError,[mbOk],0) ; //Display error message
    end;

  //-- Check line for one or more spaces
  If valid_input Then
    begin
     If ExecRegExpr('^\s+$', directory_index) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
         us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be a line of spaces.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

  //-- Check for valid format
  //--Existing line: DirectoryIndex index.html index.shtml index.html.var index.htm index.php3 index.php index.pl index.cgi
  If valid_input Then
     begin
      If Not ExecRegExpr('^(index\.\w[0-9]?\s|index\.\w{2,5}\s|index\.\w{2,5}\.var\s)*(index\.\w{2,5}[0-9]?|index\.\w{2,5}|index\.\w{2,5}\.var)$', directory_index) Then
        begin
          valid_input := False;
          SysUtils.Beep; // Alert user
          us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Invalid format', mtError,[mbOk],0) ; //Display error message
        end;
      end;


  valid_directory_index_files := valid_input;
end;
{---End valid_directory_index_files -----------------------------------------}

{=============================================================================
 Server parsed ssi file extensions
   Input: ssi_extensions - SSI etentions to validate
          display_str - String to display

   Ouput: Returns true for valid data
   Alerts user, states problem detected.
=============================================================================}
function valid_ssi_file_extensions(ssi_extensions:string;display_str:string): boolean;
 var
  valid_input :boolean;
 begin
  valid_input := True;    // Assume input is invalid

  //-- Check for blank
  If ssi_extensions = '' Then
    begin
      valid_input := False;
      SysUtils.Beep; // Alert user
      us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be blank.', mtError,[mbOk],0) ; //Display error message
    end;

  //-- Check line for one or more spaces
  If valid_input Then
    begin
     If ExecRegExpr('^\s+$', ssi_extensions) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
         us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be a line of spaces.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

  //-- Check for valid format
  //--Existing line: .shtml .shtm .sht
  If valid_input Then
     begin
      If Not ExecRegExpr('^(\.\w{3,5}\s)*(\.\w{3,5})$', ssi_extensions) Then
        begin
          valid_input := False;
          SysUtils.Beep; // Alert user
          us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Invalid format', mtError,[mbOk],0) ; //Display error message
        end;
      end;


  valid_ssi_file_extensions := valid_input;
end;
{---End valid_ssi_file_extensions -------------------------------------------}

{=============================================================================
 Server Signature
   Input: server_signature - Server Signature to validate
          display_str - String to display

   Ouput: Returns true for valid data
   Alerts user, states problem detected.
=============================================================================}
function valid_server_signature(server_signature:string;display_str:string): boolean;
 var
  valid_input :boolean;
 begin
  valid_input := True;    // Assume input is invalid

  //-- Check for blank
  If server_signature = '' Then
    begin
      valid_input := False;
      SysUtils.Beep; // Alert user
      us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be blank.', mtError,[mbOk],0) ; //Display error message
    end;

  //-- Check for a space
  If valid_input Then
    begin
     If ExecRegExpr('^.*\s.*$', server_signature) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
         us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot contain a space.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

  //-- Check for value On Off
   If valid_input then
   begin
      If Not ExecRegExpr('^(On|Off|EMail)$', server_signature) Then
        begin
          valid_input := False;
          SysUtils.Beep; // Alert user
          us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Vaild values are: On Off.', mtError,[mbOk],0) ; //Display error message
        end;
   end;

  valid_server_signature := valid_input;
end;
{---End valid_server_signature ---------------------------------------------}

{=============================================================================
 IP Address
   Input: ip_address     - Name to validate
          display_str    - String to display

   Ouput: Returns true for valid data
   Alerts user, states problem detected.
=============================================================================}
function valid_ip_address(ip_address:string;display_str:string): boolean;
 var
  valid_input :boolean;
 begin
  valid_input := True;    // Assume input is invalid

  //-- Check for blank
  If ip_address = '' Then
    begin
      valid_input := False;
      SysUtils.Beep; // Alert user
      us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot be blank.', mtError,[mbOk],0) ; //Display error message
    end;

  //-- Check for a space
  If valid_input Then
    begin
     If ExecRegExpr('^.*\s.*$', ip_address) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
         us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Cannot contain a space.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

  //-- Check IP address looks resonable e.g 127.0.0.1
    If valid_input Then
    begin
     If Not ExecRegExpr('^([0-9]{1,3}\.){3}[0-9]{1,3}$', ip_address) Then
       begin
         valid_input := False;
         SysUtils.Beep; // Alert user
          us_MessageDlg('Error',display_str + ':' + sLineBreak + 'Incorrect format.', mtError,[mbOk],0) ; //Display error message
       end;
    end;

  valid_ip_address := valid_input;
end;
{---End valid_ip_address ---------------------------------------------------}

//=== ACCESS AND PASSWORD ===


{=============================================================================
 Root folder www  .htaccess file: Check exists, create if not
   Ouput: Returns true if .htaccess file exists (or was created)
=============================================================================}
function us_www_htaccess_check_create: boolean;
 var
   ret    : boolean;      // Return value
   sList  : TStringList;  // String list
   str    : string;

 begin
  ret := false;

  if FileExists(USF_WWW_HTACCESS) then
     ret := true  // File exists return true
  Else            // Ask user to allow .htaccess file creation
    begin

    str := '';
    str := str +  '.htaccess file does not exist.'                 + sLineBreak + sLineBreak;


    str := str +  'Implementing either of the following:'                      + sLineBreak;
    str := str +  'Requires an Apache .htaccess configuration file'            + sLineBreak + sLineBreak;

    str := str +  'a) Restrict access to root folder www and all sub-folders'  + sLineBreak;
    str := str +  'b) Password protect root folder www'                        + sLineBreak;
    str := str +  'c) Allow cgi-script execution in this root folder'          + sLineBreak;
    str := str +  'd) Mod rewrite'                                             + sLineBreak + sLineBreak;


    str := str +  'Do you wish to create the .htaccess file?'                  + sLineBreak;
    str := str +  '(Recommendation create file)'                               + sLineBreak;

    if us_MessageDlg('Root folder www .htaccess file does not exist', str, mtConfirmation,[mbYes, mbNo],0) = mrYes then
      begin                                                        // Create .htaccess file
       sList := TStringList.Create;                                // Create object

       sList.Add('#------------------------------------------------------------------------------');
       sList.Add('# Server root folder www .htaccess');
       sList.Add('# This file provides server security limiting access to the localhost only.');
       sList.Add('# Comment next four lines to deactivate. (Allows external access)');
       sList.Add('#------------------------------------------------------------------------------');
       sList.Add('');
       sList.Add('Order Deny,Allow');
       sList.Add('Deny from all');
       sList.Add('Allow from 127.0.0.1');
       sList.Add('Allow from ::1');
       sList.Add('');
       sList.Add('#------------------------------------------------------------------------------');
       sList.Add('# To allow execution of cgi scripts in this directory uncomment next two lines.');
       sList.Add('#------------------------------------------------------------------------------');
       sList.Add('');
       sList.Add('AddHandler cgi-script .pl .cgi');
       sList.Add('Options +ExecCGI +FollowSymLinks');
       sList.Add('');
       sList.Add('#------------------------------------------------------------------------------');
       sList.Add('# Activate this section to use the Private Server Feature!');
       sList.Add('# Defaults: Username - root; Password - root');
       sList.Add('# Note AuthUserFile: File path is relative to server root');
       sList.Add('# To lock server, uncomment the next 4 lines. (A name and password is required)');
       sList.Add('#------------------------------------------------------------------------------');
       sList.Add('');
       sList.Add('#AuthName "Uniform Server - Server Access"');
       sList.Add('#AuthType Basic');
       sList.Add('#AuthUserFile ../../htpasswd/www/.htpasswd');
       sList.Add('#Require valid-user');
       sList.Add('');

       sList.SaveToFile(USF_WWW_HTACCESS); // Save new host to file
       sList.Free;                         // remove from memory

       if FileExists(USF_WWW_HTACCESS) then
          ret := true    // File exists return true
       Else
         ret := false;   // File does not exists return false
      end
    Else               // .htaccess file does not exist - not created
      begin
         ret := false;  // does not exist - not created
      end;
 end;
 us_www_htaccess_check_create := ret;
end;
{---End us_www_htaccess_check_create -----------------------------------------------}

{=============================================================================
 Root folder SSL .htaccess file: Check exists, create if not
   Ouput: Returns true if .htaccess file exists (or was created)
=============================================================================}
function us_ssl_htaccess_check_create: boolean;
 var
   ret    : boolean;      // Return value
   sList  : TStringList;  // String list
   str    : string;

 begin
  ret := false;

  if FileExists(USF_SSL_HTACCESS) then
     ret := true  // File exists return true
  Else            // Ask user to allow .htaccess file creation
    begin

    str := '';
    str := str +  '.htaccess file does not exist.'                 + sLineBreak + sLineBreak;


    str := str +  'Implementing either of the following:'                      + sLineBreak ;
    str := str +  'Requires an Apache .htaccess configuration file'            + sLineBreak + sLineBreak;

    str := str +  'a) Restrict access to root folder ssl and all sub-folders'  + sLineBreak;
    str := str +  'b) Password protect root folder ssl'                        + sLineBreak;
    str := str +  'c) Allow cgi-script execution in this root folder'          + sLineBreak;
    str := str +  'd) Mod rewrite'                                             + sLineBreak + sLineBreak;


    str := str +  'Do you wish to create the .htaccess file?'                  + sLineBreak;
    str := str +  '(Recommendation create file)'                               + sLineBreak;

    if us_MessageDlg('Root folder ssl .htaccess file does not exist', str, mtConfirmation,[mbYes, mbNo],0) = mrYes then
      begin         // Create .htaccess file
      sList := TStringList.Create;                                // Create object

      sList.Add('#------------------------------------------------------------------------------');
      sList.Add('# Server root folder ssl .htaccess');
      sList.Add('# This file provides server security limiting access to the localhost only.');
      sList.Add('# Comment next four lines to deactivate. (Allows external access)');
      sList.Add('#------------------------------------------------------------------------------');
      sList.Add('');
      sList.Add('Order Deny,Allow');
      sList.Add('Deny from all');
      sList.Add('Allow from 127.0.0.1');
      sList.Add('Allow from ::1');
      sList.Add('');
      sList.Add('#------------------------------------------------------------------------------');
      sList.Add('# To allow execution of cgi scripts in this directory uncomment next two lines.');
      sList.Add('#------------------------------------------------------------------------------');
      sList.Add('');
      sList.Add('AddHandler cgi-script .pl .cgi');
      sList.Add('Options +ExecCGI +FollowSymLinks');
      sList.Add('');
      sList.Add('#------------------------------------------------------------------------------');
      sList.Add('# Activate this section to use the Private Server Feature!');
      sList.Add('# Defaults: Username - root; Password - root');
      sList.Add('# Note AuthUserFile: File path is relative to server root');
      sList.Add('# To lock server, uncomment the next 4 lines. (A name and password is required)');
      sList.Add('#------------------------------------------------------------------------------');
      sList.Add('');
      sList.Add('#AuthName "Uniform Server - Server Access"');
      sList.Add('#AuthType Basic');
      sList.Add('#AuthUserFile ../../htpasswd/www/.htpasswd');
      sList.Add('#Require valid-user');
      sList.Add('');

      sList.SaveToFile(USF_SSL_HTACCESS); // Save new host to file
      sList.Free;                         // remove from memory

      if FileExists(USF_SSL_HTACCESS) then
         ret := true    // File exists return true
      Else
        ret := false;   // File does not exists return false
      end
    Else             // .htaccess file does not exist - not created
      begin
       ret := false;  // does not exist - not created
      end;
 end;
 us_ssl_htaccess_check_create := ret;
end;
{---End us_ssl_htaccess_check_create -----------------------------------------------}

{====================================================================
This function checks password file for default name:password
root:root

Input: password_file - Full path of file to be checked

Output returns true - containns default
Alerts user to delete these
=====================================================================}
function password_file_contains_defaults(password_file:string):boolean;
var
    sList: TStringList;   // String list
    i:integer;            // Loop counter
begin
   password_file_contains_defaults := False; //Assume no defaults
   // Load password file
   if FileExists(password_file) Then
    begin
       sList := TStringList.Create;          // Create object
       sList.LoadFromFile(password_file);    // Load file

       //Scan list
       for i:=0 to sList.Count-1 do
         begin
           If Pos('root:root',sList[i]) = 1 then
            begin
              password_file_contains_defaults := True; //Defaults found
              msg_password_file_contains_defaults;
              break;
            end;
         end;
    end;
   sList.Free;                         // Remove from memory
end;
{---End password_file_contains_defaults---------------------------------------}

{====================================================================
This function checks password file is empty

Input: password_file - Full path of file to be checked
Output returns true  - No name-password pair entries in file
Alerts user password file empty
=====================================================================}
function password_file_empty(password_file:string):boolean;
var
    sList: TStringList;   // String list
    i:integer;            // Loop counter
begin
   password_file_empty := True; //Assume empty
   // Load password file
   if FileExists(password_file) Then
    begin
       sList := TStringList.Create;          // Create object
       sList.LoadFromFile(password_file);    // Load file

       //Scan list
       for i:=0 to sList.Count-1 do
         begin
           If Pos(':',sList[i]) > 0 then
            begin
              password_file_empty := False; //Defaults found
              break;
            end;
         end;
    end;

    If password_file_empty Then
       msg_password_file_empty;

   sList.Free;                         // Remove from memory
end;
{---End password_file_empty ---------------------------------------}

//=== GET WEB DATA ===

{====================================================================
This function gets ip address as seen fom the Internet
It uses the web-pge http://myip.dtdns.com

Output returns true  - ip-address obtained
Output ip            - returns ip-address to variable ip

Uses:  httpsend - From synapse
        httpsend.pas - Add to proect
=====================================================================}
function get_ip_address(var ip:string):boolean;
var
  ret             :boolean;   // True if ip address obtained
  us_web_page_data: boolean;  // True: File dta received
  HTTP   :THTTPSend;          // Var for object
  sList  :tstringlist;        // Storage for returned page
begin
   ip  :='';                      // Set initial value
   ret := False;                  // Assume ip not found
   us_web_page_data := False;     // Assume no reply

   HTTP   := THTTPSend.Create;    // Create object
   sList  := TStringList.create;  // Create list storage
   try
       HTTP.Timeout := 2000;      //Defaults to 5000
       if HTTP.HTTPMethod('GET', 'http://myip.dtdns.com') then
       begin
          us_web_page_data := True;            // Received reply
          sList.loadfromstream(Http.Document); // Save data in sList
       end;
   finally
     HTTP.Free;
   end;

   If us_web_page_data Then  // IP Received
     begin
       ip := sList[0];       // Ip address is first line in list
       ret := True;          // IP obtained
     end;

   get_ip_address := ret;   // Return status

   //Clean up
   sList.free;              // Free List

end;
{---End get_ip_address ----------------------------------------------}

{====================================================================
This function gets current Uniform Server version from a file
hosted at uniformserver.com

Output returns true  - version obtained
Output version       - returns version obtained to variable version

Uses:  httpsend - From synapse
        httpsend.pas - Add to proect

VERSION_FILE_ADDRESS  = 'http://www.uniformserver.com/system/.version2';
=====================================================================}
function get_version_information(var version:string):boolean;
var
  ret              :boolean;  // True if version obtained
  us_web_page_data: boolean;  // True: File data received
  HTTP   :THTTPSend;          // Var for object
  sList  :tstringlist;        // Storage for returned page
begin
   version  :='';                 // Set initial value
   ret      := False;             // Assume version not found
   us_web_page_data := False;     // Assume no reply

   HTTP   := THTTPSend.Create;    // Create object
   sList  := TStringList.create;  // Create list storage
   try
       HTTP.Timeout := 2000;      //Defaults to 5000
       if HTTP.HTTPMethod('GET', VERSION_FILE_ADDRESS) then
       begin
          us_web_page_data := True;            // Received reply
          sList.loadfromstream(Http.Document); // Save data in sList
       end;
   finally
     HTTP.Free;
   end;

   If us_web_page_data Then  // version Received
     begin
       version := sList[0];  // version is first line in list
       ret := True;          // version obtained
     end;

   get_version_information := ret;   // Return status

   //Clean up
   sList.free;              // Free List

end;
{---End get_version_information --------------------------------------}


{====================================================================
This function checks server is accessible from internet

Output returns true  - accessible from internet

Uses:  httpsend - From synapse
        httpsend.pas - Add to proect

uses file:   UniServerZ\home\us_access\www\index.html
web address: http://ip_address:port/us_test_access/index.html

=====================================================================}
function server_accessible_from_internet:boolean;
var
  us_page_url      :string;       // Url of page to read
  ip               :string;       // Server ip address seen from Internet
  ret              :boolean;      // True if accessible
  HTTP             :THTTPSend;    // Var for object
begin
   ret  := False;   // Assume not accessible
   ip   :='';       //Set initial value

  //Must have an ip address to obtain data from index.html
  If get_ip_address(ip) Then     // ip address obtained
    begin
      //Create page url
      us_page_url := 'http://' + ip +':'+ UENV_AP_PORT +'/us_test_access/index.html';

      //==Get web page
      HTTP   := THTTPSend.Create;    // Create object
      try
          HTTP.Timeout := 2000;      //Defaults to 5000
          if HTTP.HTTPMethod('GET', us_page_url) then
             ret := True;            // Received reply
      finally
        HTTP.Free;
      end;
    end

  Else //No ip address
    ret      := False;              // Not accessible no ip address

  //Return status
  server_accessible_from_internet := ret;


end;
{---End server_accessible_from_internet ----------------------------}

//=== PALE MOON BROWSER ===

{====================================================================
 PaleMoonPortableRunning():
 This function checks Pale Moon Portable browser is running
 Returns true if running otherwise returns false
 PALE_MOON_EXE   palemoon.exe  Palemoon execuitable name
 ====================================================================}
 function PaleMoonPortableRunning():boolean;
   begin
    PaleMoonPortableRunning := us_IsProcessRunning(PALE_MOON_EXE);
   end;
{--- End PaleMoonPortableRunning() ----------------------------------}

{====================================================================
EnumWindowsProc:
This function is called from the enumeration function for each windows
colletion that is ready for processing.

It gets a visible windows title bar and checks for text match set in
variable UNIQUE_TEXT_IN_TITLE_BAR 'UniServer Zero 11'

Note uses windows
=====================================================================}
function EnumWindowsProc(WHandle: HWND; LParM: LParam): LongBool;StdCall;Export;
var
 Title:array[0..255] of char;
 sTitle:STRING ;
begin
 GetWindowText(wHandle, Title,255);
 sTitle:=Title;

 If IsWindowVisible(wHandle) then
   begin
      If Pos(UNIQUE_TEXT_IN_TITLE_BAR,sTitle) <> 0 then
        begin
           palemoon_ready_flag:=True;  // Window found
           Result:= False;             // Nothing else to do stop iteration
        end
      Else
        Result := True; //Get next window collection
   end
 Else
   Result:=True; //Get next window collection
end;
{--- End EnumWindowsProc -------------------------------------------}

{====================================================================
palemoon_ready:
This function assume palemoon is running, checks window is displayed.
A window displayed indicates Pale Moon is ready.

EnumWindows:
The EnumWindows function passes the handle of each top level window,
in turn, to the defined callback function EnumWindowsProc.
On completion returns. The global flag palemoon_ready_flag is tested.

BOOL EnumWindows(
  WNDENUMPROC lpEnumFunc, // pointer to callback function
  LPARAM lParam           // application-defined value if not
);                        // used set it to 0

Note uses windows
=====================================================================}
function palemoon_ready():boolean;
begin
  palemoon_ready_flag:=False;       // Assume not ready reset flag
  EnumWindows(@EnumWindowsProc,0);  // Initiate enumeration/callback
  If palemoon_ready_flag then       // Flag set in call back function
   palemoon_ready := True           // Windows title text found - ready
  Else
   palemoon_ready := False;         // Not ready - failed
end;
{--- End palemoon_ready ---------------------------------------------}

//=== DEFAULT BROWSER ===

{====================================================================
 DefaultBrowserRunning():
 This function checks Default browser is running
 Returns true if running otherwise returns false
 DEFAULT_BROWSER_EXE Default browser execuitable name found using
                     Reference html page. See Standard browser ready.
 ====================================================================}
 function DefaultBrowserRunning():boolean;
   begin
    DefaultBrowserRunning := us_IsProcessRunning(DEFAULT_BROWSER_EXE);
   end;
{--- End DefaultBrowserRunning() ------------------------------------}


{====================================================================
EnumWindowsProc2:
This function is called from the enumeration function for each windows
colletion that is ready for processing.

It gets a visible windows title bar and checks for text match set in
variable UNIQUE_TEXT_IN_TITLE_BAR 'UniServer Zero 11'

Sets flag: default_browser_ready_flag
Sets global varible DEFAULT_BROWSER_EXE exe name of default browser.

Note uses windows
=====================================================================}
function EnumWindowsProc2(WHandle: HWND; LParM: LParam): LongBool;StdCall;Export;
var
 Title:array[0..255] of char;
 sTitle:STRING ;
 pPid : DWORD;
begin
 pPid := 0;
 GetWindowText(wHandle, Title,255);       //Get Title bar text
 GetWindowThreadProcessId(wHandle,pPid);  //Get process pid
 sTitle:=Title;

 If IsWindowVisible(wHandle) then
   begin
      If Pos(UNIQUE_TEXT_IN_TITLE_BAR,sTitle) <> 0 then
        begin
           DEFAULT_BROWSER_EXE := GetAppName(IntToStr(pPid)); //Save default exe name
           default_browser_ready_flag:=True;                  // Window found
           Result:= False;                   // Nothing else to do stop iteration
        end
      Else
        Result := True; //Get next window collection
   end
 Else
   Result:=True; //Get next window collection
end;
{--- End EnumWindowsProc2 -------------------------------------------}

{====================================================================
default_browser_ready:
This function assume default browser is running, checks window is displayed.
A window displayed indicates default browser is ready.

EnumWindows:
The EnumWindows function passes the handle of each top level window,
in turn, to the defined callback function EnumWindowsProc2.
On completion returns. The global flag default_browser_ready_flag 
is tested.

Note: If the default_browser_ready_flag is set variable
      DEFAULT_BROWSER_EXE containes the default browser exe name 

BOOL EnumWindows(
  WNDENUMPROC lpEnumFunc, // pointer to callback function
  LPARAM lParam           // application-defined value if not
);                        // used set it to 0

Note uses windows
=====================================================================}
function default_browser_ready():boolean;
begin
  default_browser_ready_flag:=False;  // Assume not ready reset flag
  EnumWindows(@EnumWindowsProc2,0);   // Initiate enumeration/callback
  If default_browser_ready_flag then  // Flag set in call-back function
   default_browser_ready := True      // Windows title text found - ready
  Else
   default_browser_ready := False;    // Not ready - failed
end;
{--- End default_browser_ready ---------------------------------------}

//=== MESSAGE DIALOG ===

{====================================================================
us_message_dlg:
 Message dialouge functions under Win8 are restricted to a specific width.
 This restriction breaks text string formats and is particulay worst with
 translated languages.
 This function replaces the standard MessageDlg function to resolve the
 above issue.

 The following dialouge functions are used in Uniform Server:
    MessageDlg(str_title, str_msg, mtInformation,  [mbOk],        0) ; //Display message
    MessageDlg(str_title, str_msg, mtError,        [mbOk],        0) ; // Display message
    MessageDlg(str_title, str_msg, mtWarning,      [mbOk],        0) ;
    MessageDlg(str_title, str_msg, mtcustom,       [mbOk],        0) ; //Display information message
 if MessageDlg(str_title, str_msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
=====================================================================}
function us_MessageDlg(aCaption:string; aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var
  new_form: Tus_message_dlg;
begin
  new_form := Tus_message_dlg.Create(Application);  // Create a new instance of us_message_dlg

  new_form.us_heading := aCaption;;  // Heading displayed in window title box (caption)
  new_form.us_message := aMsg;       // Formated string to display

  If (DlgType = mtCustom)       Then new_form.us_image := 'Custom'; // Name of image to display
  If (DlgType = mtInformation)  Then new_form.us_image := 'Information';
  If (DlgType = mtConfirmation) Then new_form.us_image := 'Confirmation';
  If (DlgType = mtError)        Then new_form.us_image := 'Error';
  If (DlgType = mtWarning)      Then new_form.us_image := 'Warning';

  If buttons =[mbOK]        Then new_form.us_buttons := 'Ok';   // Name of buttons to display
  If buttons =[mbYes, mbNo] Then new_form.us_buttons := 'YesNo';

  us_MessageDlg :=  new_form.ShowModal; //Display form and return result from form
end;
{--- End us_MessageDlg -----------------------------------------------}


//--- End ---
end.

