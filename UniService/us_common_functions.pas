unit us_common_functions;

{#############################################################################
'# Name: us_common_functions.pas
'# Common functions for UniService
'# Developed By: The Uniform Server Development Team
'#
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ServiceManager, JwaWinSvc,
  default_config_vars,
  FileUtil,
  Dialogs,
  RegExpr;

//=== GENERAL ===
function RelToAbsDir(BaseDirIn: string; DirIn:string): string; //Convert absolute/relative to absolute paths
function All_config_files_exist:boolean;      //Check all config files exist
function All_config_files_backed_up :boolean; //Back up config files and check they exist

//=== Apache
function us_get_apache_exe():string;                    // Returns Apache executable file name

//=== MySQL
function us_get_mysql_exe():string;                      // Returns MySQL executable file name

//=== Services ===
function us_ServiceInstalled(service_name:string): Boolean; // Check named service is installed
function us_IsServiceRunning(ServiceName: string): boolean; // Check named service is running

implementation

//=== GENERAL ===
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

{====================================================================
All_config_files_exist:
 Checks all configuration files exist.
 Informs user which files are missing.
 Returns: True   - All files found
          False  - One or more files not found

=====================================================================}
function All_config_files_exist:boolean;
var
 str:string;         //List of files that do not exist
 all_exists:boolean; //All files exist
 i:integer; //loop counter
begin
 str := '';
 all_exists := true; // Assume all configuration files exist

 //==Check all files in stringlist
 for i:=0 to StrAllSource.Count-1 do
   begin
     If Not FileExists(StrAllSource[i]) Then            // Check file exists
       begin
        all_exists:= false;                             // Does not exist set flag
        str := str + StrAllSource[i] + sLineBreak;      // Add file to output string
       end;
   end;

  //==End Check  files

  //==IF one or more files missing inform user
  If not all_exists Then
   begin
     str := 'The following configuration files were not found:' + sLineBreak + sLineBreak + str + sLineBreak;
     str :=  str + 'Please resolve this issue and try again.'+ sLineBreak;
     showmessage(str);
   end;
  Result := all_exists;
end;

{--- End All_config_files_exist -------------------------------------}

{====================================================================
All_config_files_backed_up:
 Creates a backup directory if it does not exist.
 Then back-up configuration files to this directory
 Check these back-ups exist
 Inform user which files that where not backed-up.
 Returns: True   - All files backed-up
          False  - One or more files not backed-up

=====================================================================}
function All_config_files_backed_up :boolean;
var
 str:string;                 //Save missing back-up files
 all_backups_exist:boolean;  //Check back-up files exist
 i:integer;                  //loop counter
begin
  str :='';
  all_backups_exist:=true; //Assume all back-up files exist
  Result := True;          //Assume all back-up files exists

 If DirectoryExists(US_CORE_SB) Then  // Check directory exists
  begin
    Result := True;                   //Folder exists assume all files are backed-up
  end
 Else
  begin
    //== Create backup folder
    If ForceDirectories(US_CORE_SB) Then   // If directory does not exist create it
     Begin
       //Directory created copy configuration files to it
       for i:=0 to StrAllSource.Count-1 do
        begin
          CopyFile(StrAllSource[i], StrAllBackup[i]);        // Save source to backup files
        end;

        //***** Check backup files exist *******
        for i:=0 to StrAllBackup.Count-1 do
          begin
            If Not FileExists(StrAllBackup[i]) Then            // Check backup file exists
              begin
               all_backups_exist:= false;                      // Does not exist set flag
               str := str + StrAllBackup[i] + sLineBreak;      // Add file to output string
              end;
          end;

         //==IF one or more back-up files missing inform user
         If not all_backups_exist Then
          begin
           str := 'The following back-up files were not found:' + sLineBreak + sLineBreak + str + sLineBreak;
           str :=  str + 'Please resolve this issue and try again.'+ sLineBreak;
           showmessage(str);
           Result := all_backups_exist;
         end;

         //***** End Check backup files exist ****

     end
    Else  // Failed to create directory 
     begin
      Result := false; 
      showmessage('Failed to create configuration back-up folder');
     end;
  end;
end;

{--- End All_config_files_backed_up  -------------------------------}

//=== Apache
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

//=== MySQL

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


//=== Services ===


{====================================================================
us_ServiceInstalled
 This function Checks if a named Windows service is installed.
 Returns True if installed otherwise returns False
 Note: Uses ServiceManager, JwaWinSvc
====================================================================}
function us_ServiceInstalled(service_name:string): Boolean; // Check named service is installed
var
    SCManHandle, SvcHandle: SC_Handle;
    sService: PChar;
  begin
    sService := PChar(service_name);
    // Open service manager handle.
    SCManHandle := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
    if (SCManHandle > 0) then
    begin
      SvcHandle := OpenService(SCManHandle, sService, SERVICE_QUERY_STATUS);
      // if Service installed
      if (SvcHandle > 0) then
        begin
          Result := True;
          CloseServiceHandle(SvcHandle);
        end
      else
          Result := False;
      CloseServiceHandle(SCManHandle);
    end;
  end;
{End us_ServiceInstalled -----------------------------------------------------}

{====================================================================
This function Checks if a named Windows service is running.
Returns True if running otherwise returns False
Note: Uses ServiceManager, JwaWinSvc
Fails if service not installed.
====================================================================}

function us_IsServiceRunning(ServiceName: string): boolean;
    {description Checks if a Windows service is running}
  var
    Services: TServiceManager;
    ServiceStatus: TServiceStatus;
  begin
    //Check for existing services
    //equivalent to sc query <servicename>
    Services := TServiceManager.Create(nil);
    try
      try
       Services := TServiceManager.Create(nil);
       Services.Access := SC_MANAGER_CONNECT; //Acces typo in property.
       Services.Connect;                     //Connect
       Services.GetServiceStatus(ServiceName, ServiceStatus);

        if ServiceStatus.dwCurrentState = SERVICE_RUNNING then
        begin
          Result := True;
        end
        else
        begin
          Result := False;
        end;
        Services.Disconnect;
      except
        on E: EServiceManager do
        begin
          // A missing service might throw a missing handle exception? No?
        {LogOutput('Error getting service information for ' + ServiceName +
          '. Technical details: ' + E.ClassName + '/' + E.Message);}
          Result := False;
          raise; //rethrow original exception
        end;
        on E: Exception do
        begin
        {LogOutput('Error getting service information for ' + ServiceName +
          '. Technical details: ' + E.ClassName + '/' + E.Message);
          }
          Result := False;
          raise; //rethrow original exception
        end;
      end;
    finally
      Services.Free;
    end;
  end;
{End us_IsServiceRunning -----------------------------------------------------}
end.

