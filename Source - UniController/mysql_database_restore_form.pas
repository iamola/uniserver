unit mysql_database_restore_form;

{#############################################################################
'# Name:  mysql_database_restore_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#
'#
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  default_config_vars,
  us_common_functions,
  Process;

type

  { Tmysql_database_restore }

  Tmysql_database_restore = class(TForm)
    Btn_help: TButton;
    Btn_restore: TButton;
    Btn_clear: TButton;
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    procedure Btn_helpClick(Sender: TObject);
    procedure Btn_restoreClick(Sender: TObject);
    procedure Btn_clearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  mysql_database_restore: Tmysql_database_restore;

implementation

{$R *.lfm}

{====================================================
 Set initial conditions
----------------------------------------------------}
procedure mysql_database_restore_init;
begin
  mysql_database_restore.Btn_restore.Enabled := False;  // Disable button
  mysql_database_restore.ListBox1.ItemIndex  := -1;     // Clear database back up file selection
end;
{--- End mysql_database_restore_init ---------------}


{====================================================
 Run Restore:
 Input: Full path to sql restore file

 The mysql.exe utility is used for restoring. The following general line
 is used for restoring:
 mysql -u root -p[root_password] -h[host] [database_name] < dumpfilename.sql
 Note: Not possible to use redirection < hence use --execute instead

 More specific:
 mysql --no-defaults --host=127.0.0.1 --port=3306 --user=root --password=root  --execute="source restore_file.sql"

----------------------------------------------------}
procedure run_mysql_db_restore(restore_file:String);
Var
  AProcess:    TProcess;      // Process handle object
begin
  //--Run command string.
  AProcess := TProcess.Create(nil);   // Create new process

  AProcess.Executable := US_MYSQL_BIN + '\mysql.exe';        // Path to MySQL exe
  AProcess.Parameters.Add('--no-defaults');                  // Parameter no default file
  AProcess.Parameters.Add('--host=' + US_DB_HOST);           // Parameter server e.g 127.0.0.1
  AProcess.Parameters.Add('--port=' + UENV_MYSQL_TCP_PORT);  // Parameter mysql server port
  AProcess.Parameters.Add('--user=root');                    // Parameter root user
  AProcess.Parameters.Add('--password=' + MY_PWD);           // Parameter root user password
  AProcess.Parameters.Add('--execute="source ' + restore_file + '"'); // SQL source file command/s to run

  AProcess.Options  := AProcess.Options + [poNoConsole];     // Set option no console
  AProcess.Execute;                                          // Run command
  AProcess.Free;                                             // Release process
end;
{--- End run_mysql_db_restore ----------------------}


{====================================================
 Display list of database backup files in ListBox1
 Note: Only the file name and extension displayed
       easier for user to read.
----------------------------------------------------}
procedure display_database_backup_files;
var
 sList: TStringList;    // String list
 i:integer;             // Loop counter
begin
  mysql_database_restore.ListBox1.ItemIndex := -1;  // Clear selection
  mysql_database_restore.ListBox1.Items.Clear;      // Clear list box

  // Get list of files in db_backup_restore folder
  sList := TStringList.Create;                                // Create object
  sList := FindAllFiles(US_DB_BACKUP_RESTORE, '*.sql',false); // Create list. Do not search subs

  //Extract file name and add extension .sql
  for i:=0 to sList.Count-1 do
  begin
    sList[i] := ExtractFileNameOnly(sList[i]+'.sql');         // name + extention
  end;

  mysql_database_restore.ListBox1.Items.Assign(sList) ;       // Add string list to Listbox

  sList.Free; // Remove from memory
end;
{--- End display_database_backup_files -----------------------}


{ Tmysql_database_restore }

procedure Tmysql_database_restore.FormCreate(Sender: TObject);
begin

  //User may have deleted backup folder
  If Not(US_DB_BACKUP_RESTORE='') Then                 // There is a value for file
   If DirectoryExists(US_MYSQL) Then                  // Check MySQl Module installed
    If Not DirectoryExists(US_DB_BACKUP_RESTORE) Then // Folder does not exist
      ForceDirectories(US_DB_BACKUP_RESTORE);         // we create it
end;

procedure Tmysql_database_restore.FormShow(Sender: TObject);
begin
  mysql_database_restore.Caption := US_MYMAR_TXT+' Database Restore'; // MySQL Database Restore
  mysql_database_restore_init;   // Set initial conditions
  display_database_backup_files; // Get and display backup files
end;

procedure Tmysql_database_restore.ListBox1Click(Sender: TObject);
begin
  If Not (ListBox1.ItemIndex = -1) Then  // A selection was made
   Btn_restore.Enabled := True;          // Enable restore button
end;

procedure Tmysql_database_restore.Btn_clearClick(Sender: TObject);
begin
  mysql_database_restore_init; // Set initial conditions
end;

procedure Tmysql_database_restore.Btn_restoreClick(Sender: TObject);
var
 db_file_name:String;
begin
  db_file_name := ListBox1.Items[ListBox1.ItemIndex];  // Get selected file name and extension
  db_file_name := US_DB_BACKUP_RESTORE + '\' + db_file_name; // Add full path

  run_mysql_db_restore(db_file_name);    // Run restore
  mysql_database_restore_init;           // Reset initial conditions

  showmessage('Restore complete');
end;

procedure Tmysql_database_restore.Btn_helpClick(Sender: TObject);
var
 str:string;
begin
  str :='';
  str := str +  'This menu item restores a '+US_MYMAR_TXT+' database backup.'           + sLineBreak+ sLineBreak;
  str := str +  'Select a file from the list and click the Restore button.'  + sLineBreak;

  us_MessageDlg('Create a '+US_MYMAR_TXT+' database backup - Help ', str, mtInformation,[mbOk],0) ; //Display message

end;

end.

