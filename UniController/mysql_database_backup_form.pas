unit mysql_database_backup_form;

{#############################################################################
'# Name: mysql_database_backup_form_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  default_config_vars,
  us_common_functions,
  Process;

type

  { Tmysql_database_backup }

  Tmysql_database_backup = class(TForm)
    Btn_clear: TButton;
    Btn_help: TButton;
    Btn_create_backup: TButton;
    ChB_display_all: TCheckBox;
    Ed_db_name: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ListBox1: TListBox;
    procedure Btn_clearClick(Sender: TObject);
    procedure Btn_helpClick(Sender: TObject);
    procedure Btn_create_backupClick(Sender: TObject);
    procedure ChB_display_allClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  mysql_database_backup: Tmysql_database_backup;

implementation

{$R *.lfm}

{====================================================
 Set initial conditions
----------------------------------------------------}
procedure mysql_database_backup_init;
begin
  mysql_database_backup.Ed_db_name.Text    := '';
  mysql_database_backup.ListBox1.ItemIndex := -1;           // Clear database selection
end;
{--- End mysql_database_backup_init ----------------}


{====================================================
 Run DB Dump:
 Input: db_name       - Database name to be dump
 Input: db_file_name  - Full path to output file for dump e.g C:\fred\backup_name.sql

 The mysqldump.exe utility is used for backup. The following general line
 is used for dumping database:
 mysql -u root -p[root_password] -h[host] [database_name] < dumpfilename.sql
 mysqldump --user=root --password=root --host=127.0.0.1 --databases db_name > output_file.sql
 Note: Not possible to use redirection > hence use --result-file instead

 More specific:
 mysqldump --no-defaults --host=127.0.0.1 --port=3306 --user=root --password=root  --force --result-file=db_file_name.sql --databases db_name

----------------------------------------------------}
procedure run_mysql_db_backup(db_name:String;db_file_name:String);
Var
  AProcess:    TProcess;      // Process handle object
begin
  //--Run command string.
  AProcess := TProcess.Create(nil);   // Create new process

  AProcess.Executable := US_MYSQL_BIN + '\mysqldump.exe';    // Path to MySQL exe
  AProcess.Parameters.Add('--no-defaults');                  // Parameter no default file
  AProcess.Parameters.Add('--host=' + US_DB_HOST);           // Parameter server e.g 127.0.0.1
  AProcess.Parameters.Add('--port=' + UENV_MYSQL_TCP_PORT);  // Parameter mysql server port
  AProcess.Parameters.Add('--user=root');                    // Parameter root user
  AProcess.Parameters.Add('--password=' + MY_PWD);           // Parameter root user password
  AProcess.Parameters.Add('--force');                        // Ignore errors
  AProcess.Parameters.Add('--result-file=' + db_file_name);  // Parameter full path to output file
  AProcess.Parameters.Add('--databases');                    // Parameter databases
  AProcess.Parameters.Add(db_name);                          // Parameter databasees to backup

  AProcess.Options  := AProcess.Options + [poNoConsole];     // Set option no console
  AProcess.Execute;                                          // Run command
  AProcess.Free;                                             // Release process
end;
{--- End run_mysql_db_backup ----------------------}



{====================================================
 Display list of databases in ListBox1
----------------------------------------------------}
procedure display_databases;
var
 sList: TStringList;    // String list
 i:integer;             // Loop counter
begin
  sList  := TStringList.Create;             // Create object
  us_mysql_batch('show databases;',sList);  // Run batch

  mysql_database_backup.ListBox1.Items.Clear; //Clear list box

    //Scan returned list and add to ListBox1
    //Select state: Either cxclude the databases listed or display all
    for i:=0 to sList.Count-1 do
      begin
       If (mysql_database_backup.ChB_display_all.State=cbUnChecked) Then // Hide these databases
        begin
          if     not (sList[i]= 'Database')
             and not (sList[i]= 'information_schema')
             and not (sList[i]= 'mysql')
             and not (sList[i]= 'phpmyadmin')
             and not (sList[i]= 'performance_schema') then
             begin
                //Add filtered dbs to listbox
                mysql_database_backup.ListBox1.Items.Add(sList[i]);
             end;
        end
       Else // Display all databases
        begin
         mysql_database_backup.ListBox1.Items.Add(sList[i]);
        end;
      end;
    sList.Free;                           // Remove from memory
end;
{--- End display_databases ---------------------------------}

{ Tmysql_database_backup }

procedure Tmysql_database_backup.FormShow(Sender: TObject);
begin
    mysql_database_backup.Caption := US_MYMAR_TXT + ' Database Backup'; // MySQL Database Backup
    mysql_database_backup_init; // Set initial conditions
    display_databases;          //Display list of databases in ListBox1
end;

procedure Tmysql_database_backup.Btn_clearClick(Sender: TObject);

begin
  mysql_database_backup_init; //  Clear
end;

procedure Tmysql_database_backup.Btn_helpClick(Sender: TObject);
 var
  str:string;
begin
  str :='';
  str := str +  'This menu item creates a '+US_MYMAR_TXT+' database backup.' + sLineBreak+ sLineBreak;

  str := str +  'You can change the backup file or'     + sLineBreak;
  str := str +  'use the displayed default file name.'  + sLineBreak+ sLineBreak;

  str := str +  'The database selected is dumped to the above file.' + sLineBreak;
  str := str +  'The file includes CREATE DATABASE IF NOT EXISTS code' + sLineBreak;
  str := str +  'allowing the database to be created on this or another server. '+ sLineBreak + sLineBreak;

  str := str +  'Note: Display all databases includes default databases'   + sLineBreak;
  str := str +  'such as mysql and phpmyadmin these are normally hidden.'  + sLineBreak;
  str := str +  'Default is to display only user created databases.'       + sLineBreak+ sLineBreak;

  us_MessageDlg('Create a '+US_MYMAR_TXT+' database backup - Help ', str, mtInformation,[mbOk],0) ; //Display message
end;

procedure Tmysql_database_backup.Btn_create_backupClick(Sender: TObject);
var
  create_backup:boolean;  // Flag
  str:string;             // Temp string
  db_name      :string;   // Get database name to backup;
  db_file_name :string;   // Full path of backup file
begin
  create_backup := True;    // Assume backup can be created

  //Validate
  If (mysql_database_backup.ListBox1.ItemIndex = -1) Then
   begin
    create_backup := False;   //Can not create database backup
    str :='';
    str := str +  'No database selected.' + sLineBreak+ sLineBreak;

    str := str +  'Please select a database from the list and'  + sLineBreak;
    str := str +  'optionally change the database backup file name.' + sLineBreak;

    us_MessageDlg('Error', str, mtWarning,[mbOk],0) ; // Invalid or no file name.
   end

  Else
   begin
    If not us_valid_mysql_backup_file_name(Ed_db_name.Text) Then  // Validate input
     begin
      create_backup := False;   //Can not create database backup
      str :='';
      str := str +  'Invalid or no database backup file name.' + sLineBreak+ sLineBreak;

      str := str +  'Either select a database and use default name'  + sLineBreak;
      str := str +  'or ensure file name has the following format:' + sLineBreak;
      str := str +  'database_name.sql for example fred.sql' + sLineBreak;
      us_MessageDlg('Error', str, mtWarning,[mbOk],0) ; // Invalid or no file name.
    end;
  end;
  // End Validate

  //Create backup
  If create_backup Then
   begin
     db_name      := ListBox1.Items[ListBox1.ItemIndex];           // Get selected db name to backup
     db_file_name := US_DB_BACKUP_RESTORE + '\' + Ed_db_name.Text; // Full path of backup file
     run_mysql_db_backup(db_name,db_file_name);                    // Create backup
     mysql_database_backup_init; //  Clear
     showmessage('Backup complete');
   end;
end;

procedure Tmysql_database_backup.ChB_display_allClick(Sender: TObject);
begin
  display_databases; // Display all databases
end;

procedure Tmysql_database_backup.FormCreate(Sender: TObject);

begin
 mysql_database_backup.ChB_display_all.State:=cbUnChecked; // Hide databases

 //User may have deleted backup folder
 If Not(US_DB_BACKUP_RESTORE='') Then                // There is a value for file
  If DirectoryExists(US_MYSQL) Then                  // Check MySQl Module installed
   If Not DirectoryExists(US_DB_BACKUP_RESTORE) Then // Folder does not exist
    ForceDirectories(US_DB_BACKUP_RESTORE);          // we create it
end;

procedure Tmysql_database_backup.ListBox1Click(Sender: TObject);
begin
     Ed_db_name.Text := ListBox1.Items[ListBox1.ItemIndex]+'.sql'; // Get name selected
end;

{--- End display_databases --------------------------}


end.

