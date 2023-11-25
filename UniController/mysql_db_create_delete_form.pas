unit mysql_db_create_delete_form;

{#############################################################################
'# Name: mysql_db_create_delete_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#
'# Creates or deletes a database uses MySQL utility programs mysql.exe
'# and mysqladmin.exe avoiding the need to have a specific MySQL libary.
'# List of databases displayed are always current since they are read
'# directely from server.
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  us_common_procedures,
  us_common_functions,
  default_config_vars;

type

  { Tmysql_db_create_delete }

  Tmysql_db_create_delete = class(TForm)
    Create_btn: TButton;
    Delete_btn: TButton;
    Clear_btn: TButton;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    procedure Clear_btnClick(Sender: TObject);
    procedure Create_btnClick(Sender: TObject);
    procedure Delete_btnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  mysql_db_create_delete: Tmysql_db_create_delete;

implementation

{$R *.lfm}

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

  mysql_db_create_delete.ListBox1.Items.Clear; //Clear list box

    //Scan returned list and add to ListBox1
    //Exclude the databases listed. We dont want a user to delete these
    for i:=0 to sList.Count-1 do
      begin
       if     not (sList[i]= 'Database')
          and not (sList[i]= 'information_schema')
          and not (sList[i]= 'mysql')
          and not (sList[i]= 'performance_schema')
          and not (sList[i]= 'phpmyadmin') then
          begin
             //Add filtered dbs to listbox
             mysql_db_create_delete.ListBox1.Items.Add(sList[i]);
          end;
      end;
    sList.Free;                           // Remove from memory
end;
{----------------------------------------------------}


{ Tmysql_db_create_delete }


procedure Tmysql_db_create_delete.Clear_btnClick(Sender: TObject);
begin
  Edit1.Text := '';          // Clear input
  ListBox1.ItemIndex := -1;  // Clear selection
end;

procedure Tmysql_db_create_delete.Create_btnClick(Sender: TObject);
var
 newDatabase:string;
 db_create:boolean;       // Create new db if true
 sList: TStringList;      // String list
 i:integer;               // Loop counter
begin
 db_create:= True;        // Assume db can be created

  If not us_valid_mysql_db_name(Edit1.Text) Then  // Validate input
  begin
    db_create := False;   //Can not create a db
    us_MessageDlg('Error', 'Invalid or no database name.', mtWarning,[mbOk],0) ; // Please enter a database name
  end

  else
  begin
   newDatabase := Edit1.Text; // Get new db name
  //--Check database not already created. Note this includes hidden databases
  sList  := TStringList.Create;                    // Create object
  If us_mysql_batch('show databases;',sList) Then  // Run batch
    begin
      //Scan returned list
      for i:=0 to sList.Count-1 do
        begin
           if sList[i] = newDatabase then
            begin
               // Already exists inform user
               us_MessageDlg('Error', 'Database already created!', mtWarning,[mbOk],0) ; //Database already created!
               db_create := False; // DB already exists
               Break;              // Exit nothing else to do
            end;
        end;
     end;
     sList.Free;                  // Remove from memory
   end;

  //--Create new data base
  If  db_create Then
   begin
     us_create_mysql_database(newDatabase); // Create database
     display_databases;                     // Read databases from server and display
   end;

 end;

procedure Tmysql_db_create_delete.Delete_btnClick(Sender: TObject);
var
 dbName:String; //Selected database name to drop
begin
  if ListBox1.ItemIndex <> -1 Then
   begin
     //Drop selected database
     dbName:= ListBox1.Items[ListBox1.ItemIndex]; // Get name selected
     us_drop_mysql_database(dbName);              // Delete database
     display_databases;                           // Read databases from server and display
   end
  else
   begin
     // No selection made inform user
     us_MessageDlg('Error', 'No database selected!', mtWarning,[mbOk],0) ; //No database selected!
   end;
end;

procedure Tmysql_db_create_delete.FormShow(Sender: TObject);
begin
  mysql_db_create_delete.Caption := 'Create Delete '+US_MYMAR_TXT+' Database'; //Create Delete MySQL Database
  display_databases; //Display list of databases in ListBox1
end;


end.

