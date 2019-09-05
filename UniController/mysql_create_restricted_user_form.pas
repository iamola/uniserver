unit mysql_create_restricted_user_form;

{#############################################################################
'# Name: create_restricted_mysql_user_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#
'# 
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  us_common_functions,
  default_config_vars;

type

  { Tmysql_create_restricted_user }

  Tmysql_create_restricted_user = class(TForm)
    Btn_cancel: TButton;
    Btn_create_user: TButton;
    Btn_help: TButton;
    CB_select: TCheckBox;
    CB_insert: TCheckBox;
    CB_update: TCheckBox;
    CB_delete: TCheckBox;
    CB_create: TCheckBox;
    CB_drop: TCheckBox;
    CB_alter: TCheckBox;
    CB_index: TCheckBox;
    Ed_user_name: TEdit;
    Ed_password: TEdit;
    Ed_db_name: TEdit;
    GB_create: TGroupBox;
    GB_privileges: TGroupBox;
    GB_select_db: TGroupBox;
    Lbl_user_name: TLabel;
    Lbl_password: TLabel;
    Lbl_db_name: TLabel;
    ListBox1: TListBox;
    procedure Btn_cancelClick(Sender: TObject);
    procedure Btn_create_userClick(Sender: TObject);
    procedure Btn_helpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  mysql_create_restricted_user: Tmysql_create_restricted_user;

implementation

{$R *.lfm}

{====================================================
 Set initial conditions
----------------------------------------------------}
procedure mysql_create_restricted_user_init;
begin
  mysql_create_restricted_user.Ed_user_name.Text := '';   // Clear input
  mysql_create_restricted_user.Ed_password.Text  := '';
  mysql_create_restricted_user.Ed_db_name.Text   := '';

  mysql_create_restricted_user.CB_select.Checked:=true;
  mysql_create_restricted_user.CB_insert.Checked:=true;
  mysql_create_restricted_user.CB_update.Checked:=true;
  mysql_create_restricted_user.CB_delete.Checked:=true;
  mysql_create_restricted_user.CB_create.Checked:=true;
  mysql_create_restricted_user.CB_drop.Checked:=true;
  mysql_create_restricted_user.CB_alter.Checked:=true;
  mysql_create_restricted_user.CB_index.Checked:=true;

  mysql_create_restricted_user.ListBox1.ItemIndex := -1;  // Clear database selection
end;
{--- End mysql_create_restricted_user_init ----------}


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

  mysql_create_restricted_user.ListBox1.Items.Clear; //Clear list box

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
             mysql_create_restricted_user.ListBox1.Items.Add(sList[i]);
          end;
      end;
    sList.Free;                           // Remove from memory
end;
{--- End display_databases --------------------------}

{ Tmysql_create_restricted_user }

procedure Tmysql_create_restricted_user.FormShow(Sender: TObject);
begin
  mysql_create_restricted_user.Caption := 'Create Restricted '+US_MYMAR_TXT+' User'; //Create Restricted MySQL User
  mysql_create_restricted_user_init; // Set initial conditions
  display_databases;                 // Display list of databases in ListBox1
end;

procedure Tmysql_create_restricted_user.ListBox1Click(Sender: TObject);
begin
   Ed_db_name.Text := ListBox1.Items[ListBox1.ItemIndex]; // Get name selected
end;

procedure Tmysql_create_restricted_user.Btn_helpClick(Sender: TObject);
Var
  str:string;
begin
  //str_mysql_uc_help_title          = Restricted MySQL User - Help
  str :='';
  str := str +  'This menu item creates a '+US_MYMAR_TXT+' user with restricted privileges.' + sLineBreak+ sLineBreak;

  str := str +  'Enter a user name and password.' + sLineBreak;
  str := str +  'Select required user privileges.' + sLineBreak+ sLineBreak;

  str := str +  'This user is restricted to using a single database.' + sLineBreak;
  str := str +  'Enter a database name or select one from the option list.' + sLineBreak;
  str := str +  'If the database does not exit it is created.' + sLineBreak;

  us_MessageDlg('Restricted '+US_MYMAR_TXT+' User - Help ', str, mtInformation,[mbOk],0) ; //Display message

end;

procedure Tmysql_create_restricted_user.Btn_cancelClick(Sender: TObject);
begin
  mysql_create_restricted_user_init;
end;

procedure Tmysql_create_restricted_user.Btn_create_userClick(Sender: TObject);
var
  create_user:boolean;
  database_name:string;
  user_name:string;
  user_password :string;
  priv_selected:boolean;
  priv_str:string;
  priv_select:string;
  priv_insert:string;
  priv_update:string;
  priv_delete:string;
  priv_create:string;
  priv_drop:string;
  priv_alter:string;
  priv_index:string;
  mysql_str:string;
  dummy: TStringList;    // String list
begin
   create_user:= True;        // Assume user can be created

 If not us_valid_mysql_user_name(Ed_user_name.Text) Then  // Validate input
  begin
    create_user := False;   //Can not create a user
    us_MessageDlg('Error', 'Invalid or no user name', mtWarning,[mbOk],0) ; // Invalid or no user name
  end;

 If not us_valid_mysql_password(Ed_password.Text) Then   // Validate input
  begin
    create_user := False;   //Can not create a user
    us_MessageDlg('Error', 'Invalid or no user password', mtWarning,[mbOk],0) ; // Invalid or no user password
  end;

  If not us_valid_mysql_db_name(Ed_db_name.Text) Then  // Validate input
  begin
    create_user := False;   //Can not create a user
    us_MessageDlg('Error', 'Invalid or no db name. Please enter or select a database name', mtWarning,[mbOk],0) ; // Invalid or no db name. Please enter or select a database name
  end;

  //--Check privileges selected
  priv_selected := False;  //Assume not selected
  If CB_select.Checked then priv_selected:=true;
  If CB_insert.Checked then priv_selected:=true;
  If CB_update.Checked then priv_selected:=true;
  If CB_delete.Checked then priv_selected:=true;
  If CB_create.Checked then priv_selected:=true;
  If CB_drop.Checked   then priv_selected:=true;
  If CB_alter.Checked  then priv_selected:=true;
  If CB_index.Checked  then priv_selected:=true;

  If not priv_selected Then  // No selection
  begin
    create_user := False;   //Can not create a user
    us_MessageDlg('Error', 'No privileges selected.', mtWarning,[mbOk],0) ; // No privileges selected.
  end;


  //--Create new restricted user
  If  create_user Then
   begin

  database_name := Ed_db_name.Text;
  user_name     := Ed_user_name.Text;
  user_password := Ed_password.Text;

  If CB_select.Checked Then priv_select:='SELECT ' else priv_select:=''; //Note space after privilege required
  If CB_insert.Checked Then priv_insert:='INSERT ' else priv_insert:=''; //Note space
  If CB_update.Checked Then priv_update:='UPDATE ' else priv_update:=''; //Note space
  If CB_delete.Checked Then priv_delete:='DELETE ' else priv_delete:=''; //Note space
  If CB_create.Checked Then priv_create:='CREATE ' else priv_create:=''; //Note space
  If CB_drop.Checked   Then priv_drop  :='DROP '   else priv_drop  :=''; //Note space
  If CB_alter.Checked  Then priv_alter :='ALTER '  else priv_alter :=''; //Note space
  If CB_index.Checked  Then priv_index :='INDEX '  else priv_index :=''; //Note space

  priv_str := trim(priv_select + priv_insert + priv_update + priv_delete + priv_create + priv_drop + priv_alter  + priv_index); //Create string, trim left right spaces
  priv_str := StringReplace(priv_str,' ',', ',[rfReplaceAll]);


  //--- Build Query string
  mysql_str := '' ;
  mysql_str :=  mysql_str + 'GRANT ' + priv_str + ' ';
  mysql_str :=  mysql_str + 'ON ' + database_name +'.* TO ';
  mysql_str :=  mysql_str + user_name + '@127.0.0.1 ';
  mysql_str :=  mysql_str + 'IDENTIFIED BY ''' + user_password +''';';

  //showmessage(mysql_str);

  //--- Update MySQL server
  dummy  := TStringList.Create;    // Create object
  us_mysql_batch(mysql_str,dummy); // Use dummy stringlist (not used)
  dummy.Free;                      // Remove from memory

  //--Display restricted MySQL User Created
  us_MessageDlg('Created' , 'Restricted MySQL User Created', mtInformation,[mbOk],0) ;
  mysql_create_restricted_user_init;
   end;

end;

end.

