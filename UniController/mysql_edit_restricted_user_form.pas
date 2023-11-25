unit mysql_edit_restricted_user_form;

{#############################################################################
'# Name: edit_restricted_mysql_user_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  us_common_procedures,
  us_common_functions,
  default_config_vars;

type

  { Tmysql_edit_restricted_user }

  Tmysql_edit_restricted_user = class(TForm)
    Btn_cancel: TButton;
    Btn_update_user: TButton;
    Btn_help: TButton;
    Btn_delete: TButton;
    Btn_cancel_2: TButton;
    CB_alter: TCheckBox;
    CB_create: TCheckBox;
    CB_delete: TCheckBox;
    CB_drop: TCheckBox;
    CB_index: TCheckBox;
    CB_insert: TCheckBox;
    CB_select: TCheckBox;
    CB_update: TCheckBox;
    Ed_db_name: TEdit;
    Ed_password: TEdit;
    Ed_user_name: TEdit;
    GB_edit: TGroupBox;
    GB_privileges: TGroupBox;
    GB_select_a_user: TGroupBox;
    Lbl_db_name: TLabel;
    Lbl_password: TLabel;
    Lbl_user_name: TLabel;
    ListBox1: TListBox;
    procedure Btn_cancelClick(Sender: TObject);
    procedure Btn_cancel_2Click(Sender: TObject);
    procedure Btn_deleteClick(Sender: TObject);
    procedure Btn_helpClick(Sender: TObject);
    procedure Btn_update_userClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  mysql_edit_restricted_user: Tmysql_edit_restricted_user;

implementation

{$R *.lfm}

{====================================================
 Set initial conditions
----------------------------------------------------}
procedure mysql_edit_restricted_user_init;
begin
  mysql_edit_restricted_user.Ed_user_name.Text := '';   // Clear input
  mysql_edit_restricted_user.Ed_password.Text  := '';
  mysql_edit_restricted_user.Ed_db_name.Text   := '';

  mysql_edit_restricted_user.CB_select.Checked:=true;
  mysql_edit_restricted_user.CB_insert.Checked:=true;
  mysql_edit_restricted_user.CB_update.Checked:=true;
  mysql_edit_restricted_user.CB_delete.Checked:=true;
  mysql_edit_restricted_user.CB_create.Checked:=true;
  mysql_edit_restricted_user.CB_drop.Checked:=true;
  mysql_edit_restricted_user.CB_alter.Checked:=true;
  mysql_edit_restricted_user.CB_index.Checked:=true;

  mysql_edit_restricted_user.ListBox1.ItemIndex := -1;    // Clear database selection
  mysql_edit_restricted_user.Btn_delete.Enabled := False; // Disable button

end;
{--- End mysql_create_restricted_user_init ---------}

{====================================================
 Display list of users in ListBox1
----------------------------------------------------}
procedure display_users;
var
 sList: TStringList;    // String list
 i:integer;             // Loop counter
begin
  mysql_edit_restricted_user.Btn_delete.Enabled := False; // Disable button

  sList  := TStringList.Create;             // Create object
  us_mysql_batch('select user from mysql.user;',sList);  // Run batch

  mysql_edit_restricted_user.ListBox1.Items.Clear; //Clear list box

    //Scan returned list and add to ListBox1
    //Exclude MySQL users listed. We dont want a user to delete these
    for i:=0 to sList.Count-1 do
      begin
       if     not (sList[i]= 'user')
          and not (sList[i]= 'pma')
          and not (sList[i]= 'root') then
          begin
             //Add filtered users to listbox
             mysql_edit_restricted_user.ListBox1.Items.Add(sList[i]);
          end;
      end;
    sList.Free;                           // Remove from memory
end;

{ Tmysql_edit_restricted_user }

procedure Tmysql_edit_restricted_user.FormShow(Sender: TObject);
begin
  mysql_edit_restricted_user.Caption := 'Edit Restricted '+US_MYMAR_TXT+' User'; // Edit Restricted MySQL User
  mysql_edit_restricted_user_init;
  display_users; //Display list of users in ListBox1
end;

procedure Tmysql_edit_restricted_user.ListBox1Click(Sender: TObject);
var
  user_name:string;    // Get user name
  mysql_str:string;
  grant_str:string;    // Last line contains grant information
  sList: TStringList;  // String list

begin
  user_name         := ListBox1.Items[ListBox1.ItemIndex];  // Get name selected
  Ed_user_name.Text := user_name;                           // Display name in edit box
  mysql_edit_restricted_user.Btn_delete.Enabled := True;    // Enable button

  //--Get user grant information
  mysql_str := 'SHOW GRANTS FOR ''' + user_name + '''@''localhost'';';  //Sql to list grants for user

  sList  := TStringList.Create;       // Create object
  us_mysql_batch(mysql_str,sList);    // Run batch
  grant_str := sList[sList.Count-1];  // Last line contains information required.
  sList.Free;                         // Remove from memory

  //--Set privileges check boxes
  If Pos('SELECT',grant_str) <> 0 then CB_select.Checked := True else CB_select.Checked := False;
  If Pos('INSERT',grant_str) <> 0 then CB_insert.Checked := True else CB_insert.Checked := False;
  If Pos('UPDATE',grant_str) <> 0 then CB_update.Checked := True else CB_update.Checked := False;
  If Pos('DELETE',grant_str) <> 0 then CB_delete.Checked := True else CB_delete.Checked := False;
  If Pos('CREATE',grant_str) <> 0 then CB_create.Checked := True else CB_create.Checked := False;
  If Pos('DROP',  grant_str) <> 0 then CB_drop.Checked   := True else CB_drop.Checked   := False;
  If Pos('ALTER', grant_str) <> 0 then CB_alter.Checked  := True else CB_alter.Checked  := False;
  If Pos('INDEX', grant_str) <> 0 then CB_index.Checked  := True else CB_index.Checked  := False;

 //showmessage(grant_str);
 //Get database name corresponding to user
   sList  := TStringList.Create;               // Create object
   // Convert string to a string list
   while (Length(grant_str) > 0) do            // Scan line
     begin
       sList.Add(us_get_word(grant_str, '`')); // Split this at "`"
     end;
   Ed_db_name.Text := sList[1];                // Second entry in list
   sList.Free;                                 // Remove from memory

end;

procedure Tmysql_edit_restricted_user.Btn_helpClick(Sender: TObject);
var
  str:string;
begin
    //str_mysql_ue_help_title         = Edit Restricted MySQL User - Help
  str :='';

  str := str + 'From this menu item you can either delete'   + sLineBreak;
  str := str + 'or update a restricted '+US_MYMAR_TXT+' user.' + sLineBreak+ sLineBreak;

  str := str + 'Delete:' + sLineBreak;
  str := str + 'Select user from list and click delete button.' + sLineBreak+ sLineBreak;

  str := str + 'Update:' + sLineBreak;
  str := str + 'Select user from list. Change parameters as required.' + sLineBreak;
  str := str + 'Click Update User button.' + sLineBreak;


  us_MessageDlg('Edit Restricted '+US_MYMAR_TXT+' User - Help', str, mtInformation,[mbOk],0) ; //Display message
end;

procedure Tmysql_edit_restricted_user.Btn_cancelClick(Sender: TObject);
begin
  mysql_edit_restricted_user_init;
end;

procedure Tmysql_edit_restricted_user.Btn_cancel_2Click(Sender: TObject);
begin
  mysql_edit_restricted_user_init;
end;

procedure Tmysql_edit_restricted_user.Btn_deleteClick(Sender: TObject);
var
 sList: TStringList;    // String list
 mysql_str:string;
 user_name:string;
begin
 //--- Delete user
 user_name     := Ed_user_name.Text;
 sList  := TStringList.Create;                                   // Create object
 mysql_str := 'DROP USER ''' + user_name + '''@''localhost'';';  //Sql to delete user
 us_mysql_batch(mysql_str,sList);                                //Delete user
 sList.Free;

 //--Clear inputs
 Ed_user_name.Text := '';   // Clear input
 Ed_password.Text  := '';
 Ed_db_name.Text   := '';
 ListBox1.ItemIndex := -1;  // Clear selection

 //--Clear privileges selected
 CB_select.Checked :=False;
 CB_insert.Checked :=False;
 CB_update.Checked :=False;
 CB_delete.Checked :=False;
 CB_create.Checked :=False;
 CB_drop.Checked   :=False;
 CB_alter.Checked  :=False;
 CB_index.Checked  :=False;

 mysql_edit_restricted_user.Btn_delete.Enabled := False; // Disable button

 display_users;    //re-populate with new user list

end;

{====================================================================
Update users:
 First delete user and then recreate with new data
 Create user and grant privileges to a database
 General format (Note sequence of commas!)
 query = "GRANT SELECT, INSERT, UPDATE, DELETE, ALTER, INDEX ON dbname.* TO username@localhost IDENTIFIED BY 'password'";
---------------------------------------------------------------------}
procedure Tmysql_edit_restricted_user.Btn_update_userClick(Sender: TObject);
var
  update_user:boolean;
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
  sList: TStringList;    // String list
  db_exists:boolean;
  i:integer;
begin
     update_user:= True;     // Assume user can be update

 If not us_valid_mysql_user_name(Ed_user_name.Text) Then  // Validate input
  begin
    update_user := False;   //Can not update user
    us_MessageDlg('Error', 'Invalid or no user name', mtWarning,[mbOk],0) ; // Invalid or no user name
  end;

 If not us_valid_mysql_password(Ed_password.Text) Then   // Validate input
  begin
    update_user := False;   //Can not update user
    us_MessageDlg('Error', 'Invalid or no user password', mtWarning,[mbOk],0) ; // Invalid or no user password
  end;

  If not us_valid_mysql_db_name(Ed_db_name.Text) Then  // Validate input
  begin
    update_user := False;   //Can not create a user
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
    update_user := False;   //Can not create a user
    us_MessageDlg('Error', 'No privileges selected.', mtWarning,[mbOk],0) ; // No privileges selected.
  end;


  //==== Update restricted user
  If  update_user Then
   begin
    database_name := Ed_db_name.Text;
    user_name     := Ed_user_name.Text;
    user_password := Ed_password.Text;

    If CB_select.Checked Then priv_select:='SELECT ' else priv_select:=''; //Note space
    If CB_insert.Checked Then priv_insert:='INSERT ' else priv_insert:=''; //Note space
    If CB_update.Checked Then priv_update:='UPDATE ' else priv_update:=''; //Note space
    If CB_delete.Checked Then priv_delete:='DELETE ' else priv_delete:=''; //Note space
    If CB_create.Checked Then priv_create:='CREATE ' else priv_create:=''; //Note space
    If CB_drop.Checked   Then priv_drop  :='DROP '   else priv_drop  :=''; //Note space
    If CB_alter.Checked  Then priv_alter :='ALTER '  else priv_alter :=''; //Note space
    If CB_index.Checked  Then priv_index :='INDEX '  else priv_index :=''; //Note space

    priv_str := trim(priv_select + priv_insert + priv_update + priv_delete + priv_create + priv_drop + priv_alter  + priv_index); //Create string, trim left right spaces
    priv_str := StringReplace(priv_str,' ',', ',[rfReplaceAll]);

    //--- Check database exists, if not create it.
    sList     := TStringList.Create;                // Create object
    db_exists := false;                             // Reset flag
    If us_mysql_batch('show databases;',sList) Then // Database list returned
     for i:=0 to sList.Count-1 do                   // Scan list
       begin
         if sList[i]= database_name Then
          begin
            db_exists := true;                   // DB found
          end;
       end;

     If not db_exists then                       // DB not found
      begin
        us_create_mysql_database(database_name); // Create new database
      end;
    sList.Free;                                  // Remove from memory

    //--- Delete user
    sList  := TStringList.Create;                                  // Create object
    mysql_str := 'DROP USER ''' + user_name + '''@''localhost'';'; // Sql to delete user
    us_mysql_batch(mysql_str,sList);                               // Delete user
    sList.Free;                                                    // Remove from memory

    //--- Build Query string
  mysql_str := '' ;
  mysql_str :=  mysql_str + 'CREATE USER ' + user_name + '@localhost ';
  mysql_str :=  mysql_str + 'IDENTIFIED BY ''' + user_password +''';';
  mysql_str :=  mysql_str + 'GRANT ' + priv_str + ' ';
  mysql_str :=  mysql_str + 'ON ' + database_name +'.* TO ';
  mysql_str :=  mysql_str + user_name + '@localhost ;';

  //showmessage(mysql_str);

  //--- Update MySQL server
  sList  := TStringList.Create;    // Create object
  us_mysql_batch(mysql_str,sList); // Stringlist not used
  sList.Free;                      // Remove from memory

  //--Display restricted MySQL User Updated
  us_MessageDlg('Updated', 'Restricted '+US_MYMAR_TXT+' User Updated', mtInformation,[mbOk],0) ;
  mysql_edit_restricted_user_init; //Reset
   end;
end;

end.

