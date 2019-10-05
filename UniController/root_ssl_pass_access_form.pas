unit root_ssl_pass_access_form;

{#############################################################################
'# Name: root_ssl_pass_access_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  default_config_vars,
  us_common_functions;

type

  { Troot_ssl_pass_access }

  Troot_ssl_pass_access = class(TForm)
    Btn_add_to_list: TButton;
    Btn_delete_selected: TButton;
    Edit_name: TEdit;
    Edit_password: TEdit;
    GBox_enter_name_password: TGroupBox;
    GBox_enable_disable_password: TGroupBox;
    GBox_access: TGroupBox;
    Label_name: TLabel;
    Label_password: TLabel;
    ListBox1: TListBox;
    RBtn_password_disabled: TRadioButton;
    RBtn_password_enabled: TRadioButton;
    RBtn_local: TRadioButton;
    RBtn_local_intranet: TRadioButton;
    RBtn_local_intranet_internet: TRadioButton;
    procedure Btn_add_to_listClick(Sender: TObject);
    procedure Btn_delete_selectedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RBtn_localClick(Sender: TObject);
    procedure RBtn_local_intranetClick(Sender: TObject);
    procedure RBtn_local_intranet_internetClick(Sender: TObject);
    procedure RBtn_password_disabledClick(Sender: TObject);
    procedure RBtn_password_enabledClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  root_ssl_pass_access: Troot_ssl_pass_access;

implementation

{$R *.lfm}


{====================================================================
ssl Access Initiliasion:
 Sets button and caption text.
 Display name-password pairs set in config file.
 Display password and access state set in config file.

A radio button defines which section is executed.
 These radio buttons initially have their OnClick event disabled.
 This prevents generating an event on setting initial button state.
 After setting initial state OnClick events are enabled.
=====================================================================}
procedure ssl_access_init;
 var
  sList: TStringList;   // String list
  i:integer;
begin
  //===Set initial button state from configuration file settings

   //--- Display name-password pairs containd in config file
  if FileExists(USF_SSL_PASSWORD) Then
   begin
      root_ssl_pass_access.ListBox1.Items.Clear;    // Clear list
      sList := TStringList.Create;                  // Create object
      sList.LoadFromFile(USF_SSL_PASSWORD);         // Load file
      root_ssl_pass_access.ListBox1.Items := sList; // Add list to
      sList.Free;                                   // Remove from memory
   end;

   //--- Display password and access state from config file ssl\.htaccess
   if FileExists(USF_SSL_HTACCESS) Then
    begin
       sList := TStringList.Create;             // Create object
       sList.LoadFromFile(USF_SSL_HTACCESS);    // Load file

       //Disable radio button OnClick events
       root_ssl_pass_access.RBtn_password_disabled.OnClick       := nil; // Disable event
       root_ssl_pass_access.RBtn_password_enabled.OnClick        := nil;
       root_ssl_pass_access.RBtn_local.OnClick                   := nil;
       root_ssl_pass_access.RBtn_local_intranet.OnClick          := nil;
       root_ssl_pass_access.RBtn_local_intranet_internet.OnClick := nil;

       //Set button initial state
       root_ssl_pass_access.RBtn_password_disabled.Checked       := False;
       root_ssl_pass_access.RBtn_password_enabled.Checked        := False;
       root_ssl_pass_access.RBtn_local.Checked                   := False;
       root_ssl_pass_access.RBtn_local_intranet.Checked          := False;
       root_ssl_pass_access.RBtn_local_intranet_internet.Checked := False;

       //Scan list
       for i:=0 to sList.Count-1 do
        begin
         // Display password enabled state
         If Pos('#AuthName',sList[i]) = 1 then root_ssl_pass_access.RBtn_password_disabled.Checked := True;
         If Pos('AuthName',sList[i])  = 1 then root_ssl_pass_access.RBtn_password_enabled.Checked  := True;

         // Display access state
         If Pos('Allow from 127.0.0.1',sList[i])          = 1 then root_ssl_pass_access.RBtn_local.Checked := True;                   //Local access
         If Pos('Allow from 127.0.0.1 192.168',sList[i])  = 1 then root_ssl_pass_access.RBtn_local_intranet.Checked := True;          //local and Intranet access
         If Pos('#Allow from 127.0.0.1',sList[i])         = 1 then root_ssl_pass_access.RBtn_local_intranet_internet.Checked := True; //Internet access enabled
        end;

       //Enable radio button OnClick events
       root_ssl_pass_access.RBtn_password_disabled.OnClick       := @root_ssl_pass_access.RBtn_password_disabledClick;
       root_ssl_pass_access.RBtn_password_enabled.OnClick        := @root_ssl_pass_access.RBtn_password_enabledClick;
       root_ssl_pass_access.RBtn_local.OnClick                   := @root_ssl_pass_access.RBtn_localClick;
       root_ssl_pass_access.RBtn_local_intranet.OnClick          := @root_ssl_pass_access.RBtn_local_intranetClick;
       root_ssl_pass_access.RBtn_local_intranet_internet.OnClick := @root_ssl_pass_access.RBtn_local_intranet_internetClick;

      sList.Free; // Remove from memory
    end;

end;
{---End ssl_access_init --------------------------------------------}

{====================================================================
This procedure enables or  disables password access section in
configuration file ssl\.htaccess

A radio button defines which section is executed.
  Password Disabled
  Password Enabled
=====================================================================}
procedure ssl_enable_disable_password_section;
var
    sList: TStringList;   // String list
    i:integer;            // Loop counter
begin
   // Load configuration file .htaccess
   if FileExists(USF_SSL_HTACCESS) Then
    begin
       sList := TStringList.Create;             // Create object
       sList.LoadFromFile(USF_SSL_HTACCESS);    // Load file

       // Disable block
       If root_ssl_pass_access.RBtn_password_disabled.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
             If Pos('AuthName',sList[i]) = 1 then
               sList[i] := '#AuthName "Uniform Server - Server Access"';

             If Pos('AuthType Basic',sList[i]) = 1 then
               sList[i] := '#AuthType Basic';

             If Pos('AuthUserFile',sList[i]) = 1 then
               sList[i] := '#AuthUserFile ../../htpasswd/ssl/.htpasswd';

             If Pos('Require',sList[i]) = 1 then
               sList[i] := '#Require valid-user';
            end;
        end;
       // Enable block
       If root_ssl_pass_access.RBtn_password_enabled.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
             If Pos('#AuthName',sList[i]) = 1 then
               sList[i] := 'AuthName "Uniform Server - Server Access"';

             If Pos('#AuthType Basic',sList[i]) = 1 then
               sList[i] := 'AuthType Basic';

             If Pos('#AuthUserFile',sList[i]) = 1 then
               sList[i] := 'AuthUserFile ../../htpasswd/ssl/.htpasswd';

             If Pos('#Require',sList[i]) = 1 then
               sList[i] := 'Require valid-user';
            end;
        end;

       If FileIsWritable(USF_SSL_HTACCESS) Then
          sList.SaveToFile(USF_SSL_HTACCESS); // Save new values to file

      sleep(100);
      sList.Free;                         // Remove from memory
    end;
end;
{---------------------------------------------------------------------}

{====================================================================
This procedure sets who can gain access to the servers using
configuration file ssl\.htaccess

A radio button defines which section is executed.
  Local
  Local and Intranet Access
  Local, Intranet and Internet Access
=====================================================================}
procedure ssl_set_access_section;
var
    sList: TStringList;   // String list
    i:integer;            // Loop counter
begin
   // Load configuration file .htaccess
   if FileExists(USF_SSL_HTACCESS) Then
    begin
       sList := TStringList.Create;             // Create object
       sList.LoadFromFile(USF_SSL_HTACCESS);    // Load file

       // Enable Local Access
       If root_ssl_pass_access.RBtn_local.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
               If Pos('Order Deny,Allow',sList[i]) <> 0 then
                 sList[i] := 'Order Deny,Allow';

               If Pos('Deny from all',sList[i]) <> 0 then
                 sList[i] := 'Deny from all';

               If Pos('Allow from 127.0.0.1',sList[i]) <> 0 then
                 sList[i] := 'Allow from 127.0.0.1';

               If Pos('Allow from ::1',sList[i]) <> 0 then
                 sList[i] := 'Allow from ::1';
            end;
        end;

       // Local and Intranet Access
       If root_ssl_pass_access.RBtn_local_intranet.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
               If Pos('Order Deny,Allow',sList[i]) <> 0 then
                 sList[i] := 'Order Deny,Allow';

               If Pos('Deny from all',sList[i]) <> 0 then
                 sList[i] := 'Deny from all';

               If Pos('Allow from 127.0.0.1',sList[i]) <> 0 then
                 sList[i] := 'Allow from 127.0.0.1 192.168.0.0/16 172.16.0.0/12 10.0.0.0/8';

               If Pos('Allow from ::1',sList[i]) <> 0 then
                 sList[i] := 'Allow from ::1';
            end;
        end;

       // Local, Intranet and Internet Access
       If root_ssl_pass_access.RBtn_local_intranet_internet.Checked Then
        begin
           //Scan list
           for i:=0 to sList.Count-1 do
            begin
               If Pos('Order Deny,Allow',sList[i]) <> 0 then
                 sList[i] := '#Order Deny,Allow';

               If Pos('Deny from all',sList[i]) <> 0 then
                 sList[i] := '#Deny from all';

               If Pos('Allow from 127.0.0.1',sList[i]) <> 0 then
                 sList[i] := '#Allow from 127.0.0.1';

               If Pos('Allow from ::1',sList[i]) <> 0 then
                 sList[i] := '#Allow from ::1';
            end;
        end;

       If FileIsWritable(USF_SSL_HTACCESS) Then
         sList.SaveToFile(USF_SSL_HTACCESS); // Save new values to file

       sleep(100);
       sList.Free;                         // Remove from memory
     end;
    end;
{---------------------------------------------------------------------}

{ Troot_ssl_pass_access }

procedure Troot_ssl_pass_access.FormShow(Sender: TObject);
begin
    //===Ensure htaccess file exists. Option for user to crate.
  if us_ssl_htaccess_check_create Then  // Check for htaccess file. Allow user to crate.
   begin
     //Create back up
     If Not FileExists(USF_SSL_HTACCESS_BACK) Then
       CopyFile(USF_SSL_HTACCESS,USF_SSL_HTACCESS_BACK);  // Create backup file

       ssl_access_init; // Set initial or reload and displlay current status
   end
  Else    //File does not exits. User skipped option to create
   begin
     root_ssl_pass_access.Close;      // Nothing else to do
   end;
end;

procedure Troot_ssl_pass_access.RBtn_localClick(Sender: TObject);
begin
     ssl_set_access_section; // Set server access block
end;

procedure Troot_ssl_pass_access.RBtn_local_intranetClick(Sender: TObject);
begin
     ssl_set_access_section; // Set server access block
end;

procedure Troot_ssl_pass_access.RBtn_local_intranet_internetClick(
  Sender: TObject);
begin
     ssl_set_access_section; // Set server access block
end;

procedure Troot_ssl_pass_access.RBtn_password_disabledClick(Sender: TObject);
begin
     ssl_enable_disable_password_section; // Disable password block
end;

procedure Troot_ssl_pass_access.RBtn_password_enabledClick(Sender: TObject);
begin
  If Not password_file_contains_defaults(USF_SSL_PASSWORD) Then
   begin
      If Not password_file_empty(USF_SSL_PASSWORD) Then
         ssl_enable_disable_password_section; // Enable password block
   end;
end;

procedure Troot_ssl_pass_access.Btn_delete_selectedClick(Sender: TObject);
{Delete selected Name Password entry update list and file}
var
   i:integer;
   sList: TStringList;       // String list
begin
  // Delete selected entry
  For i := ListBox1.Items.Count - 1 downto 0 do
   if ListBox1.Selected [i] then
    ListBox1.Items.Delete (i);

  // Update file
  sList := TStringList.Create;            // Create object
  sList.Assign(ListBox1.Items) ;          // Add items to string list


  If FileIsWritable(USF_SSL_PASSWORD) Then
    begin
     sList.SaveToFile(USF_SSL_PASSWORD);     // Save new values to file
    end;
  sleep(100);

  sList.Free;                             // Remove from memory
end;

procedure Troot_ssl_pass_access.Btn_add_to_listClick(Sender: TObject);
{Add entered name and password to list and save list to file}
 Var
   new_name_password:string;
   sList: TStringList;       // String list
begin
     If (Edit_name.Text = '') or  (Edit_password.Text ='') Then
    begin
     // Display warning mesage Both Name and Password required
      us_MessageDlg('Error','Both Name and Password required', mtWarning,[mbOk],0) ;
    end
   else
   begin
   new_name_password := Edit_name.Text + ':' + Edit_password.Text; // Get new name-password pair
   ListBox1.Items.Add(new_name_password);                          // Add pair to listbox

   sList := TStringList.Create;            // Create object
   sList.Assign(ListBox1.Items) ;          // Add items to string list

   If FileIsWritable(USF_SSL_PASSWORD) Then
     sList.SaveToFile(USF_SSL_PASSWORD);     // Save new values to file

  sleep(100);
  sList.Free;                             // Remove from memory

  Edit_name.Text      :=''; // Clear entery
  Edit_password.Text  :=''; // Clear entery
 end;
end;

end.

