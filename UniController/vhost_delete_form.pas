unit vhost_delete_form;

{#############################################################################
'# Name: vhost_delete_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#
'#
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  default_config_vars,
  us_common_procedures,
  us_common_functions,
  RegExpr;

type

  { Tvhost_delete }

  Tvhost_delete = class(TForm)
    Btn_delete_vhost: TButton;
    Btn_cancel_selection: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    procedure Btn_cancel_selectionClick(Sender: TObject);
    procedure Btn_delete_vhostClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  vhost_delete: Tvhost_delete;


implementation

{$R *.lfm}

{===============================================================
Set Initial state:
 Read Vhost file and display Server Names
----------------------------------------------------------------}
procedure set_initial_state;
var
  sList     : TStringList;  // String list
  i         : integer;      // Loop counter
  RegexObj  : TRegExpr;     // Object
  is_first  : boolean;      // First Server name is default
begin
  is_first  := True;
  vhost_delete.ListBox1.ItemIndex  := -1;  // Clear selection
  vhost_delete.ListBox1.Clear;             // Clear list box

  //Display Vhosts in Vhost configuration file
  If FileExists(USF_APACHE_VHOST_CNF) Then        // Check file exists
      begin
        sList  := TStringList.Create;             // Create object
        RegexObj := TRegExpr.Create;              // Create regex obj
        sList.LoadFromFile(USF_APACHE_VHOST_CNF); // Load Apache main config file

        for i:=0 to sList.Count-1 do
         begin
           //-- Get Server name
          RegexObj.Expression := '^\s*ServerName\s*([^\s]*)';      // Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
             begin
               If is_first Then
                  is_first := false //Do not display first
               Else
                  vhost_delete.ListBox1.Items.Add(RegexObj.Match[1]); // Add item
             end;
          end;

        //Clean up
        RegexObj.Free;   // release object
        sList.Free;      // Remove from memory
      end;//End File exists
end;

{--------------------------------------------------------------}

{===============================================================
Delete Vhost block from Vhost configuration file:
Input In_ServerName

Deletes Vhost block.
Remove duplicated blank lines

----------------------------------------------------------------}
procedure delete_vhost_block(In_ServerName:String;Var Out_root_folder:string);
var
  start_index         : integer;     // Start of Vhost block
  sList               : TStringList; // String list
  i                   : integer;     // Loop counter

begin
  //Delete Vhost block from Vhost configuration file
  If FileExists(USF_APACHE_VHOST_CNF) Then        // Check file exists
      begin
        sList  := TStringList.Create;             // Create object
        sList.LoadFromFile(USF_APACHE_VHOST_CNF); // Load Apache main config file

        //Scan list and delete Vhost block
        for i:=0 to sList.Count-1 do
          begin
            //-- Get Start of Vhost
            If (sList[i]<>'') and ExecRegExpr('^\s*<VirtualHost\s', sList[i]) Then
               start_index := i; //Save start of block index

            //-- Get Server name
            If (sList[i]<>'') and ExecRegExpr('^\s*ServerName\s*'+In_ServerName, sList[i]) Then
              begin
                 Out_root_folder := sList[i-1];   // Save root folder
                 sList.Delete(start_index);       // Delete entery

                 //While not end of block delete current entry
                 While Not ExecRegExpr('^\s*</VirtualHost\s*', sList[start_index]) Do
                      sList.Delete(start_index); //Delete entery

                 //End of block detected delete this
                 sList.Delete(start_index); //Delete end of block
                 break;                     //Noting else to do
              end;
         end;//End scan list

         //==Clean file list

         //Remove all blank lines
         for i:=sList.Count-1 downto 0 do
            If sList[i]='' Then sList.Delete(i); //Delete entery

         //Insert blank line above start of each Vhost block
         for i:=sList.Count-1 downto 0 do
           begin
             //-- Get Start of Vhost
             If (sList[i]<>'') and ExecRegExpr('^\s*<VirtualHost\s', sList[i]) Then
                sList.Insert(i,''); // Insert blank line
           end;

         //Save updated file
         If FileIsWritable(USF_APACHE_VHOST_CNF) Then
            sList.SaveToFile(USF_APACHE_VHOST_CNF);     // Save new values to file

        //Clean up
        sList.Free;      // Remove from memory

        //===Root path returned if it contains an environment variable
        //---requires expanding. Back slashes convertion also required.

        Out_root_folder := StringReplace(Out_root_folder, '${US_ROOTF}',UniConPath_F,[rfReplaceAll]); // Expand ${US_ROOTF} if present.
        Out_root_folder := StringReplace(Out_root_folder, '/','\',[rfReplaceAll]);                    // Replace forward with back slash
        Out_root_folder := StringReplace(Out_root_folder, ' ','',[rfReplaceAll]);                     // Remove spaces
        Out_root_folder := StringReplace(Out_root_folder, 'DocumentRoot','',[rfReplaceAll]);          // Remove DocumentRoot


      end;//End file exists
end;
{--------------------------------------------------------------}


{ Tvhost_delete }

procedure Tvhost_delete.Btn_delete_vhostClick(Sender: TObject);
var
  selected_host   :string;
  Out_root_folder :String;
  str             :string;
begin
  Out_root_folder :='';

  //Check for selected item and get value
  If  ListBox1.ItemIndex >=0 Then // Item selected
    begin
      selected_host := ListBox1.Items[ListBox1.ItemIndex]; // Get name selected
      delete_vhost_block(selected_host,Out_root_folder);   // Delete selected item from config file
      us_delete_from_pac_file(selected_host);              // Delete selected host from PAC file
      us_delete_from_hosts_file(selected_host);            // Delete selected host from hosts file

      set_initial_state; //Get and display host entries

      //=== Ask user to confirm root folder deletion

      str := '';
      str := str +  'The Vhost was deleted.'+ sLineBreak+ sLineBreak;

      str := str +  'Note: The Vhost root folder was not deleted!' + sLineBreak + sLineBreak;

      str := str +  'Would you like to delete the root folder and all its content:' + sLineBreak;
      str := str +  Out_root_folder + sLineBreak + sLineBreak;

      str := str +  'To delete root folder Click Yes ' ;

      if us_MessageDlg('VHost Deleted', str, mtConfirmation,[mbYes, mbNo],0) = mrYes then
         DeleteDirectoryEx(Out_root_folder);   // Delete root folder

      us_MessageDlg('Apache Info','VHost root folder was deleted', mtcustom,[mbOk],0) ; //Display information message
      vhost_delete.Close;                      // Close form

     end
  Else // No item selected
      us_MessageDlg('Warning','No selection!', mtWarning,[mbOk],0) ; //Display warning message
end;

procedure Tvhost_delete.Btn_cancel_selectionClick(Sender: TObject);
begin
    vhost_delete.ListBox1.ItemIndex  := -1;  // Clear selection
end;


procedure Tvhost_delete.FormCreate(Sender: TObject);
begin
  set_initial_state; // Get and display host entries
end;

procedure Tvhost_delete.FormShow(Sender: TObject);
begin
  set_initial_state; // Get and display host entries
end;

end.

