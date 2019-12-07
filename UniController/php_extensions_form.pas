unit php_extensions_form;

{#############################################################################
'# Name:  php_extensions_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'# V1.1.2 2-05-2014 Added PHP 56 selected section
'#                  Allow comments adjacent to extentions in php.ini configs.
'# This form lists all PHP extensions and shows extentions selected.
'# for the currently select PHP configuration file
'# A user can select or deselect extension. The currently selected PHP configuration
'# PHP configuration  is updated to reflect user selection.
'#############################################################################}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  StdCtrls, default_config_vars;

type

  { Tphp_extensions }

  Tphp_extensions = class(TForm)
    Lbl_verson: TLabel;
    Lbl_file: TLabel;
    PHP_Extensions_CheckListBox: TCheckListBox;
    procedure FormShow(Sender: TObject);
    procedure PHP_Extensions_CheckListBoxItemClick(Sender: TObject;
      Index: integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  php_extensions: Tphp_extensions;
  USF_PHP_INI_PATH  :String;    // Path to ini for selected PHP version

implementation

{$R *.lfm}

{ Tphp_extensions }

procedure Tphp_extensions.FormShow(Sender: TObject);
  var
   US_PHP_EXTEN      :String;        // Path to PHP extensions folder

   sList_in          : TStringList;  // Currently seleted php.ini string list
   sList_files       : TStringList;  // Extension folder file list
   i:integer;                        // Loop counter
   j:integer;                        // Loop counter
   str_version       :string;        // PHP version selected
   str_file_selected :string;        // PHP ini file selected

   begin
    PHP_Extensions_CheckListBox.ItemIndex := -1;  // Clear selection
    PHP_Extensions_CheckListBox.Items.Clear;      // Clear list box

    USF_PHP_INI_PATH  :='';    // Set Initial value
    US_PHP_EXTEN      :='';    //
    str_version       :='PHP Version: ';  // PHP version selected
    str_file_selected :='Config File: ';  // PHP ini file selected

    //==Set config file paths for selected PHP version

    //--PHP 56  selected
    If UENV_PHP_SELECT ='php56' Then
    begin
     str_version := str_version + 'php56';
     US_PHP_EXTEN  := US_PHP56_EXT;  // PHP extensions folder
     If UENV_PHP_INI_SELECT ='php_test.ini'        Then USF_PHP_INI_PATH := USF_PHP_INI_TEST_56; // config php_test.ini
     If UENV_PHP_INI_SELECT ='php_development.ini' Then USF_PHP_INI_PATH := USF_PHP_INI_DEV_56;  // config php_development.ini
     If UENV_PHP_INI_SELECT ='php_production.ini'  Then USF_PHP_INI_PATH := USF_PHP_INI_PROD_56; // php_production.ini
    end;
    //--End PHP 56  selected

    //--PHP 70  selected
    If UENV_PHP_SELECT ='php70' Then
    begin
     str_version := str_version + 'php70';
     US_PHP_EXTEN  := US_PHP70_EXT;  // PHP extensions folder
     If UENV_PHP_INI_SELECT ='php_test.ini'        Then USF_PHP_INI_PATH := USF_PHP_INI_TEST_70; // config php_test.ini
     If UENV_PHP_INI_SELECT ='php_development.ini' Then USF_PHP_INI_PATH := USF_PHP_INI_DEV_70;  // config php_development.ini
     If UENV_PHP_INI_SELECT ='php_production.ini'  Then USF_PHP_INI_PATH := USF_PHP_INI_PROD_70; // php_production.ini
    end;
    //--End PHP 70  selected

    //--PHP 71  selected
    If UENV_PHP_SELECT ='php71' Then
    begin
     str_version := str_version + 'php71';
     US_PHP_EXTEN  := US_PHP71_EXT;  // PHP extensions folder
     If UENV_PHP_INI_SELECT ='php_test.ini'        Then USF_PHP_INI_PATH := USF_PHP_INI_TEST_71; // config php_test.ini
     If UENV_PHP_INI_SELECT ='php_development.ini' Then USF_PHP_INI_PATH := USF_PHP_INI_DEV_71;  // config php_development.ini
     If UENV_PHP_INI_SELECT ='php_production.ini'  Then USF_PHP_INI_PATH := USF_PHP_INI_PROD_71; // php_production.ini
    end;
    //--End PHP 71  selected

    //--PHP 72  selected
    If UENV_PHP_SELECT ='php72' Then
    begin
     str_version := str_version + 'php72';
     US_PHP_EXTEN  := US_PHP72_EXT;  // PHP extensions folder
     If UENV_PHP_INI_SELECT ='php_test.ini'        Then USF_PHP_INI_PATH := USF_PHP_INI_TEST_72; // config php_test.ini
     If UENV_PHP_INI_SELECT ='php_development.ini' Then USF_PHP_INI_PATH := USF_PHP_INI_DEV_72;  // config php_development.ini
     If UENV_PHP_INI_SELECT ='php_production.ini'  Then USF_PHP_INI_PATH := USF_PHP_INI_PROD_72; // php_production.ini
    end;
    //--End PHP 72  selected

    //--PHP 73  selected
    If UENV_PHP_SELECT ='php73' Then
    begin
     str_version := str_version + 'php73';
     US_PHP_EXTEN  := US_PHP73_EXT;  // PHP extensions folder
     If UENV_PHP_INI_SELECT ='php_test.ini'        Then USF_PHP_INI_PATH := USF_PHP_INI_TEST_73; // config php_test.ini
     If UENV_PHP_INI_SELECT ='php_development.ini' Then USF_PHP_INI_PATH := USF_PHP_INI_DEV_73;  // config php_development.ini
     If UENV_PHP_INI_SELECT ='php_production.ini'  Then USF_PHP_INI_PATH := USF_PHP_INI_PROD_73; // php_production.ini
    end;
    //--End PHP 73  selected

    //--PHP 74  selected
    If UENV_PHP_SELECT ='php74' Then
    begin
     str_version := str_version + 'php74';
     US_PHP_EXTEN  := US_PHP74_EXT;  // PHP extensions folder
     If UENV_PHP_INI_SELECT ='php_test.ini'        Then USF_PHP_INI_PATH := USF_PHP_INI_TEST_74; // config php_test.ini
     If UENV_PHP_INI_SELECT ='php_development.ini' Then USF_PHP_INI_PATH := USF_PHP_INI_DEV_74;  // config php_development.ini
     If UENV_PHP_INI_SELECT ='php_production.ini'  Then USF_PHP_INI_PATH := USF_PHP_INI_PROD_74; // php_production.ini
    end;
    //--End PHP 74  selected

    //Display text
    Lbl_verson.Caption := str_version;                             //Dispaly currently selected PHP series
    Lbl_file.Caption   := str_file_selected + UENV_PHP_INI_SELECT; //Dispaly currently selected PHP config file

    //Create lists and load config file
    sList_in  := TStringList.Create;           // Create object

    If FileExists(USF_PHP_INI_PATH) Then
     sList_in.LoadFromFile(USF_PHP_INI_PATH);  // Load file

    //==Get list of files in PHP extensions folder
    sList_files := FindAllFiles(US_PHP_EXTEN, '*.*',false); // Create list Do not search subs

    //Extract file name and add extension
    For i:=0 to sList_files.Count-1 do
    begin
     sList_files[i] := ExtractFileNameOnly(sList_files[i]);  // name + extention
    end;

    //Scan sList_in and sList_files and set CheckListBox

    //Scan sList_files
    For j:=0 to sList_files.Count-1 do
    begin
     //Scan sList_pro
     For i:=0 to sList_in.Count-1 do
     begin

      If ((Pos(sList_files[j] + '.dll',sList_in[i]) <> 0) Or
          (Pos(StringReplace(sList_files[j],'php_','',[]),sList_in[i]) <> 0)) Then  //Item found
      begin

       PHP_Extensions_CheckListBox.Items.Add(StringReplace(sList_files[j],'php_','',[]));  //Add to displayed list

       // If first character is not a comment ";" then module has been selected
       If Not(LeftStr(sList_in[i],1) = ';') then
       begin
        PHP_Extensions_CheckListBox.Checked[PHP_Extensions_CheckListBox.Items.Count-1] := True;
       end;
       Break;         // exit loop
      end;

     end; // Scan sList

    end; //End Scan FileListBox

    //Clean
    sList_in.Free;
    sList_files.Free;
end;

procedure Tphp_extensions.PHP_Extensions_CheckListBoxItemClick(Sender: TObject;
  Index: integer);
var
   sList_in          :TStringList; // String list selected php.ini

   i                 :integer;     // Loop counter
   str               :string;
   extensionIndex    :integer;     // Index of line containing extention name
begin

   str := PHP_Extensions_CheckListBox.Items[Index];  //Get selected line

   //===== File php_in.ini ========
   //Create lists and load config file
   sList_in  := TStringList.Create;           // Create object

   If FileExists(USF_PHP_INI_PATH) Then
     sList_in.LoadFromFile(USF_PHP_INI_PATH);  // Load file

    //Search file-stringlist for extension name
    for i:=0 to sList_in.Count-1 do
      begin
        If Pos(str,sList_in[i]) <> 0 then
        begin
          extensionIndex := i; // Extension name found save its index
          Break;               // Nothing else to do
        end;
      end;

   // Set check status
   If PHP_Extensions_CheckListBox.Checked[Index] Then
    begin
      //user checked box
        If (LeftStr(sList_in[i],1) = ';') then // Check for a comment
        begin
           sList_in[extensionIndex] := StringReplace(sList_in[extensionIndex],';','',[]);
        end;
    end
   else
    begin
      //user unchecked box
        If Not(LeftStr(sList_in[i],1) = ';') then // Check not already a comment
        begin
          sList_in[extensionIndex] := ';'+ sList_in[extensionIndex]; // Disable line
        end;
    end;
//   showmessage(sList_ini[extensionIndex]);

  If FileIsWritable(USF_PHP_INI_PATH) Then
     sList_in.SaveToFile(USF_PHP_INI_PATH);      // Save new values to file

  sleep(100);

   //===== END File php_in.ini ========

  //Clean
  sList_in.Free;

end;

end.



