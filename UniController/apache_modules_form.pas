unit apache_modules_form;

{#############################################################################
'# Name: default_config_vars.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#
'#
'# This form lists all Apache modules and shows modules selected.
'# A user can select or deselect modules. The Apache
'# configuration file is updated to reflect user selection.
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  strutils,
  default_config_vars,
  us_common_functions;


type

  { Tapache_modules }

  Tapache_modules = class(TForm)
    Apache_Modules_CheckListBox: TCheckListBox;
    procedure Apache_Modules_CheckListBoxItemClick(Sender: TObject; Index: integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  apache_modules: Tapache_modules;
  sList: TStringList;    // String list
  sList2: TStringList;   // String list
  sList_files: TStringList;

implementation

{$R *.lfm}

{ Tapache_modules }

procedure Tapache_modules.FormCreate(Sender: TObject);
begin
end;

procedure Tapache_modules.FormShow(Sender: TObject);
var
i:integer;             // Loop counter
j:integer;             // Loop counter
str:string;
strFill:string;
MaxLineLength:integer; // Length of first part

begin
  Apache_Modules_CheckListBox.ItemIndex := -1;  // Clear selection
  Apache_Modules_CheckListBox.Items.Clear;      // Clear list box

  MaxLineLength:= 0; //Set intial value

  //Get list of files in Apache modules folder
  sList       := TStringList.Create;      // Create object
  sList2      := TStringList.Create;      // Create object

  sList.LoadFromFile(USF_APACHE_CNF);     // Load file
  sList_files := FindAllFiles(US_APACHE_MODULES, '*.so',false); //Create list. Do not search subs

    //Extract file name and add extension
    for i:=0 to sList_files.Count-1 do
    begin
      sList_files[i] := ExtractFileNameOnly(sList_files[i]+'.so');  // name + extention
    end;

  //Scan sList and FileListBox and set CheckListBox

 //=====Scan for maximum line length
   //Scan sList_files
  for j:=0 to sList_files.Count-1 do
    begin

      //Scan sList
      for i:=0 to sList.Count-1 do
        begin

             If Pos(sList_files[j],sList[i]) <> 0 then
           begin
             str := sList[i];                     // Set string to line
             // Convert string to a string list
             while (Length(str) > 0) do           // Scan line
               begin
                sList2.Add(us_get_word(str, #32)); // Split this at space
               end;

               If MaxLineLength < Length(sList2[1]) Then
                begin
                   MaxLineLength:= Length(sList2[1]);
                end;

             sList2.Clear;  //Clear list
             Break;         // exit loop
           end;

        end; // Scan sList

    end; //End Scan FileListBox
  //=====End Scan for maximum line lenth



  //Scan sList_files
  for j:=0 to sList_files.Count-1 do
    begin

      //Scan sList
      for i:=0 to sList.Count-1 do
        begin

          If Pos(sList_files[j],sList[i]) <> 0 then
           begin
             str := sList[i];                     // Set string to line
             // Convert string to a string list
             while (Length(str) > 0) do           // Scan line
               begin
                sList2.Add(us_get_word(str, #32)); // Split this at space
               end;

             strFill:= AddCharR(' ',sList2[1],MaxLineLength+3);

             Apache_Modules_CheckListBox.Items.Add(strFill+ sList_files[j]);

             // If # not found module is selected
             If Pos('#',sList[i]) = 0 then
             begin
               Apache_Modules_CheckListBox.Checked[Apache_Modules_CheckListBox.Items.Count-1] := True;
             end;

             sList2.Clear;  //Clear list
             Break;         // exit loop
           end;

        end; // Scan sList
    end; //End Scan FileListBox
end;

procedure Tapache_modules.Apache_Modules_CheckListBoxItemClick(Sender: TObject;Index: integer);
var
   i:integer;             // Loop counter
   str:string;
   sList3: TStringList;   // String list
   moduleIndex:integer;   // Index of line contained module name
begin

   sList3 := TStringList.Create;                     // Create object
   str := Apache_Modules_CheckListBox.Items[Index];  //Get selected line

   //Split selected item and use last string (module name) for search
   while (Length(str) > 0) do                       // Scan line
    begin
      sList3.Add(us_get_word(str, #32));            // Split space
    end;

    //Search file-stringlist for module name
    for i:=0 to sList.Count-1 do
      begin
        If Pos(sList3[sList3.Count-1],sList[i]) <> 0 then
        begin
          moduleIndex := i; // Module name found save its index
          Break;            // Nothing else to do
        end;
      end;

   // Set check status
   If Apache_Modules_CheckListBox.Checked[Index] Then
    begin
      //user checked box
        If Pos('#',sList[moduleIndex]) = 1 then
        begin
           sList[moduleIndex] := StringReplace(sList[moduleIndex],'#','',[]);
        end;
    end
   else
    begin
      //user unchecked box
         If Pos('#',sList[moduleIndex]) = 0 then
        begin
          sList[moduleIndex] := '#'+ sList[moduleIndex]; // Disable line
        end;
    end;
//   showmessage(sList[moduleIndex]);

   If FileIsWritable(USF_APACHE_CNF) Then
     begin
       sList.SaveToFile(USF_APACHE_CNF);  // Save new values to file
     end;
   sleep(100);

   sList3.Free;                           // Remove from memory
end;

procedure Tapache_modules.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
    sList.Free;                            // Remove from memory
    sList2.Free;                           // Remove from memory
    sList_files.Free;                      // Remove from memory
end;

end.



