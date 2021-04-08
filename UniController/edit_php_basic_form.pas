unit edit_php_basic_form;

{#############################################################################
'# Name: edit_php_basic_form.pas
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
  us_common_functions,
  RegExpr;

type

  { Tedit_php_basic }

  Tedit_php_basic = class(TForm)
    Btn_update_production: TButton;
    Btn_help: TButton;
    Btn_update_test: TButton;
    Btn_update_development: TButton;
    Com_p_display_errors: TComboBox;
    Com_p_short_open_tags: TComboBox;
    Com_p_startup_errors: TComboBox;
    Com_p_track_errors: TComboBox;
    Com_p_html_errors: TComboBox;
    Com_p_var_order: TComboBox;
    Com_d_server_signature: TComboBox;
    Com_p_server_signature: TComboBox;
    Com_d_display_errors: TComboBox;
    Com_d_short_open_tags: TComboBox;
    Com_d_startup_errors: TComboBox;
    Com_d_track_errors: TComboBox;
    Com_d_html_errors: TComboBox;
    Com_d_var_order: TComboBox;
    Com_c_display_errors: TComboBox;
    Com_c_short_open_tags: TComboBox;
    Com_c_startup_errors: TComboBox;
    Com_c_track_errors: TComboBox;
    Com_c_html_errors: TComboBox;
    Com_c_var_order: TComboBox;
    Com_c_server_signature: TComboBox;
    Ed_d_max_exe_time: TEdit;
    Ed_d_max_memory: TEdit;
    Ed_d_max_post_size: TEdit;
    Ed_d_max_upload_size: TEdit;
    Ed_c_max_exe_time: TEdit;
    Ed_p_max_exe_time: TEdit;
    Ed_p_max_memory: TEdit;
    Ed_p_max_post_size: TEdit;
    Ed_p_max_upload_size: TEdit;
    Ed_c_max_memory: TEdit;
    Ed_c_max_post_size: TEdit;
    Ed_c_max_upload_size: TEdit;
    Gb_production: TGroupBox;
    Gb_current: TGroupBox;
    Gb_development: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure Btn_helpClick(Sender: TObject);
    procedure Btn_update_testClick(Sender: TObject);
    procedure Btn_update_developmentClick(Sender: TObject);
    procedure Btn_update_productionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  edit_php_basic: Tedit_php_basic;

  USF_PHP_INI       :String;    // Path to ini for selected PHP version
  USF_PHP_INI_PROD  :String;    // Path to pro ini for selected PHP version
  USF_PHP_INI_DEV   :String;    // Path to dev ini for selected PHP version

implementation

{$R *.lfm}

{ Tedit_php_basic }

procedure Tedit_php_basic.Btn_update_testClick(Sender: TObject);
var
sList: TStringList;   // String list
RegexObj: TRegExpr;   // Object
i:integer;            // loop counter
begin
     //=== Save new settings to configuration file php_test.ini
   if FileExists(USF_PHP_INI) Then
    begin
       sList := TStringList.Create;        // Create object
       sList.LoadFromFile(USF_PHP_INI);    // Load file
       RegexObj := TRegExpr.Create;        // Create regex obj

       //Scan list
       for i:=0 to sList.Count-1 do
        begin

         //-- Get Show PHP In Server Signature
          RegexObj.Expression := '^\s*expose_php\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                     // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Com_c_server_signature.Text,True); // Replace with user input
             end;

         //-- Get Maximum Script Execute Time (s.)
         RegexObj.Expression := '^\s*max_execution_time\s*=\s*(\w+)'; // Set search pattern
          if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                             // Match found
            begin
             //Search for the match found and replace with new string
             RegexObj.Expression := RegexObj.Match[1];                           // Set new search pattern.
             sList[i] := RegexObj.Replace(sList[i],Ed_c_max_exe_time.Text,True); // Replace with user input
            end;

          //-- Get Maximum Memory Amount (MB)
          RegexObj.Expression := '^\s*memory_limit\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                       // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                         // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Ed_c_max_memory.Text,True); // Replace with user input
             end;

         //-- Get Display Errors
         RegexObj.Expression := '^\s*display_errors\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                              // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Com_c_display_errors.Text,True); // Replace with user input
             end;

          //-- Get Maximum Post Size
         RegexObj.Expression := '^\s*post_max_size\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Ed_c_max_post_size.Text,True); // Replace with user input
             end;

         //-- Get Maximum Upload Size
         RegexObj.Expression := '^\s*upload_max_filesize\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                              // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                              // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Ed_c_max_upload_size.Text,True); // Replace with user input
             end;

          //-- Get Short Open Tags
          RegexObj.Expression := '^\s*short_open_tag\s*=\s*(\w+)';  //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                               // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_c_short_open_tags.Text,True); // Replace with user input
              end;

          //-- Get Display startup errors
          RegexObj.Expression := '^\s*display_startup_errors\s*=\s*(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                 // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_c_startup_errors.Text,True); // Replace with user input
              end;

          //-- Get Track errors
          RegexObj.Expression := '^\s*track_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_c_track_errors.Text,True); // Replace with user input
              end;

           //-- Get Html errors
          RegexObj.Expression := '^\s*html_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_c_html_errors.Text,True); // Replace with user input
              end;

          //-- Get Variables order
          RegexObj.Expression := '^\s*variables_order\s*=\s*"(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                           // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_c_var_order.Text,True); // Replace with user input
              end;

         end;

         If FileIsWritable(USF_PHP_INI) Then
           begin
            sList.SaveToFile(USF_PHP_INI);  // Save new values to file
           end;
         sleep(100);


        //Inform user
        us_MessageDlg('PHP Configuration', 'PHP Configuration file updated' , mtInformation,[mbOk],0) ; //Display message

       //Clean up
       sList.Free;                         // Remove from memory
       RegexObj.Free;                      // release object
        end;

end;

procedure Tedit_php_basic.Btn_helpClick(Sender: TObject);
var
str:string;
begin
  //str_php_config_main_help_title = PHP Configuration
  str :='';
  str := str + 'This form allows you to change commonly configured PHP options' + sLineBreak + sLineBreak;

  str := str + 'php_test.ini file:'  + sLineBreak;
  str := str + 'Allows configuration testing prior to changing php_development.ini or php_production.ini'    + sLineBreak + sLineBreak;

  str := str + 'php_development.ini file.'  + sLineBreak;
  str := str + 'Allows errors to be written to screen. You can enable other'    + sLineBreak;
  str := str + 'parameters for testing.'  + sLineBreak + sLineBreak;

  str := str + 'php_production.ini file. ' + sLineBreak;
  str := str + 'Prevents errors being written to screen. Settings tighten security.'  + sLineBreak + sLineBreak;

  str := str + 'Note: Short open tags <? and ?> are switched off by default.'  + sLineBreak;
  str := str + 'Older scripts may require these enable only for testing!';

  us_MessageDlg('PHP Configuration', str, mtInformation,[mbOk],0) ; //Display message
end;

procedure Tedit_php_basic.Btn_update_developmentClick(Sender: TObject);
var
sList: TStringList;   // String list
RegexObj: TRegExpr;   // Object
i:integer;            // loop counter
begin
    //=== Save new settings to configuration file php_development.ini
   if FileExists(USF_PHP_INI_DEV) Then
    begin
       sList := TStringList.Create;        // Create object
       sList.LoadFromFile(USF_PHP_INI_DEV);    // Load file
       RegexObj := TRegExpr.Create;        // Create regex obj

       //Scan list
       for i:=0 to sList.Count-1 do
        begin

         //-- Get Show PHP In Server Signature
          RegexObj.Expression := '^\s*expose_php\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                     // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Com_d_server_signature.Text,True); // Replace with user input
             end;

         //-- Get Maximum Script Execute Time (s.)
         RegexObj.Expression := '^\s*max_execution_time\s*=\s*(\w+)'; // Set search pattern
          if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                             // Match found
            begin
             //Search for the match found and replace with new string
             RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
             sList[i] := RegexObj.Replace(sList[i],Ed_d_max_exe_time.Text,True); // Replace with user input
            end;

          //-- Get Maximum Memory Amount (MB)
          RegexObj.Expression := '^\s*memory_limit\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                       // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Ed_d_max_memory.Text,True); // Replace with user input
             end;

         //-- Get Display Errors
         RegexObj.Expression := '^\s*display_errors\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Com_d_display_errors.Text,True); // Replace with user input
             end;

          //-- Get Maximum Post Size
         RegexObj.Expression := '^\s*post_max_size\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Ed_d_max_post_size.Text,True); // Replace with user input
             end;

         //-- Get Maximum Upload Size
         RegexObj.Expression := '^\s*upload_max_filesize\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                              // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Ed_d_max_upload_size.Text,True); // Replace with user input
             end;

          //-- Get Short Open Tags
          RegexObj.Expression := '^\s*short_open_tag\s*=\s*(\w+)';  //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_d_short_open_tags.Text,True); // Replace with user input
              end;

          //-- Get Display startup errors
          RegexObj.Expression := '^\s*display_startup_errors\s*=\s*(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                 // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_d_startup_errors.Text,True); // Replace with user input
              end;

          //-- Get Track errors
          RegexObj.Expression := '^\s*track_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_d_track_errors.Text,True); // Replace with user input
              end;

           //-- Get Html errors
          RegexObj.Expression := '^\s*html_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_d_html_errors.Text,True); // Replace with user input
              end;

          //-- Get Variables order
          RegexObj.Expression := '^\s*variables_order\s*=\s*"(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                           // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_d_var_order.Text,True); // Replace with user input
              end;

         end;

         If FileIsWritable(USF_PHP_INI_DEV) Then
           begin
            sList.SaveToFile(USF_PHP_INI_DEV);  // Save new values to file
           end;
         sleep(100);

        //Inform user
        us_MessageDlg('PHP Configuration', 'PHP Configuration file updated' , mtInformation,[mbOk],0) ; //Display message

        //Clean up
       sList.Free;                         // Remove from memory
       RegexObj.Free;                      // release object
        end;
end;

procedure Tedit_php_basic.Btn_update_productionClick(Sender: TObject);
var
sList: TStringList;   // String list
RegexObj: TRegExpr;   // Object
i:integer;            // loop counter
begin
   //=== Save new settings to configuration file php_production.ini
   if FileExists(USF_PHP_INI_PROD) Then
    begin
       sList := TStringList.Create;        // Create object
       sList.LoadFromFile(USF_PHP_INI_PROD);    // Load file
       RegexObj := TRegExpr.Create;        // Create regex obj

       //Scan list
       for i:=0 to sList.Count-1 do
        begin

         //-- Get Show PHP In Server Signature
          RegexObj.Expression := '^\s*expose_php\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                     // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Com_p_server_signature.Text,True); // Replace with user input
             end;

         //-- Get Maximum Script Execute Time (s.)
         RegexObj.Expression := '^\s*max_execution_time\s*=\s*(\w+)'; // Set search pattern
          if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                             // Match found
            begin
             //Search for the match found and replace with new string
             RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
             sList[i] := RegexObj.Replace(sList[i],Ed_p_max_exe_time.Text,True); // Replace with user input
            end;

          //-- Get Maximum Memory Amount (MB)
          RegexObj.Expression := '^\s*memory_limit\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                       // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Ed_p_max_memory.Text,True); // Replace with user input
             end;

         //-- Get Display Errors
         RegexObj.Expression := '^\s*display_errors\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Com_p_display_errors.Text,True); // Replace with user input
             end;

          //-- Get Maximum Post Size
         RegexObj.Expression := '^\s*post_max_size\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Ed_p_max_post_size.Text,True); // Replace with user input
             end;

         //-- Get Maximum Upload Size
         RegexObj.Expression := '^\s*upload_max_filesize\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                              // Match found
             begin
              //Search for the match found and replace with new string
              RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
              sList[i] := RegexObj.Replace(sList[i],Ed_p_max_upload_size.Text,True); // Replace with user input
             end;

          //-- Get Short Open Tags
          RegexObj.Expression := '^\s*short_open_tag\s*=\s*(\w+)';  //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_p_short_open_tags.Text,True); // Replace with user input
              end;

          //-- Get Display startup errors
          RegexObj.Expression := '^\s*display_startup_errors\s*=\s*(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                 // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_p_startup_errors.Text,True); // Replace with user input
              end;

          //-- Get Track errors
          RegexObj.Expression := '^\s*track_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_p_track_errors.Text,True); // Replace with user input
              end;

           //-- Get Html errors
          RegexObj.Expression := '^\s*html_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_p_html_errors.Text,True); // Replace with user input
              end;

          //-- Get Variables order
          RegexObj.Expression := '^\s*variables_order\s*=\s*"(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                           // Match found
              begin
               //Search for the match found and replace with new string
               RegexObj.Expression := RegexObj.Match[1];                                // Set new search pattern.
               sList[i] := RegexObj.Replace(sList[i],Com_p_var_order.Text,True); // Replace with user input
              end;

         end;

         If FileIsWritable(USF_PHP_INI_PROD) Then
           begin
            sList.SaveToFile(USF_PHP_INI_PROD);  // Save new values to file
           end;
         sleep(100);

        //Inform user
         us_MessageDlg('PHP Configuration', 'PHP Configuration file updated' , mtInformation,[mbOk],0) ; //Display message

        //Clean up
       sList.Free;                         // Remove from memory
       RegexObj.Free;                      // release object
        end;
end;

procedure Tedit_php_basic.FormShow(Sender: TObject);
var
sList: TStringList;   // String list
RegexObj: TRegExpr;   // Object
i:integer;            // loop counter
begin

  USF_PHP_INI       :='';    // Path to ini for selected PHP version
  USF_PHP_INI_DEV   :='';    // Path to dev ini for selected PHP version
  USF_PHP_INI_PROD  :='';    // Path to pro ini for selected PHP version

//==Set config file paths for selected PHP version

//--PHP 70  selected
If UENV_PHP_SELECT ='php70' then
 begin
  USF_PHP_INI      := USF_PHP_INI_TEST_70; // config php_test.ini
  USF_PHP_INI_DEV  := USF_PHP_INI_DEV_70;  // config php_development.ini
  USF_PHP_INI_PROD := USF_PHP_INI_PROD_70; // php_production.ini
 end;
//--End PHP 70  selected

//--PHP 71  selected
If UENV_PHP_SELECT ='php71' then
 begin
  USF_PHP_INI      := USF_PHP_INI_TEST_71; // config php_test.ini
  USF_PHP_INI_DEV  := USF_PHP_INI_DEV_71;  // config php_development.ini
  USF_PHP_INI_PROD := USF_PHP_INI_PROD_71; // php_production.ini
 end;
//--End PHP 71  selected

//--PHP 72  selected
If UENV_PHP_SELECT ='php72' then
 begin
  USF_PHP_INI      := USF_PHP_INI_TEST_72; // config php_test.ini
  USF_PHP_INI_DEV  := USF_PHP_INI_DEV_72;  // config php_development.ini
  USF_PHP_INI_PROD := USF_PHP_INI_PROD_72; // php_production.ini
 end;
//--End PHP 72  selected

//--PHP 73  selected
If UENV_PHP_SELECT ='php73' then
 begin
  USF_PHP_INI      := USF_PHP_INI_TEST_73; // config php_test.ini
  USF_PHP_INI_DEV  := USF_PHP_INI_DEV_73;  // config php_development.ini
  USF_PHP_INI_PROD := USF_PHP_INI_PROD_73; // php_production.ini
 end;
//--End PHP 73  selected

//--PHP 74  selected
If UENV_PHP_SELECT ='php74' then
 begin
  USF_PHP_INI      := USF_PHP_INI_TEST_74; // config php_test.ini
  USF_PHP_INI_DEV  := USF_PHP_INI_DEV_74;  // config php_development.ini
  USF_PHP_INI_PROD := USF_PHP_INI_PROD_74; // php_production.ini
 end;
//--End PHP 74  selected

//--PHP 80  selected
If UENV_PHP_SELECT ='php80' then
 begin
  USF_PHP_INI      := USF_PHP_INI_TEST_80; // config php_test.ini
  USF_PHP_INI_DEV  := USF_PHP_INI_DEV_80;  // config php_development.ini
  USF_PHP_INI_PROD := USF_PHP_INI_PROD_80; // php_production.ini
 end;
//--End PHP 80  selected

    //=== Get data from configuration file php_test.ini
   if FileExists(USF_PHP_INI) Then
    begin
       sList := TStringList.Create;        // Create object
       sList.LoadFromFile(USF_PHP_INI);    // Load file
       RegexObj := TRegExpr.Create;        // Create regex obj

       //Scan list
       for i:=0 to sList.Count-1 do
        begin

         //-- Get Show PHP In Server Signature
          RegexObj.Expression := '^\s*expose_php\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                     // Match found
             begin
              Com_c_server_signature.Text := RegexObj.Match[1]; //Display setting
             end;

         //-- Get Maximum Script Execute Time (s.)
         RegexObj.Expression := '^\s*max_execution_time\s*=\s*(\w+)'; // Set search pattern
          if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                             // Match found
            begin
             Ed_c_max_exe_time.Text := RegexObj.Match[1];             //Display setting
            end;

          //-- Get Maximum Memory Amount (MB)
          RegexObj.Expression := '^\s*memory_limit\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                       // Match found
             begin
              Ed_c_max_memory.Text := RegexObj.Match[1];         //Display size MB
             end;

         //-- Get Display Errors
         RegexObj.Expression := '^\s*display_errors\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              Com_c_display_errors.Text := RegexObj.Match[1];      //Display errors
             end;

          //-- Get Maximum Post Size
         RegexObj.Expression := '^\s*post_max_size\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              Ed_c_max_post_size.Text := RegexObj.Match[1];       //Display post size
             end;

         //-- Get Maximum Upload Size
         RegexObj.Expression := '^\s*upload_max_filesize\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                              // Match found
             begin
              Ed_c_max_upload_size.Text := RegexObj.Match[1];           //Display post size
             end;

          //-- Get Short Open Tags
          RegexObj.Expression := '^\s*short_open_tag\s*=\s*(\w+)';  //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               Com_c_short_open_tags.Text := RegexObj.Match[1];      //Display open tags
              end;

          //-- Get Display startup errors
          RegexObj.Expression := '^\s*display_startup_errors\s*=\s*(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                 // Match found
              begin
               Com_c_startup_errors.Text := RegexObj.Match[1];               // Display startup errors
              end;

          //-- Get Track errors
          RegexObj.Expression := '^\s*track_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               Com_c_track_errors.Text := RegexObj.Match[1];         // Display Track errors
              end;

           //-- Get Html errors
          RegexObj.Expression := '^\s*html_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               Com_c_html_errors.Text := RegexObj.Match[1];          // Display Html errors
              end;

          //-- Get Variables order
          RegexObj.Expression := '^\s*variables_order\s*=\s*"(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                           // Match found
              begin
               Com_c_var_order.Text := RegexObj.Match[1];              // Display Variables order
              end;

         end;
        //Clean up
        sList.Free;                         // Remove from memory
        RegexObj.Free;                      // release object
       end;


  //=== Get values from configuration file php_development.ini

    if FileExists(USF_PHP_INI_DEV) Then
    begin
       sList := TStringList.Create;         // Create object
       sList.LoadFromFile(USF_PHP_INI_DEV); // Load file
       RegexObj := TRegExpr.Create;         // Create regex obj

       //Scan list
       for i:=0 to sList.Count-1 do
        begin

         //-- Get Show PHP In Server Signature
          RegexObj.Expression := '^\s*expose_php\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                     // Match found
             begin
              Com_d_server_signature.Text := RegexObj.Match[1]; //Display setting
             end;

         //-- Get Maximum Script Execute Time (s.)
         RegexObj.Expression := '^\s*max_execution_time\s*=\s*(\w+)'; // Set search pattern
          if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                             // Match found
            begin
             Ed_d_max_exe_time.Text := RegexObj.Match[1];             //Display setting
            end;

          //-- Get Maximum Memory Amount (MB)
          RegexObj.Expression := '^\s*memory_limit\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                       // Match found
             begin
              Ed_d_max_memory.Text := RegexObj.Match[1];         //Display size MB
             end;

         //-- Get Display Errors
         RegexObj.Expression := '^\s*display_errors\s*=\s*(\w+)'; //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              Com_d_display_errors.Text := RegexObj.Match[1];      //Display errors
             end;

          //-- Get Maximum Post Size
         RegexObj.Expression := '^\s*post_max_size\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
             begin
              Ed_d_max_post_size.Text := RegexObj.Match[1];       //Display post size
             end;

         //-- Get Maximum Upload Size
         RegexObj.Expression := '^\s*upload_max_filesize\s*=\s*(\w+)';  //Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                              // Match found
             begin
              Ed_d_max_upload_size.Text := RegexObj.Match[1];           //Display post size
             end;

          //-- Get Short Open Tags
          RegexObj.Expression := '^\s*short_open_tag\s*=\s*(\w+)';  //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               Com_d_short_open_tags.Text := RegexObj.Match[1];      //Display open tags
              end;

          //-- Get Display startup errors
          RegexObj.Expression := '^\s*display_startup_errors\s*=\s*(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                 // Match found
              begin
               Com_d_startup_errors.Text := RegexObj.Match[1];               // Display startup errors
              end;

          //-- Get Track errors
          RegexObj.Expression := '^\s*track_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               Com_d_track_errors.Text := RegexObj.Match[1];         // Display Track errors
              end;

           //-- Get Html errors
          RegexObj.Expression := '^\s*html_errors\s*=\s*(\w+)';    // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
              begin
               Com_d_html_errors.Text := RegexObj.Match[1];          // Display Html errors
              end;

          //-- Get Variables order
          RegexObj.Expression := '^\s*variables_order\s*=\s*"(\w+)';  // Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                           // Match found
              begin
               Com_d_var_order.Text := RegexObj.Match[1];              // Display Variables order
              end;

         end;

        //Clean up
        sList.Free;                         // Remove from memory
        RegexObj.Free;                      // release object

       end;


  //=== Get values from configuration file php_production.ini

    if FileExists(USF_PHP_INI_PROD) Then
     begin
        sList := TStringList.Create;          // Create object
        sList.LoadFromFile(USF_PHP_INI_PROD); // Load file
        RegexObj := TRegExpr.Create;          // Create regex obj

        //Scan list
        for i:=0 to sList.Count-1 do
         begin

          //-- Get Show PHP In Server Signature
           RegexObj.Expression := '^\s*expose_php\s*=\s*(\w+)'; //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                     // Match found
              begin
               Com_p_server_signature.Text := RegexObj.Match[1]; //Display setting
              end;

          //-- Get Maximum Script Execute Time (s.)
          RegexObj.Expression := '^\s*max_execution_time\s*=\s*(\w+)'; // Set search pattern
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                             // Match found
             begin
              Ed_p_max_exe_time.Text := RegexObj.Match[1];             //Display setting
             end;

           //-- Get Maximum Memory Amount (MB)
           RegexObj.Expression := '^\s*memory_limit\s*=\s*(\w+)'; //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                       // Match found
              begin
               Ed_p_max_memory.Text := RegexObj.Match[1];         //Display size MB
              end;

          //-- Get Display Errors
          RegexObj.Expression := '^\s*display_errors\s*=\s*(\w+)'; //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
              begin
               Com_p_display_errors.Text := RegexObj.Match[1];      //Display errors
              end;

           //-- Get Maximum Post Size
          RegexObj.Expression := '^\s*post_max_size\s*=\s*(\w+)';  //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                        // Match found
              begin
               Ed_p_max_post_size.Text := RegexObj.Match[1];       //Display post size
              end;

          //-- Get Maximum Upload Size
          RegexObj.Expression := '^\s*upload_max_filesize\s*=\s*(\w+)';  //Set search pattern
            if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                              // Match found
              begin
               Ed_p_max_upload_size.Text := RegexObj.Match[1];           //Display post size
              end;

           //-- Get Short Open Tags
           RegexObj.Expression := '^\s*short_open_tag\s*=\s*(\w+)';  //Set search pattern
             if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
               begin
                Com_p_short_open_tags.Text := RegexObj.Match[1];      //Display open tags
               end;

           //-- Get Display startup errors
           RegexObj.Expression := '^\s*display_startup_errors\s*=\s*(\w+)';  // Set search pattern
             if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                 // Match found
               begin
                Com_p_startup_errors.Text := RegexObj.Match[1];               // Display startup errors
               end;

           //-- Get Track errors
           RegexObj.Expression := '^\s*track_errors\s*=\s*(\w+)';    // Set search pattern
             if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
               begin
                Com_p_track_errors.Text := RegexObj.Match[1];         // Display Track errors
               end;

            //-- Get Html errors
           RegexObj.Expression := '^\s*html_errors\s*=\s*(\w+)';    // Set search pattern
             if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                         // Match found
               begin
                Com_p_html_errors.Text := RegexObj.Match[1];          // Display Html errors
               end;

           //-- Get Variables order
           RegexObj.Expression := '^\s*variables_order\s*=\s*"(\w+)';  // Set search pattern
             if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                           // Match found
               begin
                Com_p_var_order.Text := RegexObj.Match[1];              // Display Variables order
               end;

          end;

         //Clean up
         sList.Free;                         // Remove from memory
         RegexObj.Free;                      // release object
        end;


 end;

procedure Tedit_php_basic.ListBox1Click(Sender: TObject);
begin

end;

end.

