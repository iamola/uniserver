unit msmtp_php_mail_form;

{#############################################################################
'# Name: msmtp_php_mail_form.pas
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
  RegExpr,
  Process;

type

  { Tmsmtp_php_mail }

  Tmsmtp_php_mail = class(TForm)
    Btn_send: TButton;
    Btn_clear: TButton;
    Btn_edit_config_file: TButton;
    CB_select_default_account: TComboBox;
    Ed_mail_to: TEdit;
    Ed_subject: TEdit;
    GB_1: TGroupBox;
    GB_2: TGroupBox;
    GB_3_log_file: TGroupBox;
    Lbl_configure_accounts: TLabel;
    Lbl_select_set: TLabel;
    Lbl_email_to: TLabel;
    Lbl_subject: TLabel;
    Lbl_message: TLabel;
    Memo_message: TMemo;
    Memo_log: TMemo;
    procedure Btn_clearClick(Sender: TObject);
    procedure Btn_edit_config_fileClick(Sender: TObject);
    procedure Btn_sendClick(Sender: TObject);
    procedure CB_select_default_accountSelect(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo_messageClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  msmtp_php_mail: Tmsmtp_php_mail;

implementation

{$R *.lfm}
{================================================================
Dsplay log file
-----------------------------------------------------------------}
procedure display_log;
var
sList: TStringList;                   // String list
i:integer;                            // Loop counter
begin
  If FileExists(USF_MSMTP_LOG)Then
    begin
     msmtp_php_mail.Memo_log.Lines.Clear;  // Clear memo content
     sList  := TStringList.Create;         // Create object
     sList.LoadFromFile(USF_MSMTP_LOG);    // Load file
     //Scan sList
     for i:=0 to sList.Count-1 do
      begin
        msmtp_php_mail.Memo_log.Lines.Add(sList[i]); // Add lines
      end;//End scan list

    sList.Free;   // Remove from memory
  end;
end;
{----------------------------------------------------------------}

{ Tmsmtp_php_mail }

procedure Tmsmtp_php_mail.CB_select_default_accountSelect(Sender: TObject);
var
  sList    : TStringList; // String list
  i        :integer;      // Loop counter
  RegexObj : TRegExpr;    // Object
begin
  //List accounts configured
  If Fileexists(USF_MSMTP_INI) Then
   begin
       sList  := TStringList.Create;       // Create object
       sList.LoadFromFile(USF_MSMTP_INI);  // Load file
       RegexObj := TRegExpr.Create;

       //Scan sList and update default account
       RegexObj.Expression := '^\s*account\s+default\s*:\s*([^\s]*)\s*';  //Set search pattern
       for i:=0 to sList.Count-1 do
         begin
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                     // Match found
             begin
                sList[i] := 'account default : ' + CB_select_default_account.Text;   //Display in select box
                break;
             end;
         end;//End scan list

          If FileIsWritable(USF_MSMTP_INI) Then
             sList.SaveToFile(USF_MSMTP_INI);     // Save new values to file

         sleep(100);

        RegexObj.Free;  // release object
        sList.Free;     // Remove from memory
   end;
end;


procedure Tmsmtp_php_mail.Btn_edit_config_fileClick(Sender: TObject);
begin
      us_display_in_editor(USF_MSMTP_INI);
end;

{===================================================================
Sending an e-mail using the command line has the following format:
The Subject and message text are contained in an external file
this is piped "<" into the command line.

msmtp --file=full_path\msmtprc.ini user_name@target_email_address -t < some_file\temp.mail

File: temp.mail format

Subject: This is a test       // Subject required
[blank line]                  // *** Must include a blank line ***
Testing (your content).       // Mail message follows
More content blah blah..      // More lines
--------------------------------------------------------------------}
procedure Tmsmtp_php_mail.Btn_sendClick(Sender: TObject);
var
  sList: TStringList;      // String list
  i:integer;               // Loop counter
  AProcess: TProcess;      // Process handle
begin
 //Create mail file to be piped to the command line
  sList  := TStringList.Create;               // Create object
  sList.Add('Subject: '+ Ed_subject.Text);    // Add subject line
  sList.Add('');                              // *** Add blank line subject/message seperator

  //Scan message memo and add these lines to mail file list
  for i := 0 to Memo_message.Lines.Count -1 do
      sList.Add(Memo_message.Lines[i]);    // Add message line to mail file list

  //Save file and clean
  sList.SaveToFile(USF_MSMTP_MESSAGE_TEMP); // Save new values to file

  sleep(500);

  sList.Free;   // Remove from memory

  //=== Create command and send mail
  //--Run command string.
  AProcess := TProcess.Create(nil);                  // Create new process

  AProcess.Executable := 'cmd';                      // Executable to run
  AProcess.Parameters.Add('/T:B0');                  // Set background colour
  AProcess.Parameters.Add('/c');                     // Close on completion
  AProcess.Parameters.Add('title');                  // A title is required
  AProcess.Parameters.Add('US_Test');                // Title

  //msmtp --file=full_path\msmtprc.ini user_name@target_email_address -t < some_file\temp.mail
  AProcess.Parameters.Add('&&');                     // Start a new command line
  AProcess.Parameters.Add(USF_MSMTP_EXE);            // Executable exe
  AProcess.Parameters.Add('--file='+USF_MSMTP_INI);  // Ini file
  AProcess.Parameters.Add(Ed_mail_to.Text);          // user_name@target_email_address
  AProcess.Parameters.Add('-t');                     // Command parameter
  AProcess.Parameters.Add('<');                      // File pipe
  AProcess.Parameters.Add(USF_MSMTP_MESSAGE_TEMP);   //Subject-message file
  AProcess.Parameters.Add('');

  AProcess.Options     := AProcess.Options + [poWaitOnExit];  // Set option wait to complete
  AProcess.ShowWindow  := swoHIDE;                            // Hide command window
  AProcess.Execute;                                           // Run command
  AProcess.Free;                                              // Release process
  //=== End Create command and send mail


  //Remove file
  DeleteFile(USF_MSMTP_MESSAGE_TEMP);

  //Display log file
  display_log;

end;

procedure Tmsmtp_php_mail.Btn_clearClick(Sender: TObject);
Var
   f : system.text;
begin

 Memo_log.Lines.Clear;             // Clear log memo content
 If Fileexists(USF_MSMTP_LOG) Then // Check for log file. If exist empty it
   begin
     AssignFile(f, USF_MSMTP_LOG); // Assign file
     Rewrite(f);                   // Clear file
     CloseFile(f);                 // Close empty file
   end;

 Memo_log.Lines.LoadFromFile(USF_MSMTP_LOG); // Load file

 end;

procedure Tmsmtp_php_mail.FormShow(Sender: TObject);
var
  sList    :TStringList; // String list
  i        :integer;     // Loop counter
  RegexObj :TRegExpr;    // Object
begin
  //List accounts configured
  If Fileexists(USF_MSMTP_INI) Then
   begin
       CB_select_default_account.Clear;     // Clear account pull down
       sList  := TStringList.Create;        // Create object
       sList.LoadFromFile(USF_MSMTP_INI);   // Load file
       RegexObj := TRegExpr.Create;
       RegexObj.Expression := '^\s*account\s*([^\s]*)\s*';  //Set search pattern

       //Scan sList and add accounts
       RegexObj.Expression := '^\s*account\s*([^\s]*)\s*';  //Set search pattern
       for i:=0 to sList.Count-1 do
         begin
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                            // Match found
             begin
               if not (RegexObj.Match[1] = 'default') Then
                  CB_select_default_account.Items.Add(RegexObj.Match[1]); //Display in select box
             end;
           end;//End scan list

       //Scan sList and add default account in selected window
       RegexObj.Expression := '^\s*account\s+default\s*:\s*([^\s]*)\s*';  //Set search pattern
       for i:=0 to sList.Count-1 do
         begin
           if (sList[i]<>'') and RegexObj.Exec(sList[i]) then                                // Match found
                CB_select_default_account.Text := RegexObj.Match[1];   //Display in select box
           end;//End scan list

        RegexObj.Free;  // release object
        sList.Free;     // Remove from memory
   end;

  //Display log file
  display_log;
end;


procedure Tmsmtp_php_mail.Memo_messageClick(Sender: TObject);
begin
   //  Memo_message.Lines.Clear; //  Clear default memo content
end;


end.

