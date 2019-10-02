unit cron_form;

{#############################################################################
'# Name: cron_form.pas
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
  default_config_vars,
  us_common_procedures,
  RegExpr,
  dateutils,
  process,
  httpsend,
  blcksock;

//==Common procedure
procedure run_cron; //Run Cron - Runs command line scripts

type

  { Tcron }

  Tcron = class(TForm)
    Btn_edit_config: TButton;
    Btn_view_log: TButton;
    CB_enable_cron: TCheckBox;
    CB_enable_cron_logging: TCheckBox;
    GB1_cron_config_view_log: TGroupBox;
    G2_cron_enable_disable: TGroupBox;
    G2_logging_enable_disable: TGroupBox;
    procedure Btn_edit_configClick(Sender: TObject);
    procedure Btn_view_logClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CB_enable_run_cronClick(Sender: TObject);
    procedure CB_enable_cron_loggingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  cron: Tcron;
  //-- Cron configuration file
  sList_cron :TStringList;   // String list contains configuration file

implementation

{===========================================================================
Cron Logging
 Logs Cron actions to a log file
 Input:         String to be logged
 USF_CRON_LOG   Path to file including file name
===========================================================================}
procedure cron_log(str_ip:string);
var
  FileVar1 :System.Text;   // File handle note use of System
  new_str  :String;        // Text string to log
  time_now :string;
begin

  //Set time now or blank line
  If (str_ip ='') Then
    new_str  := ''                        //Blank line
  Else
   begin
     //Time now
     time_now := FormatDateTime('dd/MM/YYYY HH:MM:SS', Now);
     new_str  := time_now +' ' + str_ip; //Date, time and string to log
   end;

  //Check file exists. If file does not exist, create it.
  If not FileExists(USF_CRON_LOG) Then
    begin
      System.Assign(FileVar1,USF_CRON_LOG); // Assign file
      Rewrite(FileVar1);                    // Create the file
      CloseFile(FileVar1);                  // Close file
    end;

  //Add line to log
  System.Assign(FileVar1,USF_CRON_LOG);  // Assign file (create handle)
  Append(FileVar1);                      // opens existing file for appending
  writeln(FileVar1,new_str);             // Write line adds line to end of file
  CloseFile(FileVar1);                   // Release handle
end;
{---End cron_log ----------------------------------------------------------}

{===========================================================================
Run Path Command:

 From Portable Cron’s perspective only two file groups exists, files starting
 with http: are destined to run on a server while all other files are run as
 command line scripts.

 Command line scripts are further classified using their file extension allowing
 a script to be run with the appropriate scripting engine.

 Portable Cron recognises the following:

 1) http://server_name:port/.../name.php   - Run PHP web pagescript on local server

 2) DriveLetter:/folders/....../name.php   - Run PHP command line script
    DriveLetter:/folders/....../name.bat   - Run CMD batch file command line script
    DriveLetter:/folders/....../name.vbs   - Run VBScript command line script
    DriveLetter:/folders/....../name.exe   - Run command line exe application

 Note: For portability a relative is specified. This is relative to folder UniServerZ
        C:\UniServerZ\home\us_cron\test_cron_1.bat  - Absolute
        \home\us_cron\test_cron_1.bat               - Relative
===========================================================================}
procedure run_path_command(path:string);
var
  HTTP: THTTPSend;         // Var for object
  AProcess: TProcess;
  USF_PHP_EXE     :string; // PHP command executable
  USF_PHP_INI_CLI :string; // PHP command line config

begin

  //--Set paths
  // UENV_PHP_SELECT - PHP version selected php56, php 70, php71, php 72 or php 73
  USF_PHP_EXE      :=  UniConPath + '\core\'+ UENV_PHP_SELECT +'\php.exe';     // PHP command executable
  USF_PHP_INI_CLI  :=  UniConPath + '\core\'+ UENV_PHP_SELECT +'\php-cli.ini'; // PHP command line config


  //===Run page on server. Server group ====
  If (Pos('http:',path) = 1) then                  // Server group
   begin                                           // Process
    HTTP   := THTTPSend.Create;                    // Create object
    try
      if HTTP.HTTPMethod('GET', path) then         // Run command
        begin
           If USC_cron_logging Then
             begin
               cron_log(' - Run OK --- ' +  path); // Add log entry Time and path
             end;
        end
      else
        begin
           If USC_cron_logging Then
             begin
                cron_log(' - Failed --- ' +  path);   // Add log entry Time and path
             end;
        end;
     finally
      HTTP.Free;
    end;
   end //---End Server group

  //===Command line group ==== 
  Else                                              //Command group
   begin                                            //Process

     //===Change relative path to absolute paths 
     If (Pos('\',path) = 1) then                   // Relative path
       path := UniConPath + path;                  // Absolute path


     //===Run exe file
     If (Pos('.exe',path) <> 0) then                //Is it a exe file
      begin                                         //Process
       //--Run command string.
       AProcess := TProcess.Create(nil);            // Create new process

       AProcess.Executable := path;                 // exe Executable
       AProcess.ShowWindow := swoHIDE;              // Hide command window
       AProcess.Execute;                            // Run command

       AProcess.Free;                               // Release process

       If USC_cron_logging Then
         begin
          cron_log(' - Run OK --- ' + path);     // Add log entry Time and path
         end;
     end;

     //===Run PHP CLI file
     If (Pos('.php',path) <> 0) then                //Is it a PHP CLI file
      begin                                         //Process
       //--Run command string.
       AProcess := TProcess.Create(nil);            // Create new process

       AProcess.Executable := USF_PHP_EXE;          // PHP Executable
       AProcess.Parameters.Add('-c');               // Use configuration file
       AProcess.Parameters.Add(USF_PHP_INI_CLI);    // Configuration file
       AProcess.Parameters.Add(path);               // Script to run

       AProcess.ShowWindow  := swoHIDE;             // Hide command window
       AProcess.Execute;                            // Run command
       AProcess.Free;                               // Release process

       If USC_cron_logging Then
         begin
            cron_log(' - Run OK --- ' + path); // Add log entry Time and path
         end;
     end;

     //===Run batch file
     If (Pos('.bat',path) <> 0) then         //Is it a batch file
      begin                                        //Process
       //--Run command string.
       AProcess := TProcess.Create(nil);     // Create new process

       AProcess.Executable := 'cmd';         // Executable to run
       AProcess.Parameters.Add('/T:B0');     //
       AProcess.Parameters.Add('/c');        // Close when finished
       AProcess.Parameters.Add('title');     // A title is required
       AProcess.Parameters.Add('USTest');    // Title
       AProcess.Parameters.Add('&&');        // Start a new command line
       AProcess.Parameters.Add(path);        // Path to batch file

       AProcess.ShowWindow  := swoHIDE;      // Hide command window
       AProcess.Execute;                     // Run command
       AProcess.Free;                        // Release process

       If USC_cron_logging Then
         begin
            cron_log(' - Run OK --- ' + path); // Add log entry Time and path
         end;
      end;

     //===Run VBScript file
     If (Pos('.vbs',path) <> 0) then           //Is it a vbs script file
      begin                                    //Process
       //--Run command string.
       AProcess := TProcess.Create(nil);       // Create new process

       AProcess.Executable := 'cscript.exe';   // Executable to run
       AProcess.Parameters.Add('/nologo');     // Remove cscript header
       AProcess.Parameters.Add(path);          // Path to script file

       AProcess.ShowWindow  := swoHIDE;        // Hide command window
       AProcess.Execute;                       // Run command
       AProcess.Free;                          // Release process

       If USC_cron_logging Then
         begin
            cron_log(' - Run OK --- ' + path); // Add log entry Time and path
         end;
     end;

   end;//End Command group

 //writeln('Run command line = '+ path);
end;
{--- End run_path_command -------------------------------------------------}

{===========================================================================
Run Cron - Main Cron procedure:
 Scan Configuration file and run configured items
 Note: Saved in sList_cron
 Note: Run from Timer1Timer
===========================================================================}
procedure run_cron;
var
 unix_time_now   :Integer;  // Current time unix string
 i               :Integer;  // Loop counter
 RegexObj        :TRegExpr; // Object
 s_year          :integer;
 s_month         :integer;
 s_day           :integer;
 s_hours         :integer;
 s_minutes       :integer;
 s_seconds       :integer;
 m_seconds       :integer;
 dt              :TDateTime;
 unix_start_time :Integer;
 offset_time_str :String;   // Period as set by user
 offset_time     :Integer;
 path            :string;   // Path to script/page to run
 ref_time        :Integer;  // Reference time tracker
 ref_time_str    :string;

begin

  //--- Get current time in Unix format
  unix_time_now := Trunc((Now - EncodeDate(1970, 1 ,1)) * 24 * 60 * 60);
  //writeln(IntToStr(unix_time_now));

  //=== Scan configuration file ===
  for i:=0 to sList_cron.Count-1 do
   begin
    If (Pos('[',sList_cron[i]) = 1) then  //Start of block
     begin                                //Process block

        //=== Start Time
        //Start time has following format: start  = 2009-09-21 2:56:52
        //Convert this to unix time. Get component parts using regex
        RegexObj := TRegExpr.Create;
        RegexObj.Expression := '^start\s*=\s*(\d*).(\d*).(\d*)\s*(\d*):(\d*):(\d*)\s*$';  //Set search pattern
        if RegexObj.Exec(sList_cron[i+1]) then        // Match found
           begin                                      // Extract parts
            s_year    := StrToInt(RegexObj.Match[1]);
            s_month   := StrToInt(RegexObj.Match[2]);
            s_day     := StrToInt(RegexObj.Match[3]);
            s_hours   := StrToInt(RegexObj.Match[4]);
            s_minutes := StrToInt(RegexObj.Match[5]);
            s_seconds := StrToInt(RegexObj.Match[6]);
           end;

        m_seconds := 0;                                    // Mili seconds not used but required for EncodeDateTime
        dt:=EncodeDateTime(s_year,s_month,s_day,s_hours,s_minutes,s_seconds,m_seconds);  //Encode to date time
        unix_start_time := DateTimeToUnix(dt);                                           //Convert date time to unix

        //=== Period
        // How often to run script from reference start time:
        // Values: hourly, daily, weekly, monthly or numeric in seconds

        offset_time := 0;                                       // Clear period.
        RegexObj.Expression := '^period\s*=\s*(\d+)\s*;?.*$';   // Set search pattern
        if RegexObj.Exec(sList_cron[i+2]) then                  // Match found is numeric (seconds)
           begin                                                // Process this
            offset_time := StrToInt(RegexObj.Match[1]);         // Convert numeric to Int
           end
        else                                                    // Not a digit hence Alpha
           begin
             RegexObj.Expression := '^period\s*=\s*([A-Za-z]+)\s*.*$';  // Set search pattern
             if RegexObj.Exec(sList_cron[i+2]) then                     // Match found is Alpha
               begin                                                    // Process this
                  offset_time_str := RegexObj.Match[1];                 // Save match
                  If (offset_time_str = 'hourly')  OR (offset_time_str = 'Hourly')  Then offset_time :=        60*60;
                  If (offset_time_str = 'daily')   OR (offset_time_str = 'Daily')   Then offset_time :=     24*60*60;
                  If (offset_time_str = 'weekly')  OR (offset_time_str = 'Weekly')  Then offset_time :=   7*24*60*60;
                  If (offset_time_str = 'monthly') OR (offset_time_str = 'Monthly') Then offset_time := 4*7*24*60*60;
               end
            end;

        //=== Path
        //Path for a web applications is full URL of the script.
        //Path for a Command-line (CLI) is either an absolute or relative path (back-slashes) including name
        path := '';                                         // Set initial value blank

        RegexObj.Expression := '^path\s*=\s*([^\s]+)\s*$';  // Set search pattern
        if RegexObj.Exec(sList_cron[i+3]) then              // Match found
          begin
             path := RegexObj.Match[1];                     // Extract and save to path
          end;

        //=== Ref marks end of a block - processing required
        ref_time      := 0;                                 // Reset ref time
        ref_time_str  := '';                                // Reset ref time string
        RegexObj.Expression := '^ref\s*=\s*(\d+)\s*;?.*$';  // Set search pattern

        if RegexObj.Exec(sList_cron[i+4]) then              // Match found is numeric (seconds)
           begin                                            // Process this
             ref_time_str := RegexObj.Match[1];             // Save string
             ref_time     := StrToInt(RegexObj.Match[1]);   // Convert to Int
           end ;
         RegexObj.Free; // release object


         // Reference time if blank indicates first run.
         // While a value indicates it has previously ran and is a repeat time.

         If (ref_time_str = '') Then                                                      // First run
           begin
            If (unix_start_time < unix_time_now) Then                                     // Do we need to run it
              begin
                sList_cron[i+4] := 'ref    = ' + IntToStr(unix_time_now-1 + offset_time); // Yes: Save new ref time to list
                sList_cron.SaveToFile(USF_CRON_INI);                                      // Save new values to file                                                   // Save new values
                run_path_command(path);                                                   // Run command line
              end;
             end
         Else                                                                             // Already triggered hence repeat time
           begin
            If (ref_time < unix_time_now) Then                                            // Do we need to run it
              begin
                sList_cron[i+4] := 'ref    = ' +  IntToStr(unix_time_now-1 + offset_time);// Yes: Save new ref time to list
                sList_cron.SaveToFile(USF_CRON_INI);                                      // Save new values to file
                run_path_command(path);                                                   // Run command line
              end;
           end;
     end;//End process block
   end;//End scan list
end;
{---End run_cron ------------------------------------------------------------}


{$R *.lfm}

{ Tcron }

procedure Tcron.Btn_edit_configClick(Sender: TObject);
begin
  us_display_in_editor(USF_CRON_INI);
end;

procedure Tcron.Btn_view_logClick(Sender: TObject);
begin
  us_display_in_editor(USF_CRON_LOG);
end;

procedure Tcron.Button1Click(Sender: TObject);
begin
  run_cron;
end;


procedure Tcron.CB_enable_run_cronClick(Sender: TObject);
begin
  // Check box enable/disable cron.
  // Checked   = Cron enabled
  // UnChecked = Cron disabled

IF FileExists(USF_US_CONF_INI) Then
  begin
   If (CB_enable_cron.Checked ) Then  //==Enable Cron
     begin
       us_ini_set(USF_US_CONF_INI,'CRON','enable_cron','true');   //Set value in ini configuration file
       USC_enable_cron := true;

       //-- Log cron Start
       If USC_cron_logging Then
         cron_log('### Log Start ===========================================');

     end
   Else   //==Disable Cron
     begin
       us_ini_set(USF_US_CONF_INI,'CRON','enable_cron','false'); //Set value in ini configuration file
       USC_enable_cron := false;

       If USC_cron_logging Then
         begin
           cron_log('### Log End =============================================');
           cron_log(''); //Add blank line
         end;

     end;
  end;
end;

procedure Tcron.CB_enable_cron_loggingClick(Sender: TObject);
begin
// Check box enable/disable cron logging.
// Checked   = Cron enabled
// UnChecked = Cron disabled

IF FileExists(USF_US_CONF_INI) Then
begin
 If (CB_enable_cron_logging.Checked ) Then  //==Enable Cron logging
   begin
     us_ini_set(USF_US_CONF_INI,'CRON','cron_logging','true');   //Set value in ini configuration file
     USC_cron_logging := true;
   end
 Else   //==Disable  Cron logging
   begin
     us_ini_set(USF_US_CONF_INI,'CRON','cron_logging','false'); //Set value in ini configuration file
     USC_cron_logging := False;
   end;
end;
end;

procedure Tcron.FormCreate(Sender: TObject);
begin
  //Cron check box
  If USC_enable_cron Then
     CB_enable_cron.state := cbChecked     // Cron enabled
  Else
     CB_enable_cron.State := cbUnchecked;  // Cron disabled

  //Cron logging check box
  If USC_cron_logging Then
     CB_enable_cron_logging.state := cbChecked     // Cron logging enabled
  Else
     CB_enable_cron_logging.State := cbUnchecked;  // Cron logging disabled


  //--Read cron configuration file
  sList_cron  := TStringList.Create;         // Create object
  If Fileexists(USF_CRON_INI) Then
  sList_cron.LoadFromFile(USF_CRON_INI);     // Load cron configuration file

                                             // Note: Free when app terminates

end;
end.

