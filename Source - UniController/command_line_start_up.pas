unit command_line_start_up;

{#############################################################################
'# Name: command_line_start_up.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#
'#
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,Dialogs,LCLType,
  default_config_vars,
  us_common_procedures,
  windows;

procedure us_command_line_start_up; //UniController started with parameters

implementation

uses
  main_unit;
{****************************************************************************
 us_command_line_start_up:
  This procedure starts and stops the servers via UniController command line
  parameters. A single parameter is passed to UniController this is executed
  and UniController is terminated.
  Note: A special command line parameter "pc_win_start" is covered seperately
        in section TMain.FormCreate
  Note: It is assumed the servers have been configured and are run-able.

  Parameters implemented by this procedure:
   start_apache // Start Apache server
   stop_apache  // Stop Apache server
   start_mysql  // Start MySQL server
   stop_mysql   // Stop MySQL server
   start_both   // Starts Apache and MySQL servers
   stop_both    // Stops Apache and  MySQL servers
============================================================================}
procedure us_command_line_start_up;
begin
  //Start Apache
  If (ParamCount >= 1) and (ParamStr(1) = 'start_apache') Then
   begin
     Main.visible:=False;    // Hide form
     us_main_init; // Set initial values for variables, paths and environment variables.
     sleep(1000);
     us_start_apache_program;  // Start Apache
     Application.Terminate;    // Kill UniController, exit application
   end;

  //Stop Apache
  If (ParamCount >= 1) and (ParamStr(1) = 'stop_apache') Then
   begin
     Main.visible:=False;    // Hide form
     us_main_init; // Set initial values for variables, paths and environment variables.
     sleep(1000);
     us_kill_apache_program;   // Stop Apache
     Application.Terminate;    // Kill UniController, exit application
   end;

  //Start MySQL
  If (ParamCount >= 1) and (ParamStr(1) = 'start_mysql') Then
   begin
     Main.visible:=False;    // Hide form
     us_main_init; // Set initial values for variables, paths and environment variables.
     sleep(1000);
     us_start_mysql_program; // Start MySQL
     Application.Terminate;  // Kill UniController, exit application
   end;

  //Stop MySQL
  If (ParamCount >= 1) and (ParamStr(1) = 'stop_mysql') Then
   begin
     Main.visible:=False;    // Hide form
     us_main_init; // Set initial values for variables, paths and environment variables.
     sleep(1000);
     us_clean_stop_mysql_program; // Stop MySQL
     Application.Terminate;       // Kill UniController, exit application
   end;

  //Start Both
  If (ParamCount >= 1) and (ParamStr(1) = 'start_both') Then
   begin
     Main.visible:=False;    // Hide form
     us_main_init; // Set initial values for variables, paths and environment variables.
     sleep(1000);
     us_start_apache_program;  // Start Apache
     sleep(1000);
     us_start_mysql_program;   // Start MySQL
     Application.Terminate;    // Kill UniController, exit application
   end;

  //Stop Both
  If (ParamCount >= 1) and (ParamStr(1) = 'stop_both') Then
   begin
     Main.visible:=False;    // Hide form
     us_main_init; // Set initial values for variables, paths and environment variables.
     sleep(1000);
     us_kill_apache_program;      // Stop Apache
     sleep(1000);
     us_clean_stop_mysql_program; // Stop MySQL
     Application.Terminate;       // Kill UniController, exit application
   end;
end;
{--End us_command_line_start_up ---------------------------------------------}
end.

