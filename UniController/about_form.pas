unit about_form;

{#############################################################################
'# Name: about_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.1.1 25-04-2014
'#
'#
'#############################################################################}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, default_config_vars;

type

  { TAbout }

  TAbout = class(TForm)
    Bevel1: TBevel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  About: TAbout;

implementation

{$R *.lfm}

{ TAbout }

procedure TAbout.FormCreate(Sender: TObject);
var
  str1:string;
  str2:string;
begin
   about.Caption := 'About';     // About

  str1:='';
  str1:= str1 + 'UniServer Zero XIV '+ USC_AppVersion +sLineBreak;
  str1:= str1 + 'UniController XIV ' + UNICONTROLLER_VERSION;
  Label1.Caption := str1;

  str2:='';
  str2:= str2 + 'UniController coded in Pascal and compiled with Lazarus 2.0.2' +sLineBreak;
  str2:= str2 + 'Product: Uniform Server Zero XIV ' +sLineBreak;
  str2:= str2 + 'Release status: UniController XIV version '+UNICONTROLLER_VERSION +sLineBreak+sLineBreak;
  str2 := str2 + 'People who have contributed to Uniform Server:' + sLineBreak;
  str2 := str2 + 'Developers:'  +sLineBreak;
  str2 := str2 + '- Olajide Olaolorun (olajideolaolorun)' +sLineBreak;
  str2 := str2 + '- Mike Gleaves (Ric)' +sLineBreak;
  str2 := str2 + '- Bob Strand (BobS)'+sLineBreak;
  str2 := str2 + '- Sudeep DSouza (SudeepJD)'+sLineBreak+sLineBreak;
  Label2.Caption := str2;
end;

end.

