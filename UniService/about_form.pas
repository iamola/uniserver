unit about_form;

{#############################################################################
'# Name: about_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: http://www.uniformserver.com
'# Mike Gleaves V1.0.4 25-04-2014
'# Sudeep D'Souza V2.3.0 02-10-2019
'#
'# About UniService
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

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
  str1:= str1 + 'UniServer Zero XV '+sLineBreak;
  str1:= str1 + 'UniService XV 2.5.0';
  Label1.Caption := str1;

  str2:='';
  str2:= str2 + 'UniService coded in Pascal and compiled with Lazarus' +sLineBreak;
  str2:= str2 + 'Product: Uniform Server Zero XV ' +sLineBreak;
  str2:= str2 + 'Release status: UniService XV version 2.5.0'+sLineBreak+sLineBreak;

  str2 := str2 + 'People who have contributed to Uniform Server:' + sLineBreak;
  str2 := str2 + 'Developers:'  +sLineBreak;
  str2 := str2 + '- Olajide Olaolorun (olajideolaolorun)' +sLineBreak;
  str2 := str2 + '- Mike Gleaves (Ric)' +sLineBreak;
  str2 := str2 + '- Bob Strand (BobS)'+sLineBreak;
  str2 := str2 + '- Sudeep DSouza (SudeepJD)'+sLineBreak;
  str2 := str2 + '- Davide Bonsangue (BrainStorm)'+sLineBreak;
  str2 := str2 + '- Sylvain Bourdon (sbourdon)'+sLineBreak+sLineBreak;
 Label2.Caption := str2;
end;

end.

