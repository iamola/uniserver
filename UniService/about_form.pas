unit about_form;

{#############################################################################
'# Name: about_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
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
  str1:= str1 + 'UniService XV 2.5.2';
  Label1.Caption := str1;

  str2:='';
  str2:= str2 + 'UniService coded in Pascal and compiled with Lazarus' +sLineBreak+sLineBreak;

  str2 := str2 + 'People / developers who have contributed to Uniform Server:' + sLineBreak;
  str2 := str2 + '- Olajide Olaolorun (olajideolaolorun)' +sLineBreak;
  str2 := str2 + '- Mike Gleaves (Ric)' +sLineBreak;
  str2 := str2 + '- Bob Strand (BobS)'+sLineBreak;
  str2 := str2 + '- Sudeep DSouza (SudeepJD)'+sLineBreak;
  str2 := str2 + '- Davide Bonsangue (BrainStorm)'+sLineBreak;
  str2 := str2 + '- Sylvain Bourdon (sbourdon)'+sLineBreak+sLineBreak;
 Label2.Caption := str2;
end;

end.

