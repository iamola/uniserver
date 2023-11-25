unit message_dlg_form;

{#############################################################################
'# Name: message_dlg_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons,math;

type

  { Tus_message_dlg }

  Tus_message_dlg = class(TForm)
    Btn_yes: TBitBtn;
    Btn_no: TBitBtn;
    Btn_ok: TBitBtn;
    img_error_icon: TImage;
    img_information_icon: TImage;
    Label_message: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    img_question_icon: TImage;
    img_warning_icon: TImage;
    Panel1: TPanel;
    Panel4: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    us_heading :string;  // Heading displayed in window title box (caption)
    us_message:string;   // Formated string to display
    us_image:string;     // Name of image to display
    us_buttons:string;   // Name of buttons to display
  end;

var
  us_message_dlg: Tus_message_dlg;

implementation

{$R *.lfm}

{ Tus_message_dlg }

procedure Tus_message_dlg.FormShow(Sender: TObject);
var
   caption_width :Integer;
begin
  //==Display Title and Message
   self.Caption := us_heading; // Display title
   Label_message.Caption  := us_message; // Display message

   //===Try to fit heading text into caption if main text is smaller
   //The 1.4 multiplier is a fiddle factor but should work!
   //It takes into account border sizes.
   caption_width := Ceil(1.4* self.Canvas.TextWidth(us_heading));
    If (self.Width < caption_width) Then
     begin
      Panel4.Width :=caption_width; //Expands window
     end;

  //=====Initially hide all Images and Panel1
  //Message text area expands to fill form window
   img_error_icon.Visible:=False;
   img_information_icon.Visible:=False;
   img_question_icon.Visible:=False;
   img_warning_icon.Visible:=False;
   Panel1.Visible := False;

   //===Display Images and panel1
   //From parameters passed to form from function us_MessageDlg

   If us_image = 'Information'Then
    begin
      Panel1.Visible := True;         //Expands left side of window
      img_information_icon.Visible:=True; //Display images
    end;

   If us_image = 'Confirmation'Then
    begin
     Panel1.Visible := True;
     img_question_icon.Visible:=True;
    end;

   If us_image = 'Error'Then
    begin
     Panel1.Visible := True;
     img_error_icon.Visible:=True;
    end;

   If us_image = 'Warning'Then
    begin
     Panel1.Visible := True;
     img_warning_icon.Visible:=True;
    end;

   //Position and display buttons
   //The left side and right client area will have expanded to fit image and message text.
   If us_buttons = 'Ok' Then
    begin
      Btn_ok.Left := ((self.ClientWidth - 100) div 2);
      self.Btn_ok.Visible:= True
    end
   Else
     begin
       self.Btn_ok.Visible:= False;
     end;

   If us_buttons = 'YesNo' Then
    begin
     Btn_yes.Left := ((self.ClientWidth - 205) div 2);
     Btn_no.Left := Btn_yes.Left + 105;

     self.Btn_no.Visible:= True;
     self.Btn_yes.Visible:= True;
    end
   Else
    begin
      self.Btn_no.Visible:= False;
      self.Btn_yes.Visible:= False;
    end;

   //The form will have been resized. Use this to set form center of screen
   self.left := (screen.width - self.width) div 2;
   self.top  := (screen.height - self.height) div 2;
end;

procedure Tus_message_dlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree; //Force close and free memory
end;

end.

