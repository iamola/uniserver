unit server_internet_status_form;

{#############################################################################
'# Name: server_internet_status_form.pas
'# Developed By: The Uniform Server Development Team
'# Web: https://www.uniformserver.com
'#############################################################################}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, us_common_functions, default_config_vars,
  windows,
  httpsend;

type

  { TMyThread1 }                                 //Thread to obtain IP address
   TMyThread1 = class(TThread)
   private
     str1:string;                                 //Ip addess obtained
     procedure proc1;                             //Procedure to use for synchronizing
   protected
     procedure Execute; override;
   public
     constructor Create(CreateSuspended: boolean);
   end;

   { TMyThread2 }                                 //Thread to check server accesibility
   TMyThread2 = class(TThread)
   private
     str2: string;                                //Status obtained
     procedure proc2;                             //Procedure to use for synchronizing
   protected
     procedure Execute; override;
   public
     constructor Create(CreateSuspended: boolean);
   end;

   { TMyThread3 }                                 //Thread to obtain current server version
   TMyThread3 = class(TThread)
   private
     str3: string;                                //Version obtained
     procedure proc3;                             //Procedure to use for synchronizing
   protected
     procedure Execute; override;
   public
     constructor Create(CreateSuspended: boolean);
   end;


  { Tserver_internet_status }

  Tserver_internet_status = class(TForm)
    Bevel1: TBevel;
    DividerBevel1: TDividerBevel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Lbl_ip_address_a: TLabel;
    Lbl_ip_address_b: TLabel;
    Lbl_server_accessible_a: TLabel;
    Lbl_server_accessible_b: TLabel;
    Lbl_server_version_a: TLabel;
    Lbl_server_version_b: TLabel;
    Lbl_this_server_version_a: TLabel;
    Lbl_this_server_version_b: TLabel;
    ProgressBar_acc: TProgressBar;
    ProgressBar_ip: TProgressBar;
    ProgressBar_ver: TProgressBar;
    procedure FormShow(Sender: TObject);

  private
    { private declarations }
    mThread1: TThread; //Threads
    mThread2: TThread;
    mThread3: TThread;
  public
    { public declarations }
  end;

var
  server_internet_status: Tserver_internet_status;

implementation

{$R *.lfm}


procedure TMyThread1.proc1;                  //Procedure used for synchronizing
begin
  server_internet_status.Lbl_ip_address_b.visible :=True;   //Show label
  server_internet_status.ProgressBar_ip.visible   :=False;  //Hide progress bar
  server_internet_status.Lbl_ip_address_b.Caption:=str1;    //Display result for IP address
  server_internet_status.mThread1.Terminate;                //Signal end of thread
end;


procedure TMyThread2.proc2;                   //Procedure used for synchronizing
begin
  server_internet_status.Lbl_server_accessible_b.visible :=True;   //Show label
  server_internet_status.ProgressBar_acc.visible         :=False;  //Hide progress bar
  server_internet_status.Lbl_server_accessible_b.Caption:=str2    ;//Display result for accessibility
  server_internet_status.mThread2.Terminate;                       //Signal end of thread
end;

procedure TMyThread3.proc3;                   //Procedure used for synchronizing
begin
  server_internet_status.Lbl_server_version_b.visible :=True;  //Show label
  server_internet_status.ProgressBar_ver.visible :=False;      //Hide progress bar
  server_internet_status.Lbl_server_version_b.Caption:=str3;   //Display result for current version
  server_internet_status.mThread3.Terminate;                   //Signal end of thread
end;

procedure TMyThread1.Execute;                //Run thread
var
  ip:string;
begin
 ip:='';
 while (not Terminated) and (true) do
  begin
    //IP address
    If get_ip_address(ip)
    Then str1 := ip
    Else str1 := 'Unable to obtain ip information';

    Synchronize( @proc1 );
    sleep(1000);
  end;
end;

procedure TMyThread2.Execute;                //Run thread
begin
  while (not Terminated) and (true) do
   begin
     //Server accessible
     If server_accessible_from_internet
     Then str2 := 'Accessible from internet'
     Else str2 := 'Not accessible from internet';

     Synchronize( @proc2 );
     sleep(1000);
   end;
end;

procedure TMyThread3.Execute;                //Run thread
var
   version:string;
begin
  version:='';
  while (not Terminated) and (true) do
   begin
     //Current server verion
     If get_version_information(version)
     Then str3 := version
     Else str3 := 'Unable to obtain version information';

     Synchronize( @proc3 );
     sleep(1000);
   end;
end;

constructor TMyThread1.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

 constructor TMyThread2.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

 constructor TMyThread3.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;




{ Tserver_internet_status }

procedure Tserver_internet_status.FormShow(Sender: TObject);
var
  str     :string;
begin
   str:='';
   str:= str + 'UniServer Zero XV' +sLineBreak;
   str:= str + 'UniController ' + UNICONTROLLER_VERSION;
   Label1.Caption := str;

   //This server version
   Lbl_this_server_version_b.Caption := USC_AppVersion;

   //Hide labels
   Lbl_ip_address_b.visible :=False;
   Lbl_server_accessible_b.visible :=False;
   Lbl_server_version_b.visible :=False;

   //Show progress bars
   ProgressBar_ip.visible :=True;
   ProgressBar_acc.visible :=True;
   ProgressBar_ver.visible :=True;

   //Progress bars
   ProgressBar_ip.Style  :=pbstMarquee;  //Start progress bar
   ProgressBar_acc.Style :=pbstMarquee;  //Start progress bar
   ProgressBar_ver.Style :=pbstMarquee;  //Start progress bar

   //Start threads
   mThread1 := TMyThread1.Create(false); //Create and run thread
   mThread2 := TMyThread2.Create(false); //Create and run thread
   mThread3 := TMyThread3.Create(false); //Create and run thread

end;



end.

