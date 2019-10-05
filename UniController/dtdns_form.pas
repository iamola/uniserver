unit dtdns_form;

{#############################################################################
'# Name: dtdns_form.pas
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
  ExtCtrls,
  default_config_vars,
  us_common_procedures,
  us_common_functions,
  INIFiles,
  httpsend,
  blcksock;

//Common function
function dtdns_perform_update:boolean; //Perform update at DtDns

type

  { Tdtdns }

  Tdtdns = class(TForm)
    Btn_save_changes: TButton;
    Btn_force_update: TButton;
    Btn_help_accounts: TButton;
    Btn_view_log: TButton;
    Btn_auto_run_help: TButton;
    Btn_force_refresh: TButton;
    CB2_enable_logging: TCheckBox;
    CB1_auto_run: TCheckBox;
    domain2_status: TPanel;
    domain3_status: TPanel;
    domain4_status: TPanel;
    domain5_status: TPanel;
    edit_domain_name_1: TEdit;
    edit_domain_name_2: TEdit;
    edit_domain_name_3: TEdit;
    edit_domain_name_4: TEdit;
    edit_domain_name_5: TEdit;
    edit_password: TEdit;
    GB1: TGroupBox;
    GB3: TGroupBox;
    GB2: TGroupBox;
    lbl_domain_name_1: TLabel;
    lbl_domain_name_2: TLabel;
    lbl_domain_name_3: TLabel;
    lbl_domain_name_4: TLabel;
    lbl_domain_name_5: TLabel;
    lbl_password: TLabel;
    domain1_status: TPanel;
    procedure Btn_auto_run_helpClick(Sender: TObject);
    procedure Btn_force_refreshClick(Sender: TObject);
    procedure Btn_force_updateClick(Sender: TObject);
    procedure Btn_help_accountsClick(Sender: TObject);
    procedure Btn_save_changesClick(Sender: TObject);
    procedure Btn_view_logClick(Sender: TObject);
    procedure CB1_auto_runClick(Sender: TObject);
    procedure CB2_enable_loggingClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dtdns: Tdtdns;
  dtdns_ret :boolean = False;  // Assume IP unobtained
  DTDNS_wait :boolean = False; // Reset wait
implementation

{$R *.lfm}

{===========================================================================
DtDNS validate:
 Check for account password and-or at least one domain name
 Return True  - valid
        False - invalid 
===========================================================================}
function dtdns_validate:boolean;
begin
   //Check for password and at least one domain name
   If (  not (dtdns.edit_domain_name_1.Text = '')
      or not (dtdns.edit_domain_name_2.Text = '')
      or not (dtdns.edit_domain_name_3.Text = '')
      or not (dtdns.edit_domain_name_4.Text = '')
      or not (dtdns.edit_domain_name_5.Text = ''))
     and not (dtdns.edit_password.Text = '') Then
    dtdns_validate := true     // Domain name and password.
   Else
    dtdns_validate := false;   // Invalid No domain name and/or no password 
end;
{--- End dtdns_validate ---------------------------------------------------}


{===========================================================================
DtDNS Logging
 Logs DtDNS update actions to a log file
 Input:             String to be logged
 USF_DTDNS_LOG      Path to log file including file name
===========================================================================}
procedure dtdns_log(str_ip:string);
var
  FileVar1 :System.Text;   // File handle note use of System
  new_str  :String;        // Text string to log
  time_now :string;
begin

  //Set time now or blank line
  If (str_ip ='') Then
   begin
    new_str  := '';                      //Blank line
   end
  Else
   begin
     //Time now
     time_now := FormatDateTime('dd/MM/YYYY HH:MM:SS', Now);
     new_str  := time_now +' ' + str_ip; //Date, time and string to log
   end;


  //Check file exists. If file does not exist, create it.
  If not FileExists(USF_DTDNS_LOG) Then
    begin
      System.Assign(FileVar1,USF_DTDNS_LOG); // Assign file
      Rewrite(FileVar1);                     // Create the file
      CloseFile(FileVar1);                   // Close file
      dtdns.Btn_view_log.Enabled := True;    // Enable view-log button
    end;

  //Add line to log
  System.Assign(FileVar1,USF_DTDNS_LOG);  // Assign file (create handle)
  Append(FileVar1);                       // opens existing file for appending
  writeln(FileVar1,new_str);              // Write line adds line to end of file
  CloseFile(FileVar1);                    // Release handle
end;
{---End dtdns_log ---------------------------------------------------------}

{===========================================================================
Update a domain name on the DtDNS server.

Log results if appropriate log returned error code from DtDNS server

 Input: Domain name
        Account password
===========================================================================}
procedure update_and_log(DomainName:string;password:string;ip:string);
 var
   HTTP: THTTPSend;       // Var for object
   sList: tstringlist;    // Storage for returned page
   str:string;
begin

  //Create requst
  str := 'http://www.dtdns.com/api/autodns.cfm?id='+DomainName+'&pw='+password+'&ip='+ip;

  HTTP   := THTTPSend.Create;    // Create object
  sList  := TStringList.create;  // Create list storage

  try
    if not HTTP.HTTPMethod('GET', str) then
      begin
        If DTDNS_logging Then
          begin
           dtdns_log(' - ERROR '+ IntToStr(Http.Resultcode)); //Log error
          end;
      end
    else
      begin
         sList.loadfromstream(Http.Document); //OK Get returned message
         //Add log entry Time, Domain name and returned message
         If DTDNS_logging Then
           begin
             dtdns_log(' - ' + ' '+sList[0]);
           end;
      end;
  finally
    HTTP.Free;
    sList.free;
  end;

end;
{---End update_and_log -------------------------------------------------------}

{==============================================================================
Check domain IPs at DtDNS and update indicators.
 1) Get your IP as seen from internet.
 2) Check this against configured IPs at DtDNS for each domain name
 3) Update indicators to reflect current state.

 Output: Returns
         True  - Your IP address was obtained.
         False - Unable to get your IP as seen from internet.   

------------------------------------------------------------------------------}
function check_ips_update_indicators:boolean;
var
    HTTP             :THTTPSend;     // Var object
    us_web_page_data :boolean;       // Data received flag True: Data received
    us_web_ip        :string;        // Ip address as seen from the Internet
    sList            :tstringlist;   // Storage for returned page
    oBlockSocket     :TBlockSocket;  // Synapse sockets uses: blcksock
    ret              :boolean;       // Return state
begin
     ret := False; //Assume IP as seen from internet not obtained

     //=== Get Ip address as seen fom Internet ===========================
     us_web_page_data := False;     // Assume no reply
     HTTP   := THTTPSend.Create;    // Create object
     sList  := TStringList.create;  // Create list storage
     try
         HTTP.Timeout := 5000;      //200 Defaults to 5000
         if HTTP.HTTPMethod('GET', 'http://myip.dtdns.com') then
         begin
            us_web_page_data := True;            // Received reply
            ret := True;                         // IP obtained
            sList.loadfromstream(Http.Document); // Save data in sList
         end;
     finally
       HTTP.Free;
     end;

     //==Update Indicators
     If us_web_page_data Then            // Must have reference IP to check against.

     begin                               // Update IP as required
         //Get IP address
         us_web_ip := sList[0];          //Ip address is first line in list

         //Create object
         oBlockSocket := TBlockSocket.create;

         //== Update domain1
         If not (DTDNS_domain_name_1 = '') Then
           begin
             If ( oBlockSocket.ResolveName(DTDNS_domain_name_1) = us_web_ip) Then
               begin // ips Identical
                 dtdns.domain1_status.Color := clLime;
               end
             Else   // ips different
               begin
                 dtdns.domain1_status.Color := clRed;
                 dtdns.Btn_force_update.Enabled  := True; //Enable force update
               end;
           end
         Else
          begin
            dtdns.domain1_status.Color := clSilver;
          end;

         //== Update domain2
         If not (DTDNS_domain_name_2 = '') Then
           begin
             If (oBlockSocket.ResolveName(DTDNS_domain_name_2) = us_web_ip) Then
                 dtdns.domain2_status.Color := clLime     // ips Identical
             Else   // ips different
               begin
                 dtdns.domain2_status.Color := clRed;
                 dtdns.Btn_force_update.Enabled  := True; //Enable force update
               end;
           end
         Else
            dtdns.domain2_status.Color := clSilver;

         //== Update domain3
         If not (DTDNS_domain_name_3 = '') Then
           begin
             If (oBlockSocket.ResolveName(DTDNS_domain_name_3) = us_web_ip) Then
                 dtdns.domain3_status.Color := clLime // ips Identical
             Else   // ips different
               begin
                 dtdns.domain3_status.Color := clRed;
                 dtdns.Btn_force_update.Enabled  := True; //Enable force update
               end;
           end
         Else
            dtdns.domain3_status.Color := clSilver;
 
          //== Update domain4
         If not (DTDNS_domain_name_4 = '') Then
           begin
             If (oBlockSocket.ResolveName(DTDNS_domain_name_4) = us_web_ip) Then
                 dtdns.domain4_status.Color := clLime // ips Identical
             Else   // ips different
               begin
                 dtdns.domain4_status.Color := clRed;
                 dtdns.Btn_force_update.Enabled  := True; //Enable force update
               end;
           end
         Else
            dtdns.domain4_status.Color := clSilver;

          //== Update domain5
         If not (DTDNS_domain_name_5 = '') Then
           begin
             If (oBlockSocket.ResolveName(DTDNS_domain_name_5) = us_web_ip) Then
                 dtdns.domain5_status.Color := clLime // ips Identical
             Else   // ips different
               begin
                 dtdns.domain5_status.Color := clRed;
                 dtdns.Btn_force_update.Enabled  := True; //Enable force update
               end;
           end
         Else
            dtdns.domain5_status.Color := clSilver;

         //Remove object
         oBlockSocket.free;
       end;

     //-Clean up
     sList.free;              // Free List

     check_ips_update_indicators := ret; // Return IP state
end;

{---End check_ips_update_indicators----------------------------------------}

{===========================================================================
DtDNS perform update:
 This function is run either via cron or manually.
 It updates Domain-name IP addresses at DtDNS.
 Indicators on the dtdns_form are set as appropriate.

 If run manuuly messages are reported to user.
 If run by cron messages are suppressed.

 Output: Returns
         True  - Your IP address was obtained and Update initiated.
         False - Unable to get your IP as seen from internet. 

===========================================================================}
function dtdns_perform_update:boolean;
var
  unix_time_now   :Integer;      // Unix time now
  str             :string;       // Temp string

  HTTP            :THTTPSend;    // Var object
  us_web_page_data:boolean;      // Data received flag True: Data received
  us_web_ip       :string;       // Ip address as seen from the Internet
  sList           :tstringlist;  // Storage for returned page
  oBlockSocket    :TBlockSocket; // Synapse sockets uses: blcksock
  TimRemain       :Integer;      //Time remaining
begin

  // DtDNS requires a 10 Min delay between updates. This is catered for
  // using a time tracker variable DTDNS_time_tracker
  // This tracker is checked before performing an update.

  unix_time_now   := Trunc((Now - EncodeDate(1970, 1 ,1)) * 24 * 60 * 60); // Get current time

  If (unix_time_now > DTDNS_time_tracker) Then        // Time expired perform update
    begin
      DTDNS_time_tracker := unix_time_now + (60*10); // Update time tracker by adding 10Mins
      DTDNS_wait         := False;                   // Reset wait

      //------------------------------------------------------------------------
      dtdns_log('### Log Start ===========================================');

      //=== Get Ip address as seen fom Internet ===========================
     us_web_page_data := False;     // Assume no reply
     HTTP   := THTTPSend.Create;    // Create object
     sList  := TStringList.create;  // Create list storage
     try
         HTTP.Timeout := 5000;      // 200 Defaults to 5000
         if HTTP.HTTPMethod('GET', 'http://myip.dtdns.com') then
          begin
            DTDNS_time_tracker := unix_time_now + (60*10); // Update time tracker by adding 10Mins
            us_web_page_data := True;                      // Received reply
            dtdns_ret := True;                             // IP obtained
            sList.loadfromstream(Http.Document);           // Save data in sList
          end
         Else
           begin
             DTDNS_time_tracker := unix_time_now + 1; // No IP reset time tracker
             dtdns_ret := False;                      // IP unobtained
           end;
      finally
       HTTP.Free;
     end;

     If us_web_page_data Then        // We have received IP as seen from internet

     //--Start Update
     begin                          // Extract IP address
       us_web_ip := sList[0];       // Ip address is first line in list
       dtdns_log(' - Your IP address is ' +  us_web_ip); // Add log entry Time and new ip


    //===Create object
    oBlockSocket := TBlockSocket.create;     // Create socket object for resolving domain names

    //=== Update domain1
    If not (DTDNS_domain_name_1 = '') Then   // If domain name set check its IP against new ref ip address.
     begin                                                                       // Is an update required?
       If not ( oBlockSocket.ResolveName(DTDNS_domain_name_1) = us_web_ip) Then  // IPs not identical
         begin                                                                   // Update required
           dtdns.domain1_status.Color := clRed;                                  // Set red different
           update_and_log(DTDNS_domain_name_1,DTDNS_account_password,us_web_ip); // Update
         end
         Else // IPs identical
           begin
             dtdns.domain1_status.Color := clLime; //Set green OK
             If DTDNS_logging Then
               begin
                 dtdns_log('   ' + DTDNS_domain_name_1 + ' - IP address up to date.'); // Add log entry
               end;
           end;
     end
    Else // DTDNS domain name = ''
      dtdns.domain1_status.Color := clSilver;

    //=== Update domain2
    If not (DTDNS_domain_name_2 = '') Then   // If domain name set check its IP against new ref ip address.
      begin                                                                      // Is an update required?
       If not ( oBlockSocket.ResolveName(DTDNS_domain_name_2) = us_web_ip) Then  // IPs not identical
         begin                                                                   // Update required
           dtdns.domain2_status.Color := clRed;                                  // Set red different
           update_and_log(DTDNS_domain_name_2,DTDNS_account_password,us_web_ip); // Update
         end
         Else // IPs identical
           begin
             dtdns.domain2_status.Color := clLime;     //Set green OK
             If DTDNS_logging Then
               begin
                 dtdns_log('   ' + DTDNS_domain_name_2 + ' - IP address up to date.'); // Add log entry
               end;
           end;
      end
    Else // DTDNS domain name = ''
      dtdns.domain2_status.Color := clSilver;

    //Update domain3
    If not (DTDNS_domain_name_3 = '') Then   // If domain name set check its IP against new ref ip address.
     begin                                                                       // Is an update required?
       If not ( oBlockSocket.ResolveName(DTDNS_domain_name_3) = us_web_ip) Then  // IPs not identical
         begin                                                                   // Update required
           dtdns.domain3_status.Color := clRed;                                  // Set red different
           update_and_log(DTDNS_domain_name_3,DTDNS_account_password,us_web_ip); // Update
         end
         Else // IPs identical
           begin
             dtdns.domain3_status.Color := clLime;     //Set green OK
             If DTDNS_logging Then
               begin
                 dtdns_log('   ' + DTDNS_domain_name_3 + ' - IP address up to date.'); // Add log entry
               end;
           end;
     end
    Else // DTDNS domain name = ''
      dtdns.domain3_status.Color := clSilver;

    //Update domain4
    If not (DTDNS_domain_name_4 = '') Then   // If domain name set check its IP against new ref ip address.
     begin                                                                       // Is an update required?
       If not ( oBlockSocket.ResolveName(DTDNS_domain_name_4) = us_web_ip) Then  // IPs not identical
         begin                                                                   // Update required
         dtdns.domain4_status.Color := clRed;                                    // Set red different
         update_and_log(DTDNS_domain_name_4,DTDNS_account_password,us_web_ip);   // Update
         end
         Else // IPs identical
           begin
             dtdns.domain4_status.Color := clLime;     //Set green OK
             If DTDNS_logging Then
               begin
                 dtdns_log('   ' + DTDNS_domain_name_4 + ' - IP address up to date.'); // Add log entry
               end;
           end;
     end
    Else // DTDNS domain name = ''
      dtdns.domain4_status.Color := clSilver;

    //Update domain5
    If not (DTDNS_domain_name_5 = '') Then   // If domain name set check its IP against new ref ip address.
     begin                                                                       // Is an update required?
       If not ( oBlockSocket.ResolveName(DTDNS_domain_name_5) = us_web_ip) Then  // IPs not identical
         begin                                                                   // Update required
           dtdns.domain5_status.Color := clRed;                                  // Set red different
           update_and_log(DTDNS_domain_name_5,DTDNS_account_password,us_web_ip); // Update
         end
         Else // IPs identical
           begin
             dtdns.domain5_status.Color := clLime;     //Set green OK
             If DTDNS_logging Then
               begin
                 dtdns_log('   ' + DTDNS_domain_name_5 + ' - IP address up to date.'); // Add log entry
               end;
           end;
       end
    Else // DTDNS domain name = ''
      dtdns.domain5_status.Color := clSilver;

     //Remove object
     oBlockSocket.free;

     //---End update
     end

     Else      // Failed to get IP Address give up
       
       begin
         dtdns_log(' - Failed To get IP address as seen from Internet');   // Add log entry
       end;
     //-Clean up
     sList.free;              // Free List

     dtdns_log('### Log End =============================================');
     dtdns_log(''); //Blank line
    //------------------------------------------------------------------------

    //Inform user
    If Not DTDNS_auto_run Then
       us_MessageDlg('DtDNS Info','Performing update', mtcustom,[mbOk],0) ; //Display information message

    end
  Else         //Still procesing previous force update.
    begin
      DTDNS_wait         := True;    // Set wait

      If Not DTDNS_auto_run Then
        begin
         check_ips_update_indicators; //Now perform a check and indicator update
         TimRemain := DTDNS_time_tracker - unix_time_now; //Calculate time remaining

         //str_dtdns_update2_title     = Force Update
         str:='';
         str := str + 'Waiting for statutory 10 minutes delay from previous force update. ' + sLineBreak+ sLineBreak;
         str := str + 'Please wait *:** (minutes:seconds)' + ' '+ FormatDateTime('n:ss', TimRemain/SecsPerDay) + ' before trying again thank you.'+ sLineBreak+ sLineBreak;
         str := str + 'Indicators have been updated and show current state. '+ sLineBreak;
         str := str + 'You can click the refresh button to re-check status at any time.';

         us_MessageDlg('Force Update', str, mtInformation,[mbOk],0) ; //Display message
        end;
    end;
   dtdns_perform_update := dtdns_ret;  // Return IP state as seen from internet
end;
{--- End dtdns_perform_update ---------------------------------------------}


{ Tdtdns }

procedure Tdtdns.Btn_help_accountsClick(Sender: TObject);
var
  str:string;
begin
   str:='';
   str := str + 'Note: With configured accounts initially opening the DtDNS updater ' + sLineBreak;
   str := str + 'client buttons are greyed out until the refreshed button is clicked.' + sLineBreak;
   str := str + 'This forces user to manually refresh.' + sLineBreak + sLineBreak;

   str := str + '1) Enter your DtDNS domain names.' + sLineBreak;
   str := str + '2) Enter your DtDNS account password.' + sLineBreak;
   str := str + '3) Click "Save changes" button. Saves data to configuration file. ' + sLineBreak+ sLineBreak;

   str := str + '4) Click "Force Update button".' + sLineBreak;
   str := str + ' Note: The update process can take upto 10 minutes.' + sLineBreak;
   str := str + ' Note: You need to perform a manual update every time your IP changes.' + sLineBreak;
   str := str + ' Note: Prefered solution enable Auto-run 5).' + sLineBreak+ sLineBreak;

   str := str + '5) To automatically run DtDNS update click "Auto-run" check box' + sLineBreak;
   str := str + 'This will periodically run DtDNS update.';

   us_MessageDlg('DtDNS Help', str, mtInformation,[mbOk],0) ; //Display message

end;

procedure Tdtdns.Btn_force_updateClick(Sender: TObject);

var
 str:string;
begin
 If dtdns_perform_update Then // Obtain IP as seen from internet and update ran
   begin
     If Not DTDNS_wait Then   // First run not waiting for 10M timeout
       begin                  // if waiting for 10M donot display this message
         str:='';
         str := str + 'Domain-Name IP address update initiated.' + sLineBreak + sLineBreak;
         us_MessageDlg('Domain-Name IP address update', str, mtInformation,[mbOk],0) ; //Display message
       end;
    end
   Else     // Unable to obtain IP as seen from internet
     begin
       str:='';
       str := str + 'Unable to obtain your IP address as seen from the Internet!' + sLineBreak + sLineBreak;
       str := str + 'Possible causes:' + sLineBreak + sLineBreak;
       str := str + '1) Your PC is not connected to the Internet.' + sLineBreak;
       str := str + '2) The DtDNS server may be temporarily down.'+ sLineBreak + sLineBreak;
       str := str + 'Either connect to the Internet or wait for DtDNS.' + sLineBreak;
       str := str + 'Run this menu option again or click' + sLineBreak;
       str := str + 'the refresh button to re-check status.';

       us_MessageDlg('IP Address error', str, mtInformation,[mbOk],0) ; //Display message
     end;
end;

procedure Tdtdns.Btn_force_refreshClick(Sender: TObject);
var
 str : string;
begin
 // Inform user this may take sometime
 str:='';
 str := str + 'Going to perform a Domain-Name status check over the Internet.' + sLineBreak+ sLineBreak;
 str := str + 'Note 1: This will take a few seconds please wait for it to complete.'  + sLineBreak;
 str := str + 'Note 2: If challenged by your firewall allow Internet access.' + sLineBreak+ sLineBreak;
 str := str + 'Click OK to perform check.';

 us_MessageDlg('Check current Domain-Name status', str, mtInformation,[mbOk],0); //Display message
 Application.Processmessages; //Force update ensures message dialogue closed (re-draw)

 // Check IP address as seen from internet if obtained
 // Check domain-name IP addresses are up to date at dtdns
 // Set indicators as appropriate
 If dtdns_validate Then //Valid config settings perform check
   begin
     If check_ips_update_indicators Then // Obtain IP as seen from internet and update ran
       begin
        str:='';
        str := str + 'Domain-Name status check complete.' + sLineBreak + sLineBreak;

        str := str + 'NOTE indicator colour code:' + sLineBreak;

        str := str + 'Grey:' + sLineBreak;
        str := str + 'Unable to obtain your IP address or Domain-Name not configured.' + sLineBreak;
        str := str + 'No action required.'+ sLineBreak + sLineBreak;

        str := str + 'Red:' + sLineBreak;
        str := str + 'Domain-Name IP address does not match your current IP address.' + sLineBreak;
        str := str + 'You need to run "Force Update".'+ sLineBreak + sLineBreak;

        str := str + 'Green:' + sLineBreak;
        str := str + 'Both Domain-Name IP address and your current IP address match.' + sLineBreak;
        str := str + 'No action required.';

        us_MessageDlg('Domain-Name status check', str, mtInformation,[mbOk],0) ; //Display message
     end
   Else     // Unable to obtain IP as seen from internet
     begin
       str:='';
       str := str + 'Unable to obtain your IP address as seen from the Internet!' + sLineBreak + sLineBreak;
       str := str + 'Possible causes:' + sLineBreak + sLineBreak;
       str := str + '1) Your PC is not connected to the Internet.' + sLineBreak;
       str := str + '2) The DtDNS server may be temporarily down.'+ sLineBreak + sLineBreak;
       str := str + 'Either connect to the Internet or wait for DtDNS.' + sLineBreak;
       str := str + 'Run this menu option again or click' + sLineBreak;
       str := str + 'the refresh button to re-check status.';

       us_MessageDlg('IP Address error', str, mtInformation,[mbOk],0) ; //Display message
     end;
   end;

 //If not auto-run enable input and buttons
 //user had performed a refresh.
 If Not DTDNS_auto_run Then
   begin
    edit_domain_name_1.Enabled := True;
    edit_domain_name_2.Enabled := True;
    edit_domain_name_3.Enabled := True;
    edit_domain_name_4.Enabled := True;
    edit_domain_name_5.Enabled := True;
    Btn_save_changes.Enabled   := True;
    Btn_force_update.Enabled   := True;
    edit_password.Enabled      := True;
    GB2.Enabled := True;
    GB3.Enabled := True;
   end;
end;

procedure Tdtdns.Btn_auto_run_helpClick(Sender: TObject);
var
  str:string;
begin
  str:='';
  str := str + 'Checked: ' + sLineBreak;
  str := str + 'Automatically update DtDNS accounts when UniCotroller is started.' + sLineBreak;
  str := str + 'Also disables Accounts section.' + sLineBreak  + sLineBreak;

  str := str + 'UnChecked: ' + sLineBreak;
  str := str + 'Disable automatic DtDNS update and enable Accounts section.' + sLineBreak;

  us_MessageDlg('DtDNS Auto-run Help', str, mtInformation,[mbOk],0) ; //Display message
end;



procedure Tdtdns.Btn_save_changesClick(Sender: TObject);
var
Ini1:TINIFile; // Handle for configuration file
begin
      //Check for password and at least one domain name
      If (  not (edit_domain_name_1.Text = '')
         or not (edit_domain_name_2.Text = '')
         or not (edit_domain_name_3.Text = '')
         or not (edit_domain_name_4.Text = '')
         or not (edit_domain_name_5.Text = ''))
         and not (edit_password.Text = '') Then // Domain name and password. Save content to file
           begin
             //Set new global variables
             DTDNS_domain_name_1    := edit_domain_name_1.Text;
             DTDNS_domain_name_2    := edit_domain_name_2.Text;
             DTDNS_domain_name_3    := edit_domain_name_3.Text;
             DTDNS_domain_name_4    := edit_domain_name_4.Text;
             DTDNS_domain_name_5    := edit_domain_name_5.Text;
             DTDNS_account_password := edit_password.Text;

             //Save new values in configuration file
             Ini1 := TINIFile.Create(USF_DTDNS_INI);    // create object
             //* Start
             Ini1.WriteString('ACCOUNTS','domain_name_1', DTDNS_domain_name_1); // New DtDns Domain names
             Ini1.WriteString('ACCOUNTS','domain_name_2', DTDNS_domain_name_2);
             Ini1.WriteString('ACCOUNTS','domain_name_3', DTDNS_domain_name_3);
             Ini1.WriteString('ACCOUNTS','domain_name_4', DTDNS_domain_name_4);
             Ini1.WriteString('ACCOUNTS','domain_name_5', DTDNS_domain_name_5);
             Ini1.WriteString('ACCOUNTS','password', DTDNS_account_password); // New DtDNS Account login password
              //* End
             Ini1.Free; // Free method of object

             GB2.Enabled               := True;  // Enable auto-run section
             Btn_force_update.Enabled  := True;  // Enable force update
             Btn_force_refresh.Enabled := True;  // Enable refresh

             us_MessageDlg('Configuration Updated','Configuration file updated with new values', mtInformation,[mbOk],0) ; //Display message
           end
      Else     //No domain or Password
        begin
          GB2.Enabled := False;   // Disable auto-run section
          us_MessageDlg('Error', 'A password and at least one domain name required.', mtError,[mbOk],0) ; //Display message
        end;
end;

procedure Tdtdns.Btn_view_logClick(Sender: TObject);
begin
  If FileExists(USF_DTDNS_LOG) Then
    us_display_in_editor(USF_DTDNS_LOG); //Display log file
end;

procedure Tdtdns.CB1_auto_runClick(Sender: TObject);
begin
  // Check box enable/disable auto-run.
  // Checked   = Auto-run enabled
  // UnChecked = Auto-run disabled

IF FileExists(USF_DTDNS_INI) Then
  begin
   If (CB1_auto_run.Checked ) Then  //==Enable Auto-run
     begin
       us_ini_set(USF_DTDNS_INI,'DTDNS','auto_run','true');   //Set value in ini configuration file
       GB1.Enabled := False;
       DTDNS_auto_run := True;
     end
   Else   //==Disable  Auto-run
     begin
       us_ini_set(USF_DTDNS_INI,'DTDNS','auto_run','false'); //Set value in ini configuration file
       GB1.Enabled := True;
       DTDNS_auto_run := False;
     end;
  end;
end;

procedure Tdtdns.CB2_enable_loggingClick(Sender: TObject);
begin
   // Check box enable/disable logging.
   // Checked   = Logging enabled
   // UnChecked = Logging disabled

 IF FileExists(USF_DTDNS_INI) Then
   begin
    If (CB2_enable_logging.Checked ) Then //==Enable Loggin
      begin
       DTDNS_logging := True;
       us_ini_set(USF_DTDNS_INI,'DTDNS','logging','true') //Set value in ini configuration file
      end
    Else   //==Disable  Logging
      begin
       DTDNS_logging := False;
       us_ini_set(USF_DTDNS_INI,'DTDNS','logging','false'); //Set value in ini configuration file
      end;
   end;
end;


procedure Tdtdns.FormShow(Sender: TObject);
begin
 //--Display contents of configuration file
 edit_domain_name_1.Text := DTDNS_domain_name_1;
 edit_domain_name_2.Text := DTDNS_domain_name_2;
 edit_domain_name_3.Text := DTDNS_domain_name_3;
 edit_domain_name_4.Text := DTDNS_domain_name_4;
 edit_domain_name_5.Text := DTDNS_domain_name_5;
 edit_password.Text      := DTDNS_account_password;

 //--Check for account password and at least one domain name set
 If dtdns_validate Then    // Valid inputs
   begin
      Btn_force_update.Enabled  := True;    // Enable force update
      Btn_force_refresh.Enabled := True;    // Enable refresh
      //DTDNS_auto_run check box
      If DTDNS_auto_run Then
        begin
         CB1_auto_run.state := cbChecked;   // Auto-run enabled
         GB1.Enabled := False;
        end
      Else
        begin
          CB1_auto_run.State        := cbUnchecked; // Auto-run disabled
          GB1.Enabled               := True;
          Btn_save_changes.Enabled  := False;       // Disable save changes
          // Not auto-run hence disable input and buttons
          // until user runs refresh.
          edit_domain_name_1.Enabled := False;
          edit_domain_name_2.Enabled := False;
          edit_domain_name_3.Enabled := False;
          edit_domain_name_4.Enabled := False;
          edit_domain_name_5.Enabled := False;
          Btn_save_changes.Enabled   := False;
          Btn_force_update.Enabled   := False;
          edit_password.Enabled      := False;
          GB2.Enabled := False;
          GB3.Enabled := False;
        end;
   end
 Else //Invalid - Either no domain name and/or password
   begin
      Btn_save_changes.Enabled  := True;                       // Enable save changes
      Btn_force_update.Enabled  := False;                      // Disable force update
      Btn_force_refresh.Enabled := False;                      // Disable refresh
      GB2.Enabled := False;                                    // Disable auto-run section
      //DTDNS_auto_run check box
      If DTDNS_auto_run Then
        begin
         DTDNS_auto_run := False;                               // Reset Auto run
         us_ini_set(USF_DTDNS_INI,'DTDNS','auto_run','false');  // Save in config
         CB1_auto_run.state := cbUnchecked;                     // Auto-run disable
         GB1.Enabled := True;                                   // Enable user input
        end
      Else
        begin
          CB1_auto_run.State := cbUnchecked;                    // Auto-run disabled
          GB1.Enabled := True;                                  // Enable user input
        end;
   end;

 If FileExists(USF_DTDNS_LOG) Then
   Btn_view_log.Enabled           := True    // Enable view log
 Else
   Btn_view_log.Enabled           := False;  // Disable view log

 //DTDNS_logging check box
If DTDNS_logging Then
   CB2_enable_logging.state := cbChecked     // Loging enabled
Else
   CB2_enable_logging.State := cbUnchecked;  // Loging disabled

end;

end.

