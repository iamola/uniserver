unit server_cert_key_gen_form;

{#############################################################################
'# Name: server_cert_key_gen_form.pas
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
  {default_lang_vars,}
  default_config_vars,
  us_common_procedures,
  us_common_functions,
  us_server_state,
  Process;

type

  { Tserver_cert_key_gen }

  Tserver_cert_key_gen = class(TForm)
    Button_help_cn: TButton;
    Button_generate: TButton;
    Button_help_o: TButton;
    Button_help_ou: TButton;
    Button_help_e: TButton;
    Button_help_l: TButton;
    Button_help_st: TButton;
    Button_help_c: TButton;
    Button_help_bits: TButton;
    Button_reload: TButton;
    ComboBox_c: TComboBox;
    ComboBox_bits: TComboBox;
    Edit_cn: TEdit;
    Edit_o: TEdit;
    Edit_ou: TEdit;
    Edit_e: TEdit;
    Edit_l: TEdit;
    Edit_st: TEdit;
    Label1: TLabel;
    Label_ou: TLabel;
    Label_e: TLabel;
    Label_l: TLabel;
    Label_st: TLabel;
    Label_c: TLabel;
    Label_bits: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label_cn: TLabel;
    Label_o: TLabel;
    procedure Button_generateClick(Sender: TObject);
    procedure Button_help_bitsClick(Sender: TObject);
    procedure Button_help_cClick(Sender: TObject);
    procedure Button_help_cnClick(Sender: TObject);
    procedure Button_help_eClick(Sender: TObject);
    procedure Button_help_lClick(Sender: TObject);
    procedure Button_help_oClick(Sender: TObject);
    procedure Button_help_ouClick(Sender: TObject);
    procedure Button_help_stClick(Sender: TObject);
    procedure Button_reloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  var
    server_cert_key_gen: Tserver_cert_key_gen;

  implementation

  {$R *.lfm}
  {=======================================================
  Load initial certificate data on form creation.
  Clicking the reload button also runs this procedure.
  ========================================================}
  procedure load_certificate_data();
  begin
    server_cert_key_gen.Edit_cn.Text  := UENV_US_SERVERNAME;    // name from config file
    server_cert_key_gen.Edit_o.Text   := 'UniServer';
    server_cert_key_gen.Edit_ou.Text  := 'Secure demo';
    server_cert_key_gen.Edit_e.Text   := 'webmaster@fred.com';
    server_cert_key_gen.Edit_l.Text   := 'Cambridge';
    server_cert_key_gen.Edit_st.Text  := 'Cambs';

    // Load RSA bits ComboBox
    server_cert_key_gen.ComboBox_bits.Text := '2048';            // Set default selection

    // Load Country ComboBox
    server_cert_key_gen.ComboBox_c.Text := 'GB United Kingdom'; // Set default selection

  end;
  {-------------------------------------------------------}

  { Tserver_cert_key_gen }

  procedure Tserver_cert_key_gen.FormCreate(Sender: TObject);

  begin

    // Load RSA bits ComboBox
    ComboBox_bits.Text := '2048';            // Set default selection
    ComboBox_bits.items.add('512');
    ComboBox_bits.items.add('1024');
    ComboBox_bits.items.add('2048');

    // Load Country ComboBox
    ComboBox_c.Text := 'GB United Kingdom'; // Set default selection
    ComboBox_c.items.add('US United States');
    ComboBox_c.items.add('CA Canada');
    ComboBox_c.items.add('AF Afghanistan');
    ComboBox_c.items.add('AL Albania');
    ComboBox_c.items.add('DZ Algeria');
    ComboBox_c.items.add('AS American Samoa');
    ComboBox_c.items.add('AD Andorra');
    ComboBox_c.items.add('AO Angola');
    ComboBox_c.items.add('AI Anguilla');
    ComboBox_c.items.add('AQ Antarctica');
    ComboBox_c.items.add('AG Antigua and Barbuda');
    ComboBox_c.items.add('AR Argentina');
    ComboBox_c.items.add('AM Armenia');
    ComboBox_c.items.add('AW Aruba');
    ComboBox_c.items.add('AU Australia');
    ComboBox_c.items.add('AT Austria');
    ComboBox_c.items.add('AZ Azerbaijan');
    ComboBox_c.items.add('BS Bahamas');
    ComboBox_c.items.add('BH Bahrain');
    ComboBox_c.items.add('BD Bangladesh');
    ComboBox_c.items.add('BB Barbados');
    ComboBox_c.items.add('BY Belarus');
    ComboBox_c.items.add('BE Belgium');
    ComboBox_c.items.add('BZ Belize');
    ComboBox_c.items.add('BJ Benin');
    ComboBox_c.items.add('BM Bermuda');
    ComboBox_c.items.add('BT Bhutan');
    ComboBox_c.items.add('BO Bolivia');
    ComboBox_c.items.add('BA Bosnia and Herzegovina');
    ComboBox_c.items.add('BW Botswana');
    ComboBox_c.items.add('BV Bouvet Island');
    ComboBox_c.items.add('BR Brazil');
    ComboBox_c.items.add('IO British Indian Ocean Territory');
    ComboBox_c.items.add('BN Brunei Darussalam');
    ComboBox_c.items.add('BG Bulgaria');
    ComboBox_c.items.add('BF Burkina Faso');
    ComboBox_c.items.add('BI Burundi');
    ComboBox_c.items.add('KH Cambodia');
    ComboBox_c.items.add('CM Cameroon');
    ComboBox_c.items.add('CV Cape Verde');
    ComboBox_c.items.add('KY Cayman Islands');
    ComboBox_c.items.add('CF Central African Republic');
    ComboBox_c.items.add('TD Chad');
    ComboBox_c.items.add('CL Chile');
    ComboBox_c.items.add('CN China');
    ComboBox_c.items.add('CX Christmas Island');
    ComboBox_c.items.add('CC Cocos (Keeling) Islands');
    ComboBox_c.items.add('CO Colombia');
    ComboBox_c.items.add('KM Comoros');
    ComboBox_c.items.add('CG Congo');
    ComboBox_c.items.add('CD Congo, The Democratic Republic of The');
    ComboBox_c.items.add('CK Cook Islands');
    ComboBox_c.items.add('CR Costa Rica');
    ComboBox_c.items.add('CI Cote D''ivoire');
    ComboBox_c.items.add('HR Croatia');
    ComboBox_c.items.add('CY Cyprus');
    ComboBox_c.items.add('CZ Czech Republic');
    ComboBox_c.items.add('DK Denmark');
    ComboBox_c.items.add('DJ Djibouti');
    ComboBox_c.items.add('DM Dominica');
    ComboBox_c.items.add('DO Dominican Republic');
    ComboBox_c.items.add('TP East Timor');
    ComboBox_c.items.add('EC Ecuador');
    ComboBox_c.items.add('EG Egypt');
    ComboBox_c.items.add('SV El Salvador');
    ComboBox_c.items.add('GQ Equatorial Guinea');
    ComboBox_c.items.add('ER Eritrea');
    ComboBox_c.items.add('EE Estonia');
    ComboBox_c.items.add('ET Ethiopia');
    ComboBox_c.items.add('FK Falkland Islands (Malvinas)');
    ComboBox_c.items.add('FO Faroe Islands');
    ComboBox_c.items.add('FJ Fiji');
    ComboBox_c.items.add('FI Finland');
    ComboBox_c.items.add('FR France');
    ComboBox_c.items.add('GF French Guiana');
    ComboBox_c.items.add('PF French Polynesia');
    ComboBox_c.items.add('TF French Southern Territories');
    ComboBox_c.items.add('GA Gabon');
    ComboBox_c.items.add('GM Gambia');
    ComboBox_c.items.add('GE Georgia');
    ComboBox_c.items.add('DE Germany');
    ComboBox_c.items.add('GH Ghana');
    ComboBox_c.items.add('GI Gibraltar');
    ComboBox_c.items.add('GR Greece');
    ComboBox_c.items.add('GL Greenland');
    ComboBox_c.items.add('GD Grenada');
    ComboBox_c.items.add('GP Guadeloupe');
    ComboBox_c.items.add('GU Guam');
    ComboBox_c.items.add('GT Guatemala');
    ComboBox_c.items.add('GN Guinea');
    ComboBox_c.items.add('GW Guinea-Bissau');
    ComboBox_c.items.add('GY Guyana');
    ComboBox_c.items.add('HT Haiti');
    ComboBox_c.items.add('HM Heard Island and McDonald Islands');
    ComboBox_c.items.add('VA Holy See (Vatican City State)');
    ComboBox_c.items.add('HN Honduras');
    ComboBox_c.items.add('HK Hong Kong');
    ComboBox_c.items.add('HU Hungary');
    ComboBox_c.items.add('IS Iceland');
    ComboBox_c.items.add('IN India');
    ComboBox_c.items.add('ID Indonesia');
    ComboBox_c.items.add('IE Ireland');
    ComboBox_c.items.add('IL Israel');
    ComboBox_c.items.add('IT Italy');
    ComboBox_c.items.add('JM Jamaica');
    ComboBox_c.items.add('JP Japan');
    ComboBox_c.items.add('JO Jordan');
    ComboBox_c.items.add('KZ Kazakstan');
    ComboBox_c.items.add('KE Kenya');
    ComboBox_c.items.add('KI Kiribati');
    ComboBox_c.items.add('KR Korea, Republic of');
    ComboBox_c.items.add('KW Kuwait');
    ComboBox_c.items.add('KG Kyrgyzstan');
    ComboBox_c.items.add('LA Lao People''s Democratic Republic');
    ComboBox_c.items.add('LV Latvia');
    ComboBox_c.items.add('LB Lebanon');
    ComboBox_c.items.add('LS Lesotho');
    ComboBox_c.items.add('LR Liberia');
    ComboBox_c.items.add('LI Liechtenstein');
    ComboBox_c.items.add('LT Lithuania');
    ComboBox_c.items.add('LU Luxembourg');
    ComboBox_c.items.add('MO Macau');
    ComboBox_c.items.add('MK Macedonia');
    ComboBox_c.items.add('MG Madagascar');
    ComboBox_c.items.add('MW Malawi');
    ComboBox_c.items.add('MY Malaysia');
    ComboBox_c.items.add('MV Maldives');
    ComboBox_c.items.add('ML Mali');
    ComboBox_c.items.add('MT Malta');
    ComboBox_c.items.add('MH Marshall Islands');
    ComboBox_c.items.add('MQ Martinique');
    ComboBox_c.items.add('MR Mauritania');
    ComboBox_c.items.add('MU Mauritius');
    ComboBox_c.items.add('YT Mayotte');
    ComboBox_c.items.add('MX Mexico');
    ComboBox_c.items.add('FM Micronesia, Federated States of');
    ComboBox_c.items.add('MD Moldova, Republic of');
    ComboBox_c.items.add('MC Monaco');
    ComboBox_c.items.add('MN Mongolia');
    ComboBox_c.items.add('MS Montserrat');
    ComboBox_c.items.add('MA Morocco');
    ComboBox_c.items.add('MZ Mozambique');
    ComboBox_c.items.add('MM Myanmar');
    ComboBox_c.items.add('NA Namibia');
    ComboBox_c.items.add('NR Nauru');
    ComboBox_c.items.add('NP Nepal');
    ComboBox_c.items.add('NL Netherlands');
    ComboBox_c.items.add('AN Netherlands Antilles');
    ComboBox_c.items.add('NC New Caledonia');
    ComboBox_c.items.add('NZ New Zealand');
    ComboBox_c.items.add('NI Nicaragua');
    ComboBox_c.items.add('NE Niger');
    ComboBox_c.items.add('NG Nigeria');
    ComboBox_c.items.add('NU Niue');
    ComboBox_c.items.add('NF Norfolk Island');
    ComboBox_c.items.add('MP Northern Mariana Islands');
    ComboBox_c.items.add('NO Norway');
    ComboBox_c.items.add('OM Oman');
    ComboBox_c.items.add('PK Pakistan');
    ComboBox_c.items.add('PW Palau');
    ComboBox_c.items.add('PS Palestinian Territory, Occupied');
    ComboBox_c.items.add('PA Panama');
    ComboBox_c.items.add('PG Papua New Guinea');
    ComboBox_c.items.add('PY Paraguay');
    ComboBox_c.items.add('PE Peru');
    ComboBox_c.items.add('PH Philippines');
    ComboBox_c.items.add('PN Pitcairn');
    ComboBox_c.items.add('PL Poland');
    ComboBox_c.items.add('PT Portugal');
    ComboBox_c.items.add('PR Puerto Rico');
    ComboBox_c.items.add('QA Qatar');
    ComboBox_c.items.add('RE Reunion');
    ComboBox_c.items.add('RO Romania');
    ComboBox_c.items.add('RU Russian Federation');
    ComboBox_c.items.add('RW Rwanda');
    ComboBox_c.items.add('SH Saint Helena');
    ComboBox_c.items.add('KN Saint Kitts and Nevis');
    ComboBox_c.items.add('LC Saint Lucia');
    ComboBox_c.items.add('PM Saint Pierre and Miquelon');
    ComboBox_c.items.add('VC Saint Vincent and The Grenadines');
    ComboBox_c.items.add('WS Samoa');
    ComboBox_c.items.add('SM San Marino');
    ComboBox_c.items.add('ST Sao Tome and Principe');
    ComboBox_c.items.add('SA Saudi Arabia');
    ComboBox_c.items.add('SN Senegal');
    ComboBox_c.items.add('SC Seychelles');
    ComboBox_c.items.add('SL Sierra Leone');
    ComboBox_c.items.add('SG Singapore');
    ComboBox_c.items.add('SK Slovakia');
    ComboBox_c.items.add('SI Slovenia');
    ComboBox_c.items.add('SB Solomon Islands');
    ComboBox_c.items.add('SO Somalia');
    ComboBox_c.items.add('ZA South Africa');
    ComboBox_c.items.add('GS South Georgia and The South Sandwich Islands');
    ComboBox_c.items.add('ES Spain');
    ComboBox_c.items.add('LK Sri Lanka');
    ComboBox_c.items.add('SR Suriname');
    ComboBox_c.items.add('SJ Svalbard and Jan Mayen');
    ComboBox_c.items.add('SZ Swaziland');
    ComboBox_c.items.add('SE Sweden');
    ComboBox_c.items.add('CH Switzerland');
    ComboBox_c.items.add('TW Taiwan, Province of China');
    ComboBox_c.items.add('TJ Tajikistan');
    ComboBox_c.items.add('TZ Tanzania, United Republic of');
    ComboBox_c.items.add('TH Thailand');
    ComboBox_c.items.add('TG Togo');
    ComboBox_c.items.add('TK Tokelau');
    ComboBox_c.items.add('TO Tonga');
    ComboBox_c.items.add('TT Trinidad and Tobago');
    ComboBox_c.items.add('TN Tunisia');
    ComboBox_c.items.add('TR Turkey');
    ComboBox_c.items.add('TM Turkmenistan');
    ComboBox_c.items.add('TC Turks and Caicos Islands');
    ComboBox_c.items.add('TV Tuvalu');
    ComboBox_c.items.add('UG Uganda');
    ComboBox_c.items.add('UA Ukraine');
    ComboBox_c.items.add('AE United Arab Emirates');
    ComboBox_c.items.add('GB United Kingdom');
    ComboBox_c.items.add('UM United States Minor Outlying Islands');
    ComboBox_c.items.add('UY Uruguay');
    ComboBox_c.items.add('UZ Uzbekistan');
    ComboBox_c.items.add('VU Vanuatu');
    ComboBox_c.items.add('VE Venezuela');
    ComboBox_c.items.add('VN Viet Nam');
    ComboBox_c.items.add('VG Virgin Islands, British');
    ComboBox_c.items.add('VI Virgin Islands, U.S.');
    ComboBox_c.items.add('WF Wallis and Futuna');
    ComboBox_c.items.add('EH Western Sahara');
    ComboBox_c.items.add('YE Yemen');
    ComboBox_c.items.add('YU Yugoslavia');
    ComboBox_c.items.add('ZM Zambia');
    ComboBox_c.items.add('ZW Zimbabwe');
  end;

  procedure Tserver_cert_key_gen.FormShow(Sender: TObject);
  var
    str:string;
  begin

   //=== Check for CA
   If FileExists(USF_CERT_CA) Then
     begin
      //-- CA Found
      str := '';
      str := str + 'It looks like you are using your own CA.' + sLineBreak;
      str := str + 'To avoid overwriting your current server certificate and key' + sLineBreak;
      str := str + 'this script has terminated.' + sLineBreak + sLineBreak;
      str := str + 'To create a new server certificate and key, use the CA script.' ;
      us_MessageDlg('CA Found', str, mtInformation,[mbOk],0) ; // Display message
      server_cert_key_gen.close;   // Close form nothing else to do
     end;

    //=== Check for Server certificate.
    //    If it exists let user back out

    If FileExists(USF_CERT) Then
      begin
        // Server Certificate Found
        str := '';
        str := str +  'A server certificate was found.'    + sLineBreak + sLineBreak;
        str := str +  'Would you like to continue and '    + sLineBreak;
        str := str +  'generate a NEW certificate and Key?'    ;
       if us_MessageDlg('Server Certificate Found', str, mtConfirmation,[mbYes, mbNo],0) = mrNo then
          server_cert_key_gen.close;   // Close form nothing else to do
      end;

    // Load default certificate data
    load_certificate_data();
  end;

  procedure Tserver_cert_key_gen.Button_reloadClick(Sender: TObject);
  begin
    load_certificate_data(); // Reload default data
  end;


  procedure Tserver_cert_key_gen.Button_help_cnClick(Sender: TObject);
  var
    str:string;
  begin
    // Help - Common name CN
    str := '';
    str := str + '(CN) Common Name, usually the web server hostname or your name.' + sLineBreak;
    str := str + 'To secure https://www.fred.com, your common name is www.fred.com ' + sLineBreak;
    str := str + 'or *.fred.com for a wildcard certificate.';
    us_MessageDlg('Help - Common name CN', str, mtInformation,[mbOk],0) ; // Display message
  end;

  procedure Tserver_cert_key_gen.Button_help_cClick(Sender: TObject);
  var
    str:string;
  begin
    // Help - Country C
    str := '';
    str := str + '(C) Country code two alphabetic characters.' + sLineBreak;
    str := str + 'For example "United Kingdom" gives "UK"';
    us_MessageDlg('Help - Country C' , str, mtInformation,[mbOk],0) ; // Display message
  end;

  procedure Tserver_cert_key_gen.Button_help_bitsClick(Sender: TObject);
  var
    str:string;
  begin
   // Help - RSA Bit length
   str :='';
   str := str + 'Certification authorities are no longer issuing '      + sLineBreak;
   str := str + 'certificates that are less than 2048 bit key lengths.' + sLineBreak;
   str := str + 'Recommended to leave this set to default of 2048 bits. ' ;
   us_MessageDlg('Help - RSA Bit length', str, mtInformation,[mbOk],0) ; // Display message
  end;

  procedure Tserver_cert_key_gen.Button_generateClick(Sender: TObject);
  var
    str:string;
    rsa_value        :string;
    country          :string;
    state            :string;
    location         :string;
    organisation_o   :string;
    organisation_ou  :string;
    email            :string;
    commonname       :string;
    AProcess: TProcess;
    cmd_str:string;
  begin
    //== Read certificate data entered in form
    rsa_value        := ComboBox_bits.Text;         // Default 2048
    country          := Copy(ComboBox_c.Text, 0,2); // Extract two letter code e.g UK
    state            := Edit_st.Text;               // e.g Cambs
    location         := Edit_l.Text;                // e.g Cambridge
    organisation_o   := Edit_o.Text;                // e.g. UniServer
    organisation_ou  := Edit_ou.Text;               // e.g. Secure Demo
    email            := Edit_e.Text;                // e.g. me@fred.com
    commonname       := Edit_cn.Text;               // e.g. fred.com

    //==== Certificate and key generate
    //--Build (dos) command lines and
    //--Run process this creates the certificate and key

    AProcess := TProcess.Create(nil);       // Create process
    AProcess.Executable := 'cmd';           // Want to run a command prompt

    AProcess.Parameters.Add('/T:B0');       // Set background colour
    AProcess.Parameters.Add('/c');          // 'c' Close when finished note 'K' remains open
    AProcess.Parameters.Add('title');       // A title is required
    AProcess.Parameters.Add('US Test');     // Title

    AProcess.Parameters.Add('&&');          // Start a new command line
    AProcess.Parameters.Add('CD');          // Change directory
    AProcess.Parameters.Add(US_APACHE_BIN); // To path Apache bin openssl.exe

    AProcess.Parameters.Add('&&');          // Start a new command line
    AProcess.Parameters.Add('set');         // Set environment variable command Dos
    AProcess.Parameters.Add('OPENSSL_CONF='+US_OPENSSL + '\openssl.cnf'); // Set cmd variable OPENSSL_CONF

    AProcess.Parameters.Add('&&');                   // Start a new command line
    AProcess.Parameters.Add('openssl');              // Run Openssl program
    AProcess.Parameters.Add('req');                  // Request new certificate
    AProcess.Parameters.Add('-newkey');              // and new key
    AProcess.Parameters.Add('rsa:' + rsa_value);     // Value set
    AProcess.Parameters.Add('-batch');               // Run in batch mode (requires no user input)
    AProcess.Parameters.Add('-nodes');               // No des
    AProcess.Parameters.Add('-out');                 // Output file is
    AProcess.Parameters.Add('server.csr');           // Server certificate
    AProcess.Parameters.Add('-keyout');              // Output key
    AProcess.Parameters.Add('server.key');           // to file server.key
    AProcess.Parameters.Add('-subj');                // Subject
      //Subject string
    cmd_str := '';
    cmd_str := cmd_str + '"';                        // Add opening quotes
    cmd_str := cmd_str + '/C='   + country;          // Add parameter
    cmd_str := cmd_str + '/ST='  + state;            // Add parameter
    cmd_str := cmd_str + '/L='   + location;         // Add parameter
    cmd_str := cmd_str + '/O='   + organisation_o;   // Add parameter
    cmd_str := cmd_str + '/OU='  + organisation_ou;  // Add parameter
    cmd_str := cmd_str + '/emailAddress=' + email;   // Add parameter
    cmd_str := cmd_str + '/CN='  + commonname;       // Add parameter
    cmd_str := cmd_str + '"';                        // Add closing quotes
    AProcess.Parameters.Add(cmd_str);                // Add Subject string

    AProcess.Parameters.Add('&&');                   // Start a new command line
    AProcess.Parameters.Add('openssl');              // Run Openssl program
    AProcess.Parameters.Add('x509');                 // Create x509 cert
    AProcess.Parameters.Add('-in');                  // Input this
    AProcess.Parameters.Add('server.csr');           // file server.csr request
    AProcess.Parameters.Add('-out');                 // Output this
    AProcess.Parameters.Add('server.crt');           // certificate file server.crt
    AProcess.Parameters.Add('-req');                 //
    AProcess.Parameters.Add('-signkey');             //
    AProcess.Parameters.Add('server.key');           //
    AProcess.Parameters.Add('-days');                //
    AProcess.Parameters.Add('3650');                 //

    AProcess.Parameters.Add('&&');                   // Start a new command line
    AProcess.Parameters.Add('set');                  // Set environment variable command
    AProcess.Parameters.Add('OPENSSL_CONF=');        // resets OPENSSL_CONF var

    AProcess.Options     := AProcess.Options + [poWaitOnExit];   // Set option wait to complete
    AProcess.ShowWindow  := swoHIDE;                             // Hide command window
    AProcess.Execute;                                            // Run command
    AProcess.Free;                                               // Release process
    //====End certificate and key generate

    //--Create server certificates folder if not exist
    ForceDirectoriesUTF8(US_APACHE_CERTS); { *Converted from ForceDirectories*  }

    //--Delete existing server certificate and key
    If FileExists(US_APACHE_CERTS+ '\server.crt') Then
       DeleteFile(US_APACHE_CERTS+ '\server.crt');  // Delete file
    If FileExists(US_APACHE_CERTS+ '\server.key') Then
       DeleteFile(US_APACHE_CERTS+ '\server.key');  // Delete file

    //-- Move Certificate and Key to server
    If FileExists(US_APACHE_BIN + '\server.crt')  Then
       RenameFile(US_APACHE_BIN + '\server.crt',US_APACHE_CERTS+ '\server.crt');  // Move file

    If FileExists(US_APACHE_BIN + '\server.key')  Then
       RenameFile(US_APACHE_BIN + '\server.key',US_APACHE_CERTS+ '\server.key');   // Move file

    //-- Move certificate-signing request (server.csr) to openssl folder
    If FileExists(US_APACHE_BIN + '\server.csr')  Then
       RenameFile(US_APACHE_BIN + '\server.csr',US_OPENSSL+ '\server.csr');  // Move file


    //--Enable SSL in configuration file
    us_enable_apache_ssl;


   // Server Certificate Generated
   str :='';
   str := str +  'Server certificate and key generated these '           + sLineBreak;
   str := str +  'have been installed in folder Apache2\server_certs '   + sLineBreak+ sLineBreak;

   str := str +  'SSL has been enabled in Apache''s configuration file.' + sLineBreak+ sLineBreak;
   str := str +  'Restart servers for changes to take place.';

   us_MessageDlg('Server Certificate Generated', str, mtInformation,[mbOk],0) ; //Display message

   sleep(100);
   us_update_server_state;     // Update menus

   server_cert_key_gen.close;  // Close form nothing else to do
  end;

  procedure Tserver_cert_key_gen.Button_help_eClick(Sender: TObject);
  var
    str:string;
  begin
    //Help - Email E
    str :='';
    str := str + '(E) Usually specified for a email user certificate ' + sLineBreak;
    str := str + 'for Activesync or SMIM. ';
    us_MessageDlg('Help - Email E', str, mtInformation,[mbOk],0) ; //Display message
  end;

  procedure Tserver_cert_key_gen.Button_help_lClick(Sender: TObject);
  var
    str:string;
  begin
   // Help - City/Local L
   str := '(L) City/Local For example, London. ';
   us_MessageDlg('Help - City/Local L', str, mtInformation,[mbOk],0) ; //Display message
  end;

  procedure Tserver_cert_key_gen.Button_help_oClick(Sender: TObject);
  var
    str:string;
  begin
    //Help - Organisation O
    str := '(O) Organisation for example, My Corporation';
    us_MessageDlg('Help - Organisation O ', str, mtInformation,[mbOk],0) ; //Display message
  end;

  procedure Tserver_cert_key_gen.Button_help_ouClick(Sender: TObject);
  var
    str:string;
  begin
    // Help - Organisation Unit OU
    str := '(OU) Your division or department. For example, PHP Dept.';
    us_MessageDlg('Help - Organisation Unit OU', str, mtInformation,[mbOk],0) ; //Display message
  end;

  procedure Tserver_cert_key_gen.Button_help_stClick(Sender: TObject);
  var
    str:string;
  begin
   // Help - State ST
   str := '(ST) For example, Cambridgeshire.';
   us_MessageDlg('Help - State ST', str, mtInformation,[mbOk],0) ; //Display message
  end;

  end.

