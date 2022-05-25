#pragma compile(Out, Install_PEAR.exe)
#pragma compile(UPX, False)
#pragma compile(FileDescription, UniformServer - Installation of Pear for UniformServer)
#pragma compile(ProductName, US_PearInstaller)
#pragma compile(ProductVersion, 1.0)


#include <Array.au3>

;Set the Environment Variables
$path = EnvGet("PATH")
$basePath = StringReplace(@ScriptDir,"\home\us_pear","")
$apachePath = $basePath & "\core\apache2\bin"
$phpSelect = IniRead($basePath & "\home\us_config\us_user.ini","USER","PHP_SELECT","php73")
$phpPath = $basePath & "\core\" & $phpSelect
EnvSet("PATH",$apachePath & ";" & $phpPath & ";" & $path)

;Set the Pear Path in the php_cli.ini File
IniWrite($phpPath & "\php-cli.ini","PHP","include_path",'".;' & @ScriptDir & '\pear"')

;Initate the Install
Run(@ComSpec & " /T:B0 /K title PEAR Install && cd " & @ScriptDir & " && php " & @ScriptDir & "\go-pear.phar")
WinWaitActive("PEAR Install")
$hndl = WinGetHandle("PEAR Install")
SendKeepActive("PEAR Install")
Send("local {ENTER}")
Send("yes {ENTER}")
Send("13 {ENTER}")
WinWaitActive("Browse for Files or Folders")
ControlSetText("Browse for Files or Folders","","Edit1",$phpPath)
ControlClick("Browse for Files or Folders","","Button2")
Send("{ENTER}")
SendKeepActive("")

;Wait for the installation to complete
While StringStripWS(WinGetTitle($hndl),7)<>"PEAR Install"
   Sleep(200)
Wend

;Add in the Patch for the PEAR.bat

;Add in the SYSFOLDER Patch
$batFile = FileReadToArray(@ScriptDir & "\pear.bat")
$index = _ArraySearch($batFile,"%PHP_PEAR_BIN_DIR%",17,37,0,1)
$line = $batFile[$index]
$line = StringReplace($line,"PHP_PEAR_BIN_DIR","PHP_PEAR_SYSCONF_DIR")
_ArrayInsert($batFile,$index+1,$line)

;Correct the php Path
$index = _ArraySearch($batFile,'IF "%PHP_PEAR_PHP_BIN%"%',17,37,0,1)
$line = StringReplace($batFile,"\\php.exe","\php.exe")

;Rewrite the File
FileDelete(@ScriptDir & "\pear.bat")
$fHandle = FileOpen(@ScriptDir & "\pear.bat",2)
For $i = 0 to Ubound($batFile)-1
   FileWriteLine($fHandle,$batFile[$i])
Next
FileClose($fHandle)

;Install the WebFrontEnd
SendKeepActive("PEAR Install")

;Update the PEAR protocols
Send("pear channel-update pear.php.net {ENTER}")

;Wait for the installation to complete
While StringStripWS(WinGetTitle($hndl),7)<>"PEAR Install"
   Sleep(200)
Wend

;Install the PEAR Front End
Send("pear install PEAR_Frontend_Web-0.7.5 {ENTER}")
Sleep(500)

;Wait for the installation to complete
While StringStripWS(WinGetTitle($hndl),7)<>"PEAR Install"
   Sleep(200)
Wend

Send("exit {ENTER}")

SendKeepActive("")