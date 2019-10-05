# UniformServer
The Uniform Server is a free lightweight WAMP server solution for Windows. Less than 24MB, modular design, includes the latest versions of Apache2, Perl5, PHP (switch between PHP56, PHP70, PHP71, PHP72, PHP 73), MySQL5, MYSQL8 or MariaDB5, phpMyAdmin or Adminer4. No installation required! No registry dust! Just unpack and fire up!

## UniController
The UniController is the heart of the UniformServer Package wher everything can be controlled, modified and updated. It is built using Pascal and compiled with Lazurus 2.0.4.

### Create working environment

Create a working environment for compiling and testing code:

 1. Create a new folder for example z_controller
 2. Download the latest version of Uniform Server ZeroXI 
    for example 14_0_0_ZeroXIV.exe and save to folder z_controller
 3. Double click on downloaded file (14_0_0_ZeroXIV.exe) this extracts Uniform Server ZeroXI
 4. Download and copy file UniController_1_0_2_src.exe to folder z_controller 
 5. Navigate into folder z_controller and double click on
    UniController_1_0_2_src.exe this adds the project source code. 
 6. Note: Two new folders are created synapse and unicon_images
          Project source code is added to folder z_controller.
 7. That completes working environment creation.
 
###  Compiling UniController

With the working environment in place you are ready to compile UniController as follows:
 
 1. Start Lazarus
 2. Close any existing projects: Project > Close Project
 3. In the pop-up window click "Open Projects" button 
    Navigate to folder: z_controller\UniServerZ
    Click on file UniController.lpi, click Open Button.
    The projects opens and is ready for compiling.
 4. A quick test run the project: Run > run or press F9
    Note: Synapse will produce several warning Hint messages these are not errors.
    Last line displayed, Project "UniController" successfully built
 5. UniController will run.

 You can now change code as required and re-compile. 

### AutoIt Files

The source code also includes 2 AutoIT scripts in the autoit directory

 1. hidemysql8.au3 - The console window for MySQL8 seems to remain open and the noconsole option in Pascal cannot hide it. So this script does.
 2. Install_PEAR.au3 - Automated the complete installation of PEAR
