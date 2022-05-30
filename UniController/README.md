# UniformServer
The Uniform Server is a free lightweight WAMP server solution for Windows. Less than 50MB, modular design, includes the latest versions of Apache2, PHP (switch between PHP 7.0, PHP 7.1, PHP 7.2, PHP 7.3, PHP 7.4, PHP 8.0, PHP 8.1), MySQL5, MYSQL8 or MariaDB10, phpMyAdmin or Adminer4. No installation required! No registry dust! Just unpack and fire up!

## UniController
The UniController is the heart of the UniformServer Package where everything can be controlled, modified and updated. It is built using Pascal and compiled with Lazurus.

### Create working environment

Create a working environment for compiling and testing code:

 1. Create a new folder, for example z_controller
 2. Download the latest version of Uniform Server ZeroXIV 
    for example 14_0_5_ZeroXIV.exe and save to folder z_controller
 3. Double click on downloaded file (14_0_5_ZeroXIV.exe); this extracts Uniform Server ZeroXIV to folder z_controller\UniServerZ
 4. Download the source from Github to folder z_controller\UniServerZ 
 5. Note: Two new folders are created: z_controller\UniServerZ\synapse and z_controller\UniServerZ\unicon_images
          Project source code is added to folder z_controller\UniServerZ
 6. That completes working environment creation
 
###  Compiling UniController

With the working environment in place, you are ready to compile UniController as follows:
 
 1. Start Lazarus
 2. Close any existing projects: Project > Close Project
 3. In the pop-up window, click "Open Projects" button 
    Navigate to folder: z_controller\UniServerZ
    Click on file UniController.lpi, click Open button
    The projects open and is ready for compiling
 4. A quick test run the project: Run > run or press F9
    Note: Synapse will produce several warning Hint messages; these are not errors
    Last line displayed: Project "UniController" successfully built
 5. UniController will run

 You can now change code as required and re-compile 

### AutoIt Files

The source code also includes the following AutoIT scripts in the autoit directory

 1. Install_PEAR.au3 - Automated the complete installation of PEAR
