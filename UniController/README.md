# Uniform Server
Uniform Server is a free lightweight WAMP server solution for Windows.
Build using a modular design approach, it includes the latest versions of Apache, MySQL or MariaDB, PHP (with version switching), phpMyAdmin or Adminer.

No installation required! No registry dust! Just unpack and fire up!

## UniController
The UniController is the heart of the Uniform Server Package where everything can be controlled, modified and updated. It is built using Pascal and compiled with Lazarus.

### Create working environment

Create a working environment for compiling and testing code:

 1. Create a new folder, for example z_controller.
 2. Download the latest version of Uniform Server ZeroXV, for example 15_0_0_ZeroXV.exe and save to folder z_controller.
 3. Double click on downloaded file (15_0_0_ZeroXV.exe); this extracts Uniform Server ZeroXV to folder z_controller\UniServerZ.
 4. Download the source from Github to folder z_controller\UniServerZ.
 5. Note: Two new folders are created: z_controller\UniServerZ\synapse and z_controller\UniServerZ\unicon_images. 
    Project source code is added to folder z_controller\UniServerZ.
 6. That completes working environment creation.
 
###  Compiling UniController

With the working environment in place, you are ready to compile UniController as follows:
 
 1. Start Lazarus.
 2. Close any existing project: Project > Close Project.
 3. In the pop-up window, click "Open Project" button.
    Navigate to folder: z_controller\UniServerZ.
    Click on file UniController.lpi, click Open button.
    The project opens and is ready for compiling.
 4. A quick test run the project: Run > run or press F9.
    Note: Synapse will produce several warning Hint messages; these are not errors.
    Last line displayed: Project "UniController" successfully built.
 5. UniController will run.

 You can now change code as required and re-compile.

### AutoIt Files

The source code also includes the following AutoIt script in the autoit directory:

 1. Install_PEAR.au3 - Automated the complete installation of PEAR.
