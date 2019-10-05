# UniformServer
The Uniform Server is a free lightweight WAMP server solution for Windows. Less than 24MB, modular design, includes the latest versions of Apache2, Perl5, PHP (switch between PHP56, PHP70, PHP71, PHP72, PHP 73), MySQL5, MYSQL8 or MariaDB5, phpMyAdmin or Adminer4. No installation required! No registry dust! Just unpack and fire up!

## UniService
The UniService plugin for UniformServer enables Uniform Service to be run as a Service on Windows.

### Create working environment

Create a working environment for compiling and testing code:

 1. Create a new folder for example z_controller
 2. Download the latest version of Uniform Server ZeroXI for example 14_0_0_ZeroXIV.exe and save to folder z_controller
 3. Double click on downloaded file (14_0_0_ZeroXIV.exe) this extracts Uniform Server ZeroXIV
 4. Download and copy the source files to the folder z_controller 
 5. Note: A new folder is created UniServerZ\unicon_images
          Project source code is added to folder UniServerZ.
 6. That completes working environment creation.

###  Compiling UniService

With the working environment in place you are ready to compile UniService as follows:

 1. Start Lazarus
 2. Close any existing projects: Project > Close Project
 3. In the pop-up window click "Open Projects" button 
    Navigate to folder: z_controller\UniServerZ
    Click on file UniService.lpi, click Open Button.
 4. You may have to convert the Execution level at 
    Project > Project Options > Application > For Windows
    from Require Admin to as invoker otherwise the compiler will throw and error.
    The projects opens and is ready for compiling.
 5. A quick test run the project: Run > run or press F9
    Last line displayed, Project "UniService" successfully built
 6. UniService will run.

 You can now change code as required and re-compile.