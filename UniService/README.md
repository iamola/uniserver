# UniformServer
The Uniform Server is a free lightweight WAMP server solution for Windows. Less than 50MB, modular design, includes the latest versions of Apache2, PHP (switch between PHP 7.0, PHP 7.1, PHP 7.2, PHP 7.3, PHP 7.4, PHP 8.0, PHP 8.1), MySQL5, MYSQL8 or MariaDB10, phpMyAdmin or Adminer4. No installation required! No registry dust! Just unpack and fire up!

## UniService
The UniService plugin for UniformServer enables Uniform Service to be run as a Service on Windows.

### Create working environment

Create a working environment for compiling and testing code:

 1. Create a new folder, for example z_controller
 2. Download the latest version of Uniform Server ZeroXIV 
    for example 14_0_5_ZeroXIV.exe and save to folder z_controller
 3. Double click on downloaded file (14_0_5_ZeroXIV.exe); this extracts Uniform Server ZeroXIV to folder z_controller\UniServerZ
 4. Download the latest version of UniService and save to folder z_controller\UniServerZ 
 5. Double click on downloaded file (ZeroXIV_uniservice_x_x_x.exe); this extracts UniService to folder z_controller\UniServerZ
 6. Download the source from Github to folder z_controller\UniServerZ 
 7. Note: A new folder is created: z_controller\UniServerZ\unicon_images
          Project source code is added to folder z_controller\UniServerZ
 8. That completes working environment creation

###  Compiling UniService

With the working environment in place, you are ready to compile UniController as follows:
 
 1. Start Lazarus
 2. Close any existing projects: Project > Close Project
 3. In the pop-up window, click "Open Project" button 
    Navigate to folder: z_controller\UniServerZ
    Click on file UniService.lpi, click Open button
 4. You may have to convert the "Execution Level" at 
    Project > Project Options > Application > For Windows
    from "Require Admin" to "as invoker"; otherwise, the compiler will throw an error
    The projects open and is ready for compiling
 5. A quick test run the project: Run > run or press F9
    Last line displayed: Project "UniService" successfully built
 6. UniService will run

 You can now change code as required and re-compile