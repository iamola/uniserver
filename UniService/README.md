# Uniform Server
Uniform Server is a free lightweight WAMP server solution for Windows.
Build using a modular design approach, it includes the latest versions of Apache, MySQL or MariaDB, PHP (with version switching), phpMyAdmin or Adminer.

No installation required! No registry dust! Just unpack and fire up!

## UniService
The UniService plugin for Uniform Server enables Uniform Service to be run as a Service on Windows.

### Create working environment

Create a working environment for compiling and testing code:

 1. Create a new folder, for example z_controller
 2. Download the latest version of Uniform Server ZeroXV 
    for example 15_0_0_ZeroXV.exe and save to folder z_controller
 3. Double click on downloaded file (15_0_0_ZeroXV.exe); this extracts Uniform Server ZeroXV to folder z_controller\UniServerZ
 4. Download the latest version of UniService and save to folder z_controller\UniServerZ 
 5. Double click on downloaded file (ZeroXV_uniservice_x_x_x.exe); this extracts UniService to folder z_controller\UniServerZ
 6. Download the source from Github to folder z_controller\UniServerZ 
 7. Note: A new folder is created: z_controller\UniServerZ\unicon_images
          Project source code is added to folder z_controller\UniServerZ
 8. That completes working environment creation

###  Compiling UniService

With the working environment in place, you are ready to compile UniService as follows:
 
 1. Start Lazarus
 2. Close any existing projects: Project > Close Project
 3. In the pop-up window, click "Open Project" button 
    Navigate to folder: z_controller\UniServerZ
    Click on file UniService.lpi, click Open button
 4. You may have to convert the "Execution Level" at 
    Project > Project Options > Application > For Windows
    from "Require Admin" to "as invoker"; otherwise, the compiler will throw an error
    The project opens and is ready for compiling
 5. A quick test run the project: Run > run or press F9
    Last line displayed: Project "UniService" successfully built
 6. UniService will run

 You can now change code as required and re-compile