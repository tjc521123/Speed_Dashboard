# Initial Set-Up

First, download the `dist` folder.

You must download and install `R Portable` from:
	https://sourceforge.net/projects/rportable/

The location of the installation should be the `dist` folder. 

After installing, launch R Portable and install `shiny` and `pacman`.
* Run the following commands to install the packages:
	* `install.packages('shiny')`
	* `install.packages('pacman')`

# Running the Application

Double-click on the `run.bat` file. This should open the file. 

## Dependencies

The application needs Google Chrome installed in its default location to operate. 
If you want to change the browser application/location, you must edit the `runShinyApp.R`
file with:
	`chrome.sys = "new/application/location.exe"`

The new application location **MUST** be double-quoted "".
