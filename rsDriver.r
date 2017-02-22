> require(RSelenium)
Lade nötiges Paket: RSelenium
> rsDriver(port = 4567L, browser = "chrome", version = "latest", chromever = "latest",
           +          geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
           +          verbose = TRUE, check = TRUE)
checking Selenium Server versions:
  BEGIN: PREDOWNLOAD
BEGIN: DOWNLOAD
Creating directory: C:\Users\adrian\AppData\Local\binman\binman_seleniumserver\generic\3.0.0
Downloading binary: https://www.googleapis.com/download/storage/v1/b/selenium-release/o/3.0%2Fselenium-server-s...

Creating directory: C:\Users\adrian\AppData\Local\binman\binman_seleniumserver\generic\3.0.1
Downloading binary: https://www.googleapis.com/download/storage/v1/b/selenium-release/o/3.0%2Fselenium-server-s...

Creating directory: C:\Users\adrian\AppData\Local\binman\binman_seleniumserver\generic\3.1.0
Downloading binary: https://www.googleapis.com/download/storage/v1/b/selenium-release/o/3.1%2Fselenium-server-s...

BEGIN: POSTDOWNLOAD
checking chromedriver versions:
  BEGIN: PREDOWNLOAD
BEGIN: DOWNLOAD
Creating directory: C:\Users\adrian\AppData\Local\binman\binman_chromedriver\win32\2.25
Downloading binary: https://www.googleapis.com/download/storage/v1/b/chromedriver/o/2.25%2Fchromedriver_win32.z...

Creating directory: C:\Users\adrian\AppData\Local\binman\binman_chromedriver\win32\2.26
Downloading binary: https://www.googleapis.com/download/storage/v1/b/chromedriver/o/2.26%2Fchromedriver_win32.z...

Creating directory: C:\Users\adrian\AppData\Local\binman\binman_chromedriver\win32\2.27
Downloading binary: https://www.googleapis.com/download/storage/v1/b/chromedriver/o/2.27%2Fchromedriver_win32.z...

BEGIN: POSTDOWNLOAD
checking geckodriver versions:
  BEGIN: PREDOWNLOAD
BEGIN: DOWNLOAD
Creating directory: C:\Users\adrian\AppData\Local\binman\binman_geckodriver\win64\0.14.0
Downloading binary: https://github.com/mozilla/geckodriver/releases/download/v0.14.0/geckodriver-v0.14.0-win64....

Creating directory: C:\Users\adrian\AppData\Local\binman\binman_geckodriver\win64\0.13.0
Downloading binary: https://github.com/mozilla/geckodriver/releases/download/v0.13.0/geckodriver-v0.13.0-win64....

Creating directory: C:\Users\adrian\AppData\Local\binman\binman_geckodriver\win64\0.12.0
Downloading binary: https://github.com/mozilla/geckodriver/releases/download/v0.12.0/geckodriver-v0.12.0-win64....

BEGIN: POSTDOWNLOAD
checking phantomjs versions:
  BEGIN: PREDOWNLOAD
BEGIN: DOWNLOAD
Creating directory: C:\Users\adrian\AppData\Local\binman\binman_phantomjs\windows\2.5.0
Downloading binary: https://api.bitbucket.org/2.0/repositories/ariya/phantomjs/downloads/phantomjs-2.5.0-beta-w...

Creating directory: C:\Users\adrian\AppData\Local\binman\binman_phantomjs\windows\2.1.1
Downloading binary: https://api.bitbucket.org/2.0/repositories/ariya/phantomjs/downloads/phantomjs-2.1.1-window...

Creating directory: C:\Users\adrian\AppData\Local\binman\binman_phantomjs\windows\2.0.0
Downloading binary: https://api.bitbucket.org/2.0/repositories/ariya/phantomjs/downloads/phantomjs-2.0.0-window...

BEGIN: POSTDOWNLOAD
[1] "Connecting to remote server"
Error in checkError(res) : 
  Couldnt connect to host on http://localhost:4567/wd/hub.
Please ensure a Selenium server is running.
In addition: Warning message:
  In rsDriver(port = 4567L, browser = "chrome", version = "latest",  :
                Could not determine server status.
              