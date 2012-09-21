README!
-------

Before you start:
-----------------
For everything to work nice and easy it is required to have the
crypto software for erlang installed.

How to start downloading a file using commandline:
--------------------------------------------------
From the root folder funkytorrents, go into the /src sub folder.
Here the .torrent files are available and this is also the 
directory where the downloaded files will be stored.
Start the erlang shell.

Int the erlang shell write either: 
             start:a().              	(258KB)
             start:b().			(4.3MB)
             start:c().			(8.3MB)

to download the different example files.
--------------------------------------------------

How to start from webapplication:
--------------------------------------------------
In the /chicagoboss folder execute start.sh to start the 
Chicago Boss server.

Open a browser and enter address http://localhost:8001/funkytorrent/login

Logindetails:
username: funky
password: 1234

Click the button that appears to download the file.
---------------------------------------------------
Some restrictions in this prototype:

-It can only download from one single peer at the moment.
-The peer must have the whole file that is about to be downloaded.
-The download speed is thereby limited.


