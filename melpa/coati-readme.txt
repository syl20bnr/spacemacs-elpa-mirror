emacs-coati
===========

emacs-coati is a plugin for Emacs to communicate with Coati_.

.. _Coati: https://coati.io

Install
-------

Usage
-----

From Coati to Emacs
~~~~~~~~~~~~~~~~~~~

* enable coati-mode in Emacs
* Right click in coati -> **Set IDE Curor**
* In the Emacs should now open the file and put the cursor in the position form coati.

From Emacs to Coati
~~~~~~~~~~~~~~~~~~~

* Navigate your cursor to the location in the text.
* Sent location to coati

+ Press **M-x** and enter **coati-send-loation**
+ bind **coati-send-location** to a key sequence and use it.

Preferences
-----------

* **M-x** customize
* search for coati
* 3 Settins should be displayed now

Emacs Coati Ip
~~~~~~~~~~~~~~

Ip address for the Tcp communcation, default is ``localhost``

Emacs Coati Port Coati
~~~~~~~~~~~~~~~~~~~~~~

Port Coati listens to, default is ``6667``

Emacs Coati Port Emacs
~~~~~~~~~~~~~~~~~~~~~~

Port Coati listens to, default is ``6666``
