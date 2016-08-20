You will have to set firefox to import bookmarks in his html file bookmarks.html.
(only for firefox versions >=3)
To achieve that, open about:config in firefox and double click on this line to enable value
to true:
user_pref("browser.bookmarks.autoExportHTML", false);
You should have now:
user_pref("browser.bookmarks.autoExportHTML", true);
NOTE: This is also working in the same way for mozilla aka seamonkey.
