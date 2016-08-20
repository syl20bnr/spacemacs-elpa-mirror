Yet another oddmuse for Emacs.

This mode can edit or post wiki page *asynchronous*.
So it can't hang your emacs.
You can do your work when get or post wiki page.

Below are the command you can use:

* Edit:
     `yaoddmuse-edit'                     edit wiki page.
     `yaoddmuse-edit-default'             edit default wiki page.
     `yaoddmuse-follow'                   figure out what page we need to visit
* Post:
     `yaoddmuse-post-buffer'              post buffer to wiki.
     `yaoddmuse-post-current-buffer'      post current buffer to wiki
     `yaoddmuse-post-file'                post file to wiki.
     `yaoddmuse-post-file-default'        post file to default wiki.
     `yaoddmuse-post-library'             post library to wiki.
     `yaoddmuse-post-library-default'     post library to default wiki.
     `yaoddmuse-post-dired'               post dired marked files to wiki.
     `yaoddmuse-post-dired-default'       post dired marked files to wiki.
     `yaoddmuse-post-screenshot'          post screenshot to wiki.
     `yaoddmuse-post-screenshot-default'  post screenshot to default wiki.
* View:
     `yaoddmuse-revert'                   reload current wiki page.
     `yaoddmuse-browse-page'              browse wiki page.
     `yaoddmuse-browse-page-default'      browse default wiki page.
     `yaoddmuse-browse-page-diff'         browse wiki page diff.
     `yaoddmuse-browse-page-default-diff' browse default wiki page diff.
     `yaoddmuse-browse-current-page'      browse current wiki page.
* Navigation:
     `yaoddmuse-navi-next-heading'        jump next heading.
     `yaoddmuse-navi-prev-heading'        jump previous heading.
* Update:
     `yaoddmuse-update-pagename'          will update Wiki page name.
* Insert:
     `yaoddmuse-insert-pagename'          insert wiki page name.
     `yaoddmuse-insert-file-content'      insert file content.
* Misc:
     `yaoddmuse-kill-url'                 kill current wiki page url in yank.
     `yaoddmuse-toggle-minor'             toggle minor mode state.
     `yaoddmuse-redirect'                 redirect page.
     `yaoddmuse-delete'                   delete page.
     `yaoddmuse-toggle-image-status'      toggle image status.
     `yaoddmuse-save-as'                  save special page.

Tips:
・ Get page around point:
     Command ‘yaoddmuse-follow’ try to get valid page link around point.
     If it find, edit this page, otherwise show “No link found at point.”
     And you can type “C-u” before call this command,
     then it will give you page name completing for edit.
・ Reload or switch edit page:
     When you use command ‘yaoddmuse-edit’ or ‘yaoddmuse-edit-default’,
     it will prefer to switch edit page if already have one exist.
     If you want to reload edit page forcibly, just hit “C-u” before
     execute command.
・ Smart display edit page.
     Default, edit page buffer popup when current major-mode
     is not ‘yaoddmuse-mode’, or use switch edit page buffer
     when current major-mode is ‘yaoddmuse-mode’.
・ Revert edit page:
     Command ‘yaoddmuse-revert’ revert current edit page and don’t
     need input wiki name or page name.
・ Browse page after post successful:
     If you type “C-u” before call post command,
     will browse page after post successful.
・ Post buffer to wiki:
     Command ‘yaoddmuse-post-buffer’ post special buffer to wiki,
     or use command ‘yaoddmuse-post-current-buffer’ post current buffer to wiki.
・ Post file to wiki:
     Command ‘yaoddmuse-post-file’ post special file to wiki,
     it’s useful to fast posting when you don’t want open file.
・ Post mark files in dired to wiki:
     Command ‘yaoddmuse-post-dired’ post mark files in dired to wiki,
     this command is useful when update many files to wiki.
・ Post library to wiki:
     Command ‘yaoddmuse-post-library’ and ‘yaoddmuse-post-library-default’
     will post special library to wiki, and not need input file path,
     it’s so lazy! ;)
・ Remember last summary:
     By default, yaoddmuse remember last `summary' string, if you input
     same `summary' as previous time, just hit RET.
・ Pick up file name:
     By default, when you use command `yaoddmuse-post-library' and
     `yaoddmuse-post-library-default', those commands can pick up
     file name around point, if it's library name you want, just
     hit RET.  ;)
・ Pick up page name:
     When you use commands `yaoddmuse-browse-page' or `yaoddmuse-browse-page-default',
     it will try to pick-up page name around point.
・ Encode special file:
     If you post special file, such as picture or compress file,
     it can encode file content before post it.
・ Redirect page:
     You can use command `yaoddmuse-redirect' redirect page.
     Just input page name that you want redirect to.
     You need input redirect from page if current buffer not
     `yaoddmuse' buffer.
・ Delete page:
     You can use command `yaoddmuse-delete' delete page.
     Just input page name that you want delete.
・ Insert special file:
     You can use command `yaoddmuse-insert-file-content' insert
     file content.
     This command will try to encode special file content, such as,
     picture or compress file.
・ Save page:
     You can use command `yaoddmuse-save-as' save special page,
     such as picture or compress format, and it will notify you
     correct suffix to save.
・ Toggle image view:
     By default, when got image page, it will decode image and view it.
     You can use command `yaoddmuse-toggle-image-status' to toggle
     image status for view different content.
