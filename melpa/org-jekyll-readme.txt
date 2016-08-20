Extract subtrees from your org-publish project files that have
a :blog: keyword and an :on: property with a timestamp, and
export them to a subdirectory _posts of your project's publishing
directory in the year-month-day-title.html format that Jekyll
expects.  Properties are passed over as yaml front-matter in the
exported files.  The title of the subtree is the title of the
entry.  The title of the post is a link to the post's page.

Look at http://orgmode.org/worg/org-tutorials/org-jekyll.html for
more info on how to integrate org-mode with Jekyll, and for the
inspiration of the main function down there.
