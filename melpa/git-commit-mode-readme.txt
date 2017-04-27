A major mode for editing Git commit messages.

Formatting

Highlight the formatting of git commit messages and indicate errors according
to the guidelines for commit messages (see
http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).

Highlight the first line (aka "summary") specially if it exceeds 50
characters (configurable using `git-commit-summary-max-length').

Enable `auto-fill-mode' and set the `fill-column' to 72 according to the
aforementioned guidelines (configurable using `git-commit-fill-column').

Headers

Provide commands to insert standard headers into commit messages.

- C-c C-s inserts Signed-off-by (`git-commit-signoff').
- C-C C-a inserts Acked-by (`git-commit-ack').
- C-c C-t inserts Tested-by (`git-commit-test').
- C-c C-r inserts Reviewed-by (`git-commit-review').
- C-c C-o inserts Cc (`git-commit-cc').
- C-c C-p inserts Reported-by (`git-commit-reported').

Committing

C-c C-c finishes a commit.

Check a buffer for stylistic errors before committing, and ask for
confirmation before committing with style errors.
