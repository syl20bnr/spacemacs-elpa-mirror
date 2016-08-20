             ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                         `ORG-PASSWORD-MANAGER'
              Minimal password manager for Emacs Org Mode.

                          Leandro Facchinetti
             ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


Table of Contents
─────────────────

1 Philosophy
2 Features
3 Installation
4 Usage
.. 4.1 Store passwords in Org Mode files
.. 4.2 Get username
.. 4.3 Get password
.. 4.4 Generate password
5 Configuration
6 Comparison to similar tools

Table of Contents
─────────────────

1 Philosophy
2 Features
3 Installation
4 Usage
.. 4.1 Store passwords in Org Mode files
.. 4.2 Get username
.. 4.3 Get password
.. 4.4 Generate password
5 Configuration
6 Comparison to similar tools

1 Philosophy
════════════

  Simple: To learn and use.

  Concise: Don’t recreate features that already exist elsewhere
           (e.g. secure password generation).

  Secure: Don’t do fancy security measures, they should be handled by
          specialized tools (e.g. [GnuPG]).

  Flexible: Restrict the users the least possible.


  [GnuPG] https://gnupg.org/


2 Features
══════════

  1. Use [Org Mode] as password manager.

  2. Retrieve passwords in a practical and secure manner.

  3. Generate secure passwords.

  4. No configuration required.


  [Org Mode] http://orgmode.org/


3 Installation
══════════════

  Available in [MELPA]. Install with `M-x package-install'.

  Password generation depends on [`pwgen']. If you want to use this
  feature, install it.

  If you want to use the default keybindings described below on the
  [Usage] section, add the following line to your Emacs configuration:

  ┌────
  │ (add-hook 'org-mode-hook 'org-password-manager-key-bindings)
  └────

  If you want some other keybindings, refer to the body of the function
  `org-password-manager-key-bindings' for an example on how to do it.

  If you want [`ido'] completion, enable the `org-completion-use-ido'
  variable by adding the following line to your Emacs configuration:

  ┌────
  │ (setq org-completion-use-ido t)
  └────


  [MELPA] http://melpa.org/#/org-password-manager

  [`pwgen'] http://pwgen.sourceforge.net/

  [Usage] See section 4

  [`ido'] https://www.gnu.org/software/emacs/manual/ido.html


4 Usage
═══════




4.1 Store passwords in Org Mode files
─────────────────────────────────────

  Follow the example:

  ┌────
  │ * [[http://example.com][My favorite website]]
  │   :PROPERTIES:
  │   :USERNAME: leandro
  │   :PASSWORD: chunky-tempeh
  │   :END:
  |
  │ * SSH key
  │   :PROPERTIES:
  │   :PASSWORD: tofu
  │   :END:
  └────


4.2 Get username
────────────────

  Type `C-c C-p u' (`org-password-manager-get-username') and search for
  the title of the entry containing the `USERNAME' property (e.g. “My
  favorite website”). The username is copied to the clipboard.

  If the point is at an entry that contains the `USERNAME' property, it
  is copied without querying for the heading. If you still want to be
  queried (because you want the username for a different entry) use the
  `C-u' argument typing `C-u C-c C-p u'.


4.3 Get password
────────────────

  Type `C-c C-p p' (`org-password-manager-get-password') and search for
  the title of the entry containing the `PASSWORD' property (e.g. “My
  favorite website”). The password is copied to the clipboard. It tries
  to increase the security by skipping the kill ring and copying the
  password directly to the system’s clipboard and by erasing it after 30
  seconds. This period is customizable, refer to the [Configuration]
  section for more.

  If the point is at an entry that contains the `PASSWORD' property, it
  is copied without querying for the heading. If you still want to be
  queried (because you want the password for a different entry) use the
  `C-u' argument typing `C-u C-c C-p u'.


  [Configuration] See section 5


4.4 Generate password
─────────────────────

  Type `C-c C-p g' (`org-password-manager-generate-password') and the
  generated password is inserted under the point on the buffer. It is
  also copied to your clipboard. It tries to increase the security by
  skipping the kill ring and copying the password directly to the
  system’s clipboard and by erasing it after 30 seconds. This period is
  customizable, refer to the [Configuration] section for more.

  If you want to customize the `pwgen' command before running it
  (e.g. you want a shorter password), use the `C-u' argument by typing
  `C-u C-c C-p g'.


  [Configuration] See section 5


5 Configuration
═══════════════

  Refer to `M-x customize-group org-password-manager'.


6 Comparison to similar tools
═════════════════════════════

  This work was first inspired by [Emacs] and [Org mode], obviously.

  But I also want to cite two other projects that are similar in spirit
  to `org-password-manager'. They aim to accomplish the same
  goal—i.e. using [Emacs] [Org mode] as a password manager. Though they
  differ on design from each other and from
  `org-password-manager'. Thus, the effort to create
  `org-password-manager' is still justified.

  Those related projects are both called `org-passwords'. One is by
  [Jorge Alfaro-Murillo] and the other by [Andrea Crotti].

  [Jorge Alfaro-Murillo's `org-passwords'] has lots of features, way
  more than `org-password-manager' plans to have. For example, it
  implements its own password generator, requires configuration for
  pointing to a password file that should only contain passwords and
  opens that file in read-only mode with a timeout. It is so complete
  that it is in the official distribution of [Org mode] under
  [org-contrib].

  `org-password-manager', on the other hand, uses [pwgen] to generate
  passwords, handles passwords stored on the middle of any [Org mode]
  file with other contents and doesn’t open those files in any special
  way.

  [Andrea Crotti's `org-passwords'] is more minimal than
  `org-password-manager' aims to be. It only retrieves passwords for the
  entry under the point, generates passwords by calling [pwgen] and has
  almost no documentation, requiring the user to read the source.

  I appreciate the mentioned works and thank its authors.


  [Emacs] https://www.gnu.org/software/emacs/

  [Org mode] http://orgmode.org/

  [Jorge Alfaro-Murillo]
  https://bitbucket.org/alfaromurillo/org-passwords.el

  [Andrea Crotti] https://github.com/AndreaCrotti/org-passwords/

  [Jorge Alfaro-Murillo's `org-passwords']
  https://bitbucket.org/alfaromurillo/org-passwords.el

  [org-contrib]
  http://orgmode.org/cgit.cgi/org-mode.git/tree/contrib/lisp/org-passwords.el

  [pwgen] http://pwgen.sourceforge.net/

  [Andrea Crotti's `org-passwords']
  https://github.com/AndreaCrotti/org-passwords/
