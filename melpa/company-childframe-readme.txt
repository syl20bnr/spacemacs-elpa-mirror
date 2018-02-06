* company-childframe README                                :README:
** What is company-childframe
company-childframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:
1. It is fast enough for daily use.
2. It works well with CJK language.

*At the moment*, company-childframe can not work well with:
1. company-quickhelp

** How to use company-childframe

#+BEGIN_EXAMPLE
(require 'company-childframe)
(company-childframe-mode 1)
#+END_EXAMPLE

** Tips
*** Work better with desktop.el
The below code let desktop.el not record the company-childframe-mode
#+BEGIN_EXAMPLE
(require 'desktop) ;this line is needed.
(push '(company-childframe-mode . nil)
      desktop-minor-mode-table)
#+END_EXAMPLE

** Note
company-childframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511
