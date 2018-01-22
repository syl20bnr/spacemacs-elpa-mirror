* company-childframe README                                :README:
** What is company-childframe
company-childframe is a company extension, which let company use
child frame as its candidate menu.

It has the following feature:
1. It is more fast than the company default candidate menu.
2. It works well with CJK language.

** How to use company-childframe

#+BEGIN_EXAMPLE
(require 'company-childframe)
(company-childframe-mode 1)
#+END_EXAMPLE

** Note
company-childframe.el is derived from Clément Pit-Claudel's
company-tooltip.el, which can be found at:

https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511
