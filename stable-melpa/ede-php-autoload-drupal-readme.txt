Add support for Drupal 8+ to ede-php-autoload.

Supplies ede-php-autoload with information on the classes located
in modules in a Drupal project. All modules found in core/modules,
modules, profiles/<profile>/modules and sites/<sites, including
default and all>/modules is added.

As Drupal uses composer/installers,
ede-php-autoload-composer-installers is required for practical use.
