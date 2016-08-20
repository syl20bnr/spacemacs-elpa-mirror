This makes elscreen persistent.

To use this, use customize to turn on `elscreen-persist-mode`
or add the following line somewhere in your init file:

    (elscreen-persist-mode 1)

Or manually, use `elscreen-persist-store` to store,
and use `elscreen-persist-restore` to restore.

Or manually, use `elscreen-persist-get-data` to get data to store,
and use `elscreen-persist-set-data` to set data to restore.

Please see README.md from the same repository for documentation.
