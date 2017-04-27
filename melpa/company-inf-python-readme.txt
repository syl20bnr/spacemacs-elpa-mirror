Inferior python backend for company-mode.  Allow company-mode
completions in inferior python mode buffers based on runtime
definitions.

Add company-inf-python to allowed company-mode backends list

    (add-to-list 'company-backends 'company-inf-python)
