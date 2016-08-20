clean-buffers is a little tool that used to clean useless buffers
which means buffers who's name match specify regex
(see `clean-buffer-useless-buffer-names')
or undisplayed time exceeded certain time
(see `clean-buffer-useless-buffer-timeout')

Quick start:

config `useless-buffer-names' or `useless-buffer-time-out' and then
execute the following commands:
`clean-buffers-kill-useless-buffers' to clean useless buffers
or `clean-buffers-turn-on-auto-clean-buffers' to clean useless buffers automatically
