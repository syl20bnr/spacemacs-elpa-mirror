This package provides a comment viewer of Niconico Live Broadcast <http://live.nicovideo.jp/>.
To use, require this script and configure your Niconico account like this.

(setq niconama-user "your@account.com")

And then, type M-x niconama-comment-viewer to activate comment viewer.
C-RET in "Write Comment" buffer submit the contents of this buffer to broadcast.

To kill the comment viewer, use M-x niconama-kill-comment-viewer and type process number
shown in comment viewer buffer name as "niconama-comment-viewer" (process number is 0) or
"niconama-comment-viewer <n>" (process number is n).
