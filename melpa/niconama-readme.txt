This package provides a comment viewer of Niconico Live Broadcast <http://live.nicovideo.jp/>.
To use, require this script and configure your Niconico account like this.

(setq niconama-user "your@account.com")

And then, type M-x niconama-comment-viewer to activate comment viewer.
C-RET in "Write Comment" buffer submit the contents of this buffer to broadcast.
If you want to switch 184 attribute of comment, use C-c C-y.

To kill the comment viewer, use M-x niconama-kill-comment-viewer and type process number
shown in comment viewer buffer name as <process number>: <broadcast title>.
