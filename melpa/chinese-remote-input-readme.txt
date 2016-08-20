Chinese-remote-input 可以让用户通过智能手机输入法（比如 Andorid 语音输入法）来远程输入中文。
其工作原理是：

1. 在当前工作的计算机上安装ssh服务器。
2. 在Android手机中安装ssh客户端，比如：JuiceSSH 或者 ConnectBot等。
3. 在计算机上开启 emacs-daemon，打开待录入文件，并通过命令 `remote-input-toggle' 激活 Chinese-remote-input。
4. 从手机上远程登录计算机，并运行一个 emacsclient，然后通过命令 `remote-input-terminal' 开启一个中文远程输入终端。
5. 在中文远程输入终端中输入中文后按回车键，当前行对应的中文字符串就会插入到待编辑文件的光标处。
