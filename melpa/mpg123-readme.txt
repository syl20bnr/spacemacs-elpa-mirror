
	This package is a front-end program to mpg123/ogg123 audio player.
	mpg123/ogg123 の再生フロントエンドです。

[Requirement]

	The `mpg123' program version 0.59q or later, or ogg123 version
	1.0 or later, and enough CPU power to run it.
	mpg123 0.59q以降 や ogg123 1.0以降 とそれを走らすのに十分な
	CPUパワー。最低でもMMX??

[Installation]

	You have to  install mpg123 0.59q or later,  ogg123 1.0 or later
	first, and get they work fine.  Check `mpg123 -v' or `ogg123 -v'
	option if it  displays the decoding frame number  or not.  If it
	looks good,  then the preparation  has been done.   Install this
	emacs-lisp   into  your  load-path   directory.   And   put  the
	expression below into your ~/.emacs.

	  [~/.emacs]
		(autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

	まず、mpg123あるいはogg123の正常動作を確認してから上の行を
	~/.emacsに追加します。なおmpg123は0.59q以上でないと正常に動作しな
	い可能性があります(もっと新しいのが出たらまた怪しいかもしれん…)。
	ogg123は1.0で動作確認してますがそれ以外でも大丈夫でしょう。
	mpg123(または ogg123)コマンド に -v オプションをつけて起動し音楽
	の再生とともにデコード中のフレーム番号(ogg123の場合は秒数)が画面
	に表示されるかどうか確認してください。これがうまく行かないとこの
	プログラムもうまく動きません。

[How to Play the music]

	It is  assumed that you  already have MPEG1  audio LayerI/II/III
	files - you might be only  familiar with MPEG1 Layer III aka mp3
	-  or Ogg  format music  files in  the certain  directory.  This
	program plays all music in  A direcotry.  If you want to listen,
	exec Emacs and type:

		M-x mpg123 RET
		SomeMP3DirectoryName (or playlist file) RET

	Then you will get the music  list in the directory.  Type SPC to
	start the  music.  All key bindings  are shown at  the bottom of
	music list buffer.  Please take a look at it.

	既に MPEG1 audio Layer I/II/III ファイルは持ってるものとして説明
	します(たぶんいわゆるMP3しか持ってないと思うけど気にしないわしも
	Layer2と3は作ったことすらない)。Ogg形式でももちろんおっけーよ。
	で、そのファイルはきっとどこかのディレクトリに整理しておいてある
	と思うので、音楽を聞きたくなったら、まずEmacsを起動し、

		M-x mpg123 ぺし
		ディレクトリ名 (またはプレイリストファイル名) ぺし

	と打ちます。と、そのディレクトリにある音楽ファイル一覧が出て来る
	ので、聞きたい曲に合わせてSPCを打つと演奏が始まります。その他の
	キーコマンドは音楽一覧バッファの末尾に表示されているのでそっちを
	見てください。

[Playlist]

	If you  give `M-x mpg123' a  simple file which  consists of file
	name list;  one file  name per line,  mpg123.el assumes it  as a
	playlist file.  All  of audio files listed in  playlist file are
	incorporated in *mpg123* playing  buffer.  If a line in playlist
	points  to another  playlist file,  file is  parsed recursively.
	There are mainly two ways to create a playlist file.

		* Typing `S' in *mpg123* buffer
		* Create directly on the shell;
		  Ex. % ls */*.mp3 */*.ogg > playlist

	Because a playlist is very simple, you can edit it manually to
	arrange the order of music list.

	M-x mpg123 のあとに、一行に一つMP3ファイルの名前が書かれた普通の
	ファイルを指定するとmpg123.elはそれをプレイリストファイルだとみ
	なし、そこに書かれている音楽ファイル群を *mpg123* 音楽一覧バッファ
	に全て取り込みます。プレイリストファイルを作るには二つの方法があ
	ります。

		* 音楽一覧(*mpg123*)バッファで S を押す
		* シェルの上で直接作る
		  【例】 % ls */*.mp3 */*.ogg > playlist

	プレイリストファイルはとても単純なので、直接編集して好きな曲順を
	リストを作るのは簡単でしょう。プレイリストファイル中に別のプレイ
	リストを指定することもできます。

[Configuration]

	Here are the variables for your customization.

	  [Variable]		[Default value/Meaning]
	  mpg123-default-dir	"~/mp3"
				Default directory of mp3 files
	  mpg123-mpg123-command	"mpg123"
				Command name of mpg123
	  mpg123-mpg123-command-args	nil
				Argument list to pass mpg123 command
	  mpg123-mpg123-id-coding-system	undefined
				Coding system for mp3 tag message
	  mpg123-ogg123-command	"ogg123"
				Command name of ogg123
	  mpg123-ogg123-command-args	nil
				Argument list to pass ogg123 command
	  mpg123-ogg123-id-coding-system	'junet  (or *junet* in mule2)
				Coding system for tag message(vorbiscomment)
	  mpg123-mixer-command	"mixer"
				Command name of mixer(FreeBSD)
	  mpg123-mixer-type	(Depends on the running system)
				One of 'mixer 'mixerctl 'aumix 'alsa 'apanel
				'mixer.exe 'audioctl
	  mpg123-mixer-maxvol	(Depends on the value of mpg123-mixer-type)
				Maximum volume permitted by the mixer utility
	  mpg123-preserve-playtime t
				If you want to mpg123 to play all music
				from the beginning even if the last
				played time is shown in the buffer, set
				this variable to `nil'.
	  mpg123-startup-volume 30
				Initialize sound volume with this value.
	  mpg123-default-repeat	0
				Default number of repetition
	  mpg123-show-help	t
				Whether show help under the music list
				or not
	  mpg123-omit-id3-artist nil
				Whether omit the artist name in ID3
	  mpg123-lazy-check	nil
				Check sound file or not by filename

	  mpg123-face-playing	'("yellow" . "#004080")
				Cons of default playing cursor color
				'(FGCOLOR . BGCOLOR)
	  mpg123-face-slider	'("black" . "yellow")
				Cons of default playing position slider color
	  mpg123-need-slider	t on color display, else nil
				Whether the playing position slider is
				needed or not
	  mpg123-display-slider	t
				For the buffer of large number of playlist,
				try to keep playing position slider visible
				or not.  'always for this varialbe always
				splits windows to make small window for
				slider.  Other non-nil values split window
				only if necessary. But if you run
				mpg123-mode in single frame, it becomes
				annoying because mpg123-display-slider brakes
				window configuration.
	  mpg123-auto-redraw	Redraw slider when resize window
	  mpg123-lang		Message language 0=English 1=Japanese
	  mpg123-lazy-slider	Reduce redrawing slider and windows to
				each 1 second

	あまりいじれるところ無いけど、上に書いてある変数がいじれます。

	なおLinuxでは音量調節コマンドとして aumix -w の利用を前提とします。
	このプログラムで音量をいじりたいときは aumix をインストールしてお
	きましょう。

[More comfortable]

	Yes, Emacs is the editor.  Even though you are listening to the
	music, you have to edit something!! :)

	This  program occupies  one  Emacs window.   Using this  program
	without any  window manager is  hard job.  Please use  this with
	windows.el  -  The Window  Manager  for  Emacs  - which  can  be
	obtained   from   http://www.gentei.org/~yuuji/software/.   With
	windows.el,  you can  listen the  music  which is  run in  other
	frame. Or  if you use  emacs -nw, you  can run mpg123.el  in the
	background  window and  can  switch from  and  to mpg123  buffer
	alternatively.   Of  course,  I'm  writing this  document  while
	mpg123.el is running in the background window.

	Emacs使ってるんだから聞くばっかりじゃなくて編集しなさい!  てこと
	で、全フレームを消費するmpg123.elを素のEmacsで使ってたら大変。た
	めしに windows.el と一緒につこてみてね。フレームを使ってるときは
	別フレームでバックグラウンド再生、-nw で起動しているときは裏ウィ
	ンドウでバックグラウンド再生できて、その裏ウィンドウといくつかの
	編集ウィンドウを切替えて使うなんて事も可能。もちろんこの文章も裏
	に半分隠れてるフレームでmpg123を走らせながら書いてます。
	windows.el は http://www.gentei.org/~yuuji/software/ からどうぞ。

[For Extension]

	If you want  to make an extension to  support other sound format
	and its player, you  should define some variables and functions.
	Suppose ".foo"  is sound file  name suffix, and "foo123"  is its
	player software.  Define as follows;

	mpg123-type-alist	Add cons of '("foo" . "foo123").
				This automaticall utilize variables
				and functions below.
	mpg123-foo123-command	"foo123"
	mpg123-foo123-command-args
	mpg123-foo123-time-regexp
	mpg123-foo123-init-frame-regexp
	mpg123-foo123-frame-regexp
	mpg123-foo123-convert-frame	Func: frame to serial number

	And you have to add modifications to these funcs and vars.

	mpg123:peek-tag, mpg123*time2frame-ratio-alist

	You may have to create functions as follows;

	mpg123:foo-p, mpg123:foo123-peek-tag

[Introduction-Quiz mode]

	When enable mpg123-introduction-quiz-mode  by typing "I", mpg123
	always stops at marked position  in order to wait until audience
	give an answer.

	To   prepare   introduction-quiz,   you   shoulde   mark   every
	`song-starting' position previously, and save the mpg123 playing
	buffer  by typing "S".   At the  day of  introduction-quiz game,
	load saved  playlist and get  music list in  mpg123-buffer.  All
	you have to do is to  play music until someone hit the answering
	button.  Typing `r' plays the body part of music.

	イントロクイズをするには……
	まず、
	全ての曲リストをmpg123バッファにロードします。そして、全ての曲に
	ついて歌い始めの位置を 'm' でマークしましょう。全てマーク付けが
	完了したら 'S' でプレイリストを保存します。イントロクイズ当日、
	クイズ作成者のあなたは 'I' をタイプしてイントロクイズモードにし
	ておきます。すると歌い始めの直前でmpg123は自動的に演奏を止めます。
	'r' をおすと歌い始め部分から再生します。これで大体イントロクイズ
	の進行ができるでしょう。それ以前に mpg123.el の操作にしっかり慣
	れておくのも重要です。


[Bugs]

	It is  perhaps only on  my system that sometimes  mpg123 command
	gets confused to decode and  ticks playing time very slowly.  In
	such case, mpg123.el cannot  detect that condition.  If you come
	to see such behavior, please pause and restart player by SPC key.

	たまにmpg123コマンドが動いてはいるものの音を出さなくなってしまう
	ことがあります。そのような挙動をmpg123.elは検出できないので、そ
	うなったらSPCで一旦止めて動かし直してください。Emacs19ベースの
	Muleでは複雑な理由により別フレームで演奏中に次の曲に進むと、次の
	曲に移った直後のキーを演奏用バッファに取られてしまい、なおかつ演
	奏時間の更新が(みかけ上)次にキー入力するまで止まってしまいます。
	そうなってしまう確率が下がるような工夫はしてみましたが根本的解決
	には至りませんでした。

	Play/Stop control against the  music in the stack buffer doesn't
	work.   Although  it is  feasible,  the  feature isn't  actually
	usefull and ends in self-satisfaction.  So, no plan to make it.

[No Warranty]

	This  program is  free  software and  comes  with absolutely  NO
	WARRANTY.   The  author  is  not responsible  for  any  possible
	defects  caused by this  software.  You  can freely  modify this
	program  for  your convenience.   But  if  you  want to  publish
	modified program,  please tell me before  announcement.  Take it
	easy to write me comments and bug-reports.
							yuuji@gentei.org

	このプログラムはフリーソフトウェアとして配布します。このプログラ
	ムの利用によって生じたいかなる結果に対しても作者は責任を負いませ
	ん。コメントやバグレポートはおおいに歓迎しますので御気軽に御連絡
	ください。またプログラムに対する個人的な修正は自由にして頂いて構
	いませんが、それを公開したい場合は私まで御連絡ください。連絡は以
	下のアドレスまでお願いします(2015/12現在)。
							yuuji@gentei.org
[Acknowledgements]

	Tijs van Bakel, <smoke>at<casema.net>
		Reported mpg123 termination problem on mpg123 0.59r on
		linux 2.2.10.
	sen_ml>at<eccosys.com
		Reported problem at playing music more than 100.
	Kenichi OKADA, <okada>at<opaopa.org>
		Sent a patch of setting sound volume on Solaris/sparc.
	Takuro Horikawa <takuroho>at<tky3.3web.ne.jp>
		Reported running on WinNT.
		Port `mixer command' to Windows.
		(See http://www3.tky.3web.ne.jp/~takuroho/mpg123.html)
	TAOKA Satoshi <taoka>at<infonets.hiroshima-u.ac.jp>
		Put mpg123.el into FreeBSD ports collection
	T. V. Raman <ramantv>at<earthlink.net>
		Made emacspeak-mpg123.el.  Many comments.
	Per Weijnitz <Per.Weijnitz>at<etl.ericsson.se>
		Sent a patch to enable mixer command on NT4
	Takayuki TSUKAGOSHI <tsuka>at<soft.ics.keio.ac.jp>
		Sent a patch for mule2@19.34.
	Ryuichi Arafune <arafune>at<debian.org>
		Put mpg123.el to Debian package.
	Laurent Martelli <martelli>at<iie.cnam.fr>
		Sent a patch of passing optional arguments to mpg123.
		Volume control for Linux.
	T. Amano <tomoo>at<cheri.sh>
		Reported running on Linux.
	OHTAKI Naoto <ohtaki>at<wig.nu>
		Reported running on Windows98
	MOROHOSHI Akihiko <moro>at<remus.dti.ne.jp>
		Sent a patch on coding-system detection for XEmacs+emu.el
		Fixed the failure of handling multi-byte chars in
		id3v1.1 support.
		Introduce mpg123-lazy-slider.
	Alex Shinn <foof>at<debian.org>
		Patch to handle mp3 files in multiple directories.
		Implemented `playlist'.
	Seiichi Namba <sn>at<asahi-net.email.ne.jp>
		Many collaboration codes for working with dired-dd.
		Made dired-dd-mpg123.
	Serge Arsenault <boggles>at<openface.ca>
		Sent information on OpenBSD.
	Toni Ronkko <tronkko>at<hytti.uku.fi>
		Many suggestions.
	SHIMADA Mitsunobu <simm>at<mbox.fan.gr.jp>
		Sent a patch of mpg123-auto-redraw and truncate-lines.
	N. SHIMIZU <CZA06074>at<nifty.com>
		Sent a patch to restore cursor position after id3-edit.
	HIROSE Yoshihide <yoshihide>at<fast.co.jp>
		Report and sent a patch for IRIX6.
	Andreas Fuchs <asf>at<acm.org>
		Support OggVorbis.
	Akinori MUSHA <knu>at<iDaemons.org>
		Sent a patch to read oggcomment correctly
	Yoichi NAKAYAMA <yoichi>at<eken.phys.nagoya-u.ac.jp>
		Fixed the bug when mpg123*use-face is nil.
		Fixed handling of mpg123*initial-buffer.
	Len Trigg <lenbok>at<myrealbox.com>
		Sent a patch and report on playlist file parsing.
		Remote control stuffs.
		Hooks for helper application.
	Rene Kyllingstad <kyllingstad>at<users.sourceforge.net>
		Many enhancements; mpg123-set-point-for-next-song-function,
		mpg123-format-name-function, using SIGTERM, id3v1.1,
		mpg123-now-playing, fixes for non-mule XEmacs.
	Hiroshi Imai <imai_hiroshi_niboshi>at<yahoo.co.jp>
		Suggested not to alter mixer volume in mpg123:initialize
		when mpg123-startup-volume is nil.
	Faraz Shahbazker <faraz.shahbazker>at<gmail.com>
		Sent a patch of new feature, `loop counter'.
	Thomas Morgan <tlm>at<thomasmorgan.net>
		Suggestion on mpg123-quit.
	Peter Lazar <pgl>at<bok.net>
		Suggestion on new keybindings '='.
	Vagn Johansen <vj>at<evalesco.net>
		Sent a patch for handling big mp3 files.


[History]
$Log: mpg123.el,v $
Revision 1.60  2015/12/04 10:05:43  yuuji
Fix error when ogg3info returns no music length.

Revision 1.59  2012/12/02 23:00:26  yuuji
Change header and section tags.  Nothing changed in codes.

Revision 1.58  2011/01/13 23:02:27  yuuji
Fix calculation overflow(int) in mpg123:time2frame. (thanks to vj)

Revision 1.57  2010/10/30 00:51:04  yuuji
"Unknown artist" suppression also for ogg.

Revision 1.56  2010/10/10 08:15:19  yuuji
Omit "Unknown artist"

Revision 1.55  2010/04/01 05:50:30  yuuji
"=" key also increase volume. (by pgl)

Revision 1.54  2010/03/11 15:31:08  yuuji
For emacs-23, try best to keep playing music line and time-slider visible.

Revision 1.53  2010/02/18 04:04:08  yuuji
New customizing variable mpg123-mixer-mixerctl-target

Revision 1.52  2009/10/04 12:22:32  yuuji
Check: (fboundp 'set-buffer-multibyte)

Revision 1.51  2008/05/12 15:14:46  yuuji
Fix behavior of mpg123-quit.

Revision 1.50  2007/05/27 16:54:19  yuuji
mpg123-smart-dnd-setup

Revision 1.49  2007/04/19 00:05:40  yuuji
New variable mpg123-mixer-type, mpg123-mixer-maxvol (by lenbok)

Revision 1.48  2007/03/21 15:06:22  yuuji
New hooks 'mpg123-song-started-hook and 'mpg123-song-finished-hook
introduced. (by lenbok@gmail.com)

Revision 1.47  2006/05/13 10:52:51  yuuji
Revise documentation.

Revision 1.46  2006/05/13 10:49:45  yuuji
New variable mpg123-file-name-coding-system might help correct
handling of multi-byte file names.
Using ogginfo if available, to get ogg files information.
mpg123:match-string, mpg123:buffer-substring.

Revision 1.45  2005/12/03 06:40:25  yuuji
Loop counter for current music introduced.
Thanks to Faraz Shahbazker.

Revision 1.44  2004/09/17 14:03:13  yuuji
Fixed argument handling in mpg123-delete-file.

Revision 1.43  2004/09/17 03:09:04  yuuji
* Support music longer than 99:59(not tested heavily).
* Add mpg123-active-p for external add-on's.
* Indicator and slider overlay can't be moved by insert-before-markers
  on XEmacs.  Fixed.

Revision 1.42  2004/02/12 07:09:31  yuuji
mpg123-display-slider (key bound to ".") introduced.
mpg123 now tries to keep slider visible.
"I" toggles introduction-quiz mode.
mpg123-save-playlist ("S") now saves all marked positions.

Revision 1.41  2004/02/10 15:56:22  yuuji
mpg123:draw-slider-help must always create mpg123*indicator-overlay.  Fixed.
mpg123:window-width must check window-width of mpg123*buffer.  Fixed.

Revision 1.40  2003/07/26 04:23:27  yuuji
Do not alter mixer volume in mpg123:initialize	when
mpg123-startup-volume is nil. Suggested by imai_hiroshi_niboshi@yahoo.co.jp.

Revision 1.39  2003/05/26 14:36:19  yuuji
Patch by moro@remus.dti.ne.jp;
 Fixed the failure of handling multi-byte chars in id3v1.1 support.
 Introduce mpg123-lazy-slider.

Revision 1.38  2003/04/28 09:20:48  yuuji
mpg123-set-point-for-next-song-function, mpg123-format-name-function,
mpg123-now-playing, support id3v1.1 (thanks to Rene Kyllingstad)

Revision 1.37  2003/03/31 02:31:33  yuuji
CR-LF(DOS) encoding playlist treated correctly. (by lenbok)

Revision 1.36  2003/03/28 16:00:58  yuuji
For XEmacs: 'no-conversion changed to 'binary.
`g' mpg123-goto-current-line.
Japanese messages.

Revision 1.35  2002/12/17 01:15:50  yuuji
mpg123:get-sound-type ignores case

Revision 1.34  2002/10/20 13:31:41  yuuji
* (mpg123:playlist-p): Save and restore current buffer.
* (mpg123-quit): Don't switch to killed buffer.
* (mpg123:create-buffer): Set mpg123*initial-buffer.
* Don't set mpg123*initial-buffer while loading mpg123.el.

(by Yoichi NAKAYAMA)

Revision 1.33  2002/09/27 09:09:34  yuuji
Multibyte music tag displayed wrong, fixed.
Variable `mpg123-ogg123-id-coding-system controls coding system
for vorbiscomment.

Revision 1.32  2002/09/25 05:50:21  yuuji
Fixed the bug when mpg123*use-face is nil.
Suggested by Yoichi NAKAYAMA <yoichi@eken.phys.nagoya-u.ac.jp>

Revision 1.31  2002/09/21 13:10:16  yuuji
Fixed documentation for mpg123:ogg123-peek-tag.

Revision 1.30  2002/09/21 12:56:27  yuuji
Picking ogg's comment starts from file offset 84.
Fixed by Akinori MUSHA <knu@iDaemons.org>.

Revision 1.29  2002/09/18 17:22:05  yuuji
Peek ogg's comment directly.
Revise document.

Revision 1.28  2002/09/14 12:59:08  yuuji
OggVorbis supported.

Revision 1.27  2002/09/14 12:55:52  yuuji
B/b rewind to the previous music if it reaches at the beginning of the
music.

Revision 1.26  2002/04/08 03:57:25  yuuji
IRIX 6.3 OK

Revision 1.25  2002/02/15 04:37:32  yuuji
mpg123-refresh-tag, mpg123-id3-edit by N. SHIMIZU <CZA06074@nifty.com>

Revision 1.24  2001/02/23 06:54:30  yuuji
Only `>' obeys repetition count.

Revision 1.23  2001/02/23 05:39:26  yuuji
Delete-file key-binding is changed from `C-d' to `D' for trivial reason..

Revision 1.22  2001/02/23 05:20:12  yuuji
`>' at the end of music list obeys the repetition counter.
Music list in a stack doesn't appear in the result of shuffle any more.
Shuffle preserves highlighted line any time.
Now nil for mpg123-preserve-playtime plays a music from the beginning.

Revision 1.21  2001/02/21 03:41:10  yuuji
Support for OpenBSD is confirmed.

Revision 1.20  2001/01/30 03:35:54  yuuji
(Win)convert music filename to dos file name for music over shared folder

Revision 1.19  2001/01/19 04:41:37  yuuji
Fix the invalid 'cond form.

Revision 1.18  2000/12/23 07:41:23  yuuji
Slider stays wrong position when music list added.  Fixed

Revision 1.17  2000/12/08 00:54:09  yuuji
Variable mpg123-face-playing specifies the color of cursor for playing music.
Variable mpg123-face-slider specifies the color of slider of playing position.
Variable mpg123-need-slider specifies wheter the slider is needed or not.
Mouse-2 selects directly a music on the mouse pointer(in music list) or
playing position(in delimiter line).
RET(M-x mpg123-play) on the delimiter line move the playing position
according to the proportion of the window width from left side.

Revision 1.16  2000/11/24 15:09:22  yuuji
Support emacs-21.0.9x (in mpg123:mp3-p)

Revision 1.15  2000/10/20 14:43:06  yuuji
(if (featurep 'xemacs) (require 'overlay))

Revision 1.14  2000/10/16 08:52:44  yuuji
'mpg123*cur-face renamed to 'mpg123-cur-face (For XEmacs)

Revision 1.13  2000/08/06 03:56:37  yuuji
Support volume setting on NetBSD(mixerctl)

Revision 1.12  2000/08/06 02:27:58  yuuji
Set it default to use hilighting.

Revision 1.11  2000/08/05 15:40:57  yuuji
Revise document.

Revision 1.10  2000/08/05 15:37:50  yuuji
Handle mp3 files in multiple directories.
Playlist support.

Revision 1.9  2000/06/25 14:38:17  yuuji
Fix for XEmacs+emu.el

Revision 1.8  2000/02/09 04:15:31  yuuji
Fix for mule2 (mpg123:sound-p).

Revision 1.7  1999/09/25 07:09:44  yuuji
mpg123-delete-file can delete music only from the list, not on the disk.
Shuffle after mpg123-delete-file now works correctly.

Revision 1.6  1999/09/10 02:09:02  yuuji
mpg123-mp3-scan-bytes
defmacro changed to defsubst

Revision 1.5  1999/07/24 03:58:52  yuuji
mule2でなるべく曲連係が途切れないように工夫(完璧ではない)。

Revision 1.4  1999/07/05 09:00:19  yuuji
日本語ファイル名対応(たぶん)
\C-d (mpg123-delete-file)
