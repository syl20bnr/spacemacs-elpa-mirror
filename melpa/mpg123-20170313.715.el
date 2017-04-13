;;; mpg123.el --- A front-end program to mpg123/ogg123 -*- coding: euc-jp -*-
;; Package-Version: 20170313.715
;;; (c)1999-2015 by HIROSE Yuuji [yuuji@gentei.org]
;;; $Id: mpg123.el,v 1.61 2017/02/21 08:06:31 yuuji Exp $
;;; Last modified Fri Dec  4 19:03:46 2015 on firestorm
;;; Update count: 1386

;;; Commentary:
;;	
;;	This package is a front-end program to mpg123/ogg123 audio player.
;;	mpg123/ogg123 の再生フロントエンドです。
;;	
;;[Requirement]
;;	
;;	The `mpg123' program version 0.59q or later, or ogg123 version
;;	1.0 or later, and enough CPU power to run it.
;;	mpg123 0.59q以降 や ogg123 1.0以降 とそれを走らすのに十分な
;;	CPUパワー。最低でもMMX??
;;	
;;[Installation]
;;	
;;	You have to  install mpg123 0.59q or later,  ogg123 1.0 or later
;;	first, and get they work fine.  Check `mpg123 -v' or `ogg123 -v'
;;	option if it  displays the decoding frame number  or not.  If it
;;	looks good,  then the preparation  has been done.   Install this
;;	emacs-lisp   into  your  load-path   directory.   And   put  the
;;	expression below into your ~/.emacs.
;;	
;;	  [~/.emacs]
;;		(autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)
;;	
;;	まず、mpg123あるいはogg123の正常動作を確認してから上の行を
;;	~/.emacsに追加します。なおmpg123は0.59q以上でないと正常に動作しな
;;	い可能性があります(もっと新しいのが出たらまた怪しいかもしれん…)。
;;	ogg123は1.0で動作確認してますがそれ以外でも大丈夫でしょう。
;;	mpg123(または ogg123)コマンド に -v オプションをつけて起動し音楽
;;	の再生とともにデコード中のフレーム番号(ogg123の場合は秒数)が画面
;;	に表示されるかどうか確認してください。これがうまく行かないとこの
;;	プログラムもうまく動きません。
;;	
;;[How to Play the music]
;;	
;;	It is  assumed that you  already have MPEG1  audio LayerI/II/III
;;	files - you might be only  familiar with MPEG1 Layer III aka mp3
;;	-  or Ogg  format music  files in  the certain  directory.  This
;;	program plays all music in  A direcotry.  If you want to listen,
;;	exec Emacs and type:
;;	
;;		M-x mpg123 RET
;;		SomeMP3DirectoryName (or playlist file) RET
;;	
;;	Then you will get the music  list in the directory.  Type SPC to
;;	start the  music.  All key bindings  are shown at  the bottom of
;;	music list buffer.  Please take a look at it.
;;	
;;	既に MPEG1 audio Layer I/II/III ファイルは持ってるものとして説明
;;	します(たぶんいわゆるMP3しか持ってないと思うけど気にしないわしも
;;	Layer2と3は作ったことすらない)。Ogg形式でももちろんおっけーよ。
;;	で、そのファイルはきっとどこかのディレクトリに整理しておいてある
;;	と思うので、音楽を聞きたくなったら、まずEmacsを起動し、
;;	
;;		M-x mpg123 ぺし
;;		ディレクトリ名 (またはプレイリストファイル名) ぺし
;;	
;;	と打ちます。と、そのディレクトリにある音楽ファイル一覧が出て来る
;;	ので、聞きたい曲に合わせてSPCを打つと演奏が始まります。その他の
;;	キーコマンドは音楽一覧バッファの末尾に表示されているのでそっちを
;;	見てください。
;;	
;;[Playlist]
;;	
;;	If you  give `M-x mpg123' a  simple file which  consists of file
;;	name list;  one file  name per line,  mpg123.el assumes it  as a
;;	playlist file.  All  of audio files listed in  playlist file are
;;	incorporated in *mpg123* playing  buffer.  If a line in playlist
;;	points  to another  playlist file,  file is  parsed recursively.
;;	There are mainly two ways to create a playlist file.
;;	
;;		* Typing `S' in *mpg123* buffer
;;		* Create directly on the shell;
;;		  Ex. % ls */*.mp3 */*.ogg > playlist
;;	
;;	Because a playlist is very simple, you can edit it manually to
;;	arrange the order of music list.
;;	
;;	M-x mpg123 のあとに、一行に一つMP3ファイルの名前が書かれた普通の
;;	ファイルを指定するとmpg123.elはそれをプレイリストファイルだとみ
;;	なし、そこに書かれている音楽ファイル群を *mpg123* 音楽一覧バッファ
;;	に全て取り込みます。プレイリストファイルを作るには二つの方法があ
;;	ります。
;;	
;;		* 音楽一覧(*mpg123*)バッファで S を押す
;;		* シェルの上で直接作る
;;		  【例】 % ls */*.mp3 */*.ogg > playlist
;;	
;;	プレイリストファイルはとても単純なので、直接編集して好きな曲順を
;;	リストを作るのは簡単でしょう。プレイリストファイル中に別のプレイ
;;	リストを指定することもできます。
;;	
;;[Configuration]
;;	
;;	Here are the variables for your customization.
;;	
;;	  [Variable]		[Default value/Meaning]
;;	  mpg123-default-dir	"~/mp3"
;;				Default directory of mp3 files
;;	  mpg123-mpg123-command	"mpg123"
;;				Command name of mpg123
;;	  mpg123-mpg123-command-args	nil
;;				Argument list to pass mpg123 command
;;	  mpg123-mpg123-id-coding-system	undefined
;;				Coding system for mp3 tag message
;;	  mpg123-ogg123-command	"ogg123"
;;				Command name of ogg123
;;	  mpg123-ogg123-command-args	nil
;;				Argument list to pass ogg123 command
;;	  mpg123-ogg123-id-coding-system	'junet  (or *junet* in mule2)
;;				Coding system for tag message(vorbiscomment)
;;	  mpg123-mixer-command	"mixer"
;;				Command name of mixer(FreeBSD)
;;	  mpg123-mixer-type	(Depends on the running system)
;;				One of 'mixer 'mixerctl 'aumix 'alsa 'apanel
;;				'mixer.exe 'audioctl
;;	  mpg123-mixer-maxvol	(Depends on the value of mpg123-mixer-type)
;;				Maximum volume permitted by the mixer utility
;;	  mpg123-preserve-playtime t
;;				If you want to mpg123 to play all music
;;				from the beginning even if the last
;;				played time is shown in the buffer, set
;;				this variable to `nil'.
;;	  mpg123-startup-volume 30
;;				Initialize sound volume with this value.
;;	  mpg123-default-repeat	0
;;				Default number of repetition
;;	  mpg123-show-help	t
;;				Whether show help under the music list
;;				or not
;;	  mpg123-omit-id3-artist nil
;;				Whether omit the artist name in ID3
;;	  mpg123-lazy-check	nil
;;				Check sound file or not by filename
;;	
;;	  mpg123-face-playing	'("yellow" . "#004080")
;;				Cons of default playing cursor color
;;				'(FGCOLOR . BGCOLOR)
;;	  mpg123-face-slider	'("black" . "yellow")
;;				Cons of default playing position slider color
;;	  mpg123-need-slider	t on color display, else nil
;;				Whether the playing position slider is
;;				needed or not
;;	  mpg123-display-slider	t
;;				For the buffer of large number of playlist,
;;				try to keep playing position slider visible
;;				or not.  'always for this varialbe always
;;				splits windows to make small window for
;;				slider.  Other non-nil values split window
;;				only if necessary. But if you run
;;				mpg123-mode in single frame, it becomes 
;;				annoying because mpg123-display-slider brakes
;;				window configuration.
;;	  mpg123-auto-redraw	Redraw slider when resize window
;;	  mpg123-lang		Message language 0=English 1=Japanese
;;	  mpg123-lazy-slider	Reduce redrawing slider and windows to
;;				each 1 second
;;	
;;	あまりいじれるところ無いけど、上に書いてある変数がいじれます。
;;	
;;	なおLinuxでは音量調節コマンドとして aumix -w の利用を前提とします。
;;	このプログラムで音量をいじりたいときは aumix をインストールしてお
;;	きましょう。
;;	
;;[More comfortable]
;;	
;;	Yes, Emacs is the editor.  Even though you are listening to the
;;	music, you have to edit something!! :)
;;	
;;	This  program occupies  one  Emacs window.   Using this  program
;;	without any  window manager is  hard job.  Please use  this with
;;	windows.el  -  The Window  Manager  for  Emacs  - which  can  be
;;	obtained   from   http://www.gentei.org/~yuuji/software/.   With
;;	windows.el,  you can  listen the  music  which is  run in  other
;;	frame. Or  if you use  emacs -nw, you  can run mpg123.el  in the
;;	background  window and  can  switch from  and  to mpg123  buffer
;;	alternatively.   Of  course,  I'm  writing this  document  while
;;	mpg123.el is running in the background window.
;;	
;;	Emacs使ってるんだから聞くばっかりじゃなくて編集しなさい!  てこと
;;	で、全フレームを消費するmpg123.elを素のEmacsで使ってたら大変。た
;;	めしに windows.el と一緒につこてみてね。フレームを使ってるときは
;;	別フレームでバックグラウンド再生、-nw で起動しているときは裏ウィ
;;	ンドウでバックグラウンド再生できて、その裏ウィンドウといくつかの
;;	編集ウィンドウを切替えて使うなんて事も可能。もちろんこの文章も裏
;;	に半分隠れてるフレームでmpg123を走らせながら書いてます。
;;	windows.el は http://www.gentei.org/~yuuji/software/ からどうぞ。
;;	
;;[For Extension]
;;	
;;	If you want  to make an extension to  support other sound format
;;	and its player, you  should define some variables and functions.
;;	Suppose ".foo"  is sound file  name suffix, and "foo123"  is its
;;	player software.  Define as follows;
;;	
;;	mpg123-type-alist	Add cons of '("foo" . "foo123").
;;				This automaticall utilize variables
;;				and functions below.
;;	mpg123-foo123-command	"foo123"
;;	mpg123-foo123-command-args
;;	mpg123-foo123-time-regexp
;;	mpg123-foo123-init-frame-regexp
;;	mpg123-foo123-frame-regexp
;;	mpg123-foo123-convert-frame	Func: frame to serial number
;;	
;;	And you have to add modifications to these funcs and vars.
;;	
;;	mpg123:peek-tag, mpg123*time2frame-ratio-alist
;;	
;;	You may have to create functions as follows;
;;	
;;	mpg123:foo-p, mpg123:foo123-peek-tag
;;	
;;[Introduction-Quiz mode]
;;	
;;	When enable mpg123-introduction-quiz-mode  by typing "I", mpg123
;;	always stops at marked position  in order to wait until audience
;;	give an answer.
;;	
;;	To   prepare   introduction-quiz,   you   shoulde   mark   every
;;	`song-starting' position previously, and save the mpg123 playing
;;	buffer  by typing "S".   At the  day of  introduction-quiz game,
;;	load saved  playlist and get  music list in  mpg123-buffer.  All
;;	you have to do is to  play music until someone hit the answering
;;	button.  Typing `r' plays the body part of music.
;;	
;;	イントロクイズをするには……
;;	まず、
;;	全ての曲リストをmpg123バッファにロードします。そして、全ての曲に
;;	ついて歌い始めの位置を 'm' でマークしましょう。全てマーク付けが
;;	完了したら 'S' でプレイリストを保存します。イントロクイズ当日、
;;	クイズ作成者のあなたは 'I' をタイプしてイントロクイズモードにし
;;	ておきます。すると歌い始めの直前でmpg123は自動的に演奏を止めます。
;;	'r' をおすと歌い始め部分から再生します。これで大体イントロクイズ
;;	の進行ができるでしょう。それ以前に mpg123.el の操作にしっかり慣
;;	れておくのも重要です。
;;	
;;	
;;[Bugs]
;;	
;;	It is  perhaps only on  my system that sometimes  mpg123 command
;;	gets confused to decode and  ticks playing time very slowly.  In
;;	such case, mpg123.el cannot  detect that condition.  If you come
;;	to see such behavior, please pause and restart player by SPC key.
;;	
;;	たまにmpg123コマンドが動いてはいるものの音を出さなくなってしまう
;;	ことがあります。そのような挙動をmpg123.elは検出できないので、そ
;;	うなったらSPCで一旦止めて動かし直してください。Emacs19ベースの
;;	Muleでは複雑な理由により別フレームで演奏中に次の曲に進むと、次の
;;	曲に移った直後のキーを演奏用バッファに取られてしまい、なおかつ演
;;	奏時間の更新が(みかけ上)次にキー入力するまで止まってしまいます。
;;	そうなってしまう確率が下がるような工夫はしてみましたが根本的解決
;;	には至りませんでした。
;;	
;;	Play/Stop control against the  music in the stack buffer doesn't
;;	work.   Although  it is  feasible,  the  feature isn't  actually
;;	usefull and ends in self-satisfaction.  So, no plan to make it.
;;	
;;[No Warranty]
;;	
;;	This  program is  free  software and  comes  with absolutely  NO
;;	WARRANTY.   The  author  is  not responsible  for  any  possible
;;	defects  caused by this  software.  You  can freely  modify this
;;	program  for  your convenience.   But  if  you  want to  publish
;;	modified program,  please tell me before  announcement.  Take it
;;	easy to write me comments and bug-reports.
;;							yuuji@gentei.org
;;	
;;	このプログラムはフリーソフトウェアとして配布します。このプログラ
;;	ムの利用によって生じたいかなる結果に対しても作者は責任を負いませ
;;	ん。コメントやバグレポートはおおいに歓迎しますので御気軽に御連絡
;;	ください。またプログラムに対する個人的な修正は自由にして頂いて構
;;	いませんが、それを公開したい場合は私まで御連絡ください。連絡は以
;;	下のアドレスまでお願いします(2015/12現在)。
;;							yuuji@gentei.org
;;[Acknowledgements]
;;	
;;	Tijs van Bakel, <smoke>at<casema.net>
;;		Reported mpg123 termination problem on mpg123 0.59r on
;;		linux 2.2.10.
;;	sen_ml>at<eccosys.com
;;		Reported problem at playing music more than 100.
;;	Kenichi OKADA, <okada>at<opaopa.org>
;;		Sent a patch of setting sound volume on Solaris/sparc.
;;	Takuro Horikawa <takuroho>at<tky3.3web.ne.jp>
;;		Reported running on WinNT.
;;		Port `mixer command' to Windows.
;;		(See http://www3.tky.3web.ne.jp/~takuroho/mpg123.html)
;;	TAOKA Satoshi <taoka>at<infonets.hiroshima-u.ac.jp>
;;		Put mpg123.el into FreeBSD ports collection
;;	T. V. Raman <ramantv>at<earthlink.net>
;;		Made emacspeak-mpg123.el.  Many comments.
;;	Per Weijnitz <Per.Weijnitz>at<etl.ericsson.se>
;;		Sent a patch to enable mixer command on NT4
;;	Takayuki TSUKAGOSHI <tsuka>at<soft.ics.keio.ac.jp>
;;		Sent a patch for mule2@19.34.
;;	Ryuichi Arafune <arafune>at<debian.org>
;;		Put mpg123.el to Debian package.
;;	Laurent Martelli <martelli>at<iie.cnam.fr>
;;		Sent a patch of passing optional arguments to mpg123.
;;		Volume control for Linux.
;;	T. Amano <tomoo>at<cheri.sh>
;;		Reported running on Linux.
;;	OHTAKI Naoto <ohtaki>at<wig.nu>
;;		Reported running on Windows98
;;	MOROHOSHI Akihiko <moro>at<remus.dti.ne.jp>
;;		Sent a patch on coding-system detection for XEmacs+emu.el
;;		Fixed the failure of handling multi-byte chars in
;;		id3v1.1 support.
;;		Introduce mpg123-lazy-slider.
;;	Alex Shinn <foof>at<debian.org>
;;		Patch to handle mp3 files in multiple directories.
;;		Implemented `playlist'.
;;	Seiichi Namba <sn>at<asahi-net.email.ne.jp>
;;		Many collaboration codes for working with dired-dd.
;;		Made dired-dd-mpg123.
;;	Serge Arsenault <boggles>at<openface.ca>
;;		Sent information on OpenBSD.
;;	Toni Ronkko <tronkko>at<hytti.uku.fi>
;;		Many suggestions.
;;	SHIMADA Mitsunobu <simm>at<mbox.fan.gr.jp>
;;		Sent a patch of mpg123-auto-redraw and truncate-lines.
;;	N. SHIMIZU <CZA06074>at<nifty.com>
;;		Sent a patch to restore cursor position after id3-edit.
;;	HIROSE Yoshihide <yoshihide>at<fast.co.jp>
;;		Report and sent a patch for IRIX6.
;;	Andreas Fuchs <asf>at<acm.org>
;;		Support OggVorbis.
;;	Akinori MUSHA <knu>at<iDaemons.org>
;;		Sent a patch to read oggcomment correctly
;;	Yoichi NAKAYAMA <yoichi>at<eken.phys.nagoya-u.ac.jp>
;;		Fixed the bug when mpg123*use-face is nil.
;;		Fixed handling of mpg123*initial-buffer.
;;	Len Trigg <lenbok>at<myrealbox.com>
;;		Sent a patch and report on playlist file parsing.
;;		Remote control stuffs.
;;		Hooks for helper application.
;;	Rene Kyllingstad <kyllingstad>at<users.sourceforge.net>
;;		Many enhancements; mpg123-set-point-for-next-song-function,
;;		mpg123-format-name-function, using SIGTERM, id3v1.1,
;;		mpg123-now-playing, fixes for non-mule XEmacs.
;;	Hiroshi Imai <imai_hiroshi_niboshi>at<yahoo.co.jp>
;;		Suggested not to alter mixer volume in mpg123:initialize
;;		when mpg123-startup-volume is nil.
;;	Faraz Shahbazker <faraz.shahbazker>at<gmail.com>
;;		Sent a patch of new feature, `loop counter'.
;;	Thomas Morgan <tlm>at<thomasmorgan.net>
;;		Suggestion on mpg123-quit.
;;	Peter Lazar <pgl>at<bok.net>
;;		Suggestion on new keybindings '='.
;;	Vagn Johansen <vj>at<evalesco.net>
;;		Sent a patch for handling big mp3 files.
;;
;;
;;[History]
;; $Log: mpg123.el,v $
;; Revision 1.61  2017/02/21 08:06:31  yuuji
;; Summary: string-to-int -> string-to-number
;;
;; Revision 1.60  2015/12/04 10:05:43  yuuji
;; Fix error when ogg3info returns no music length.
;;
;; Revision 1.59  2012/12/02 23:00:26  yuuji
;; Change header and section tags.  Nothing changed in codes.
;;
;; Revision 1.58  2011/01/13 23:02:27  yuuji
;; Fix calculation overflow(int) in mpg123:time2frame. (thanks to vj)
;;
;; Revision 1.57  2010/10/30 00:51:04  yuuji
;; "Unknown artist" suppression also for ogg.
;;
;; Revision 1.56  2010/10/10 08:15:19  yuuji
;; Omit "Unknown artist"
;;
;; Revision 1.55  2010/04/01 05:50:30  yuuji
;; "=" key also increase volume. (by pgl)
;;
;; Revision 1.54  2010/03/11 15:31:08  yuuji
;; For emacs-23, try best to keep playing music line and time-slider visible.
;;
;; Revision 1.53  2010/02/18 04:04:08  yuuji
;; New customizing variable mpg123-mixer-mixerctl-target
;;
;; Revision 1.52  2009/10/04 12:22:32  yuuji
;; Check: (fboundp 'set-buffer-multibyte)
;;
;; Revision 1.51  2008/05/12 15:14:46  yuuji
;; Fix behavior of mpg123-quit.
;;
;; Revision 1.50  2007/05/27 16:54:19  yuuji
;; mpg123-smart-dnd-setup
;;
;; Revision 1.49  2007/04/19 00:05:40  yuuji
;; New variable mpg123-mixer-type, mpg123-mixer-maxvol (by lenbok)
;;
;; Revision 1.48  2007/03/21 15:06:22  yuuji
;; New hooks 'mpg123-song-started-hook and 'mpg123-song-finished-hook
;; introduced. (by lenbok@gmail.com)
;;
;; Revision 1.47  2006/05/13 10:52:51  yuuji
;; Revise documentation.
;;
;; Revision 1.46  2006/05/13 10:49:45  yuuji
;; New variable mpg123-file-name-coding-system might help correct
;; handling of multi-byte file names.
;; Using ogginfo if available, to get ogg files information.
;; mpg123:match-string, mpg123:buffer-substring.
;;
;; Revision 1.45  2005/12/03 06:40:25  yuuji
;; Loop counter for current music introduced.
;; Thanks to Faraz Shahbazker.
;;
;; Revision 1.44  2004/09/17 14:03:13  yuuji
;; Fixed argument handling in mpg123-delete-file.
;;
;; Revision 1.43  2004/09/17 03:09:04  yuuji
;; * Support music longer than 99:59(not tested heavily).
;; * Add mpg123-active-p for external add-on's.
;; * Indicator and slider overlay can't be moved by insert-before-markers
;;   on XEmacs.  Fixed.
;;
;; Revision 1.42  2004/02/12 07:09:31  yuuji
;; mpg123-display-slider (key bound to ".") introduced.
;; mpg123 now tries to keep slider visible.
;; "I" toggles introduction-quiz mode.
;; mpg123-save-playlist ("S") now saves all marked positions.
;;
;; Revision 1.41  2004/02/10 15:56:22  yuuji
;; mpg123:draw-slider-help must always create mpg123*indicator-overlay.  Fixed.
;; mpg123:window-width must check window-width of mpg123*buffer.  Fixed.
;;
;; Revision 1.40  2003/07/26 04:23:27  yuuji
;; Do not alter mixer volume in mpg123:initialize	when
;; mpg123-startup-volume is nil. Suggested by imai_hiroshi_niboshi@yahoo.co.jp.
;;
;; Revision 1.39  2003/05/26 14:36:19  yuuji
;; Patch by moro@remus.dti.ne.jp;
;;  Fixed the failure of handling multi-byte chars in id3v1.1 support.
;;  Introduce mpg123-lazy-slider.
;;
;; Revision 1.38  2003/04/28 09:20:48  yuuji
;; mpg123-set-point-for-next-song-function, mpg123-format-name-function,
;; mpg123-now-playing, support id3v1.1 (thanks to Rene Kyllingstad)
;;
;; Revision 1.37  2003/03/31 02:31:33  yuuji
;; CR-LF(DOS) encoding playlist treated correctly. (by lenbok)
;;
;; Revision 1.36  2003/03/28 16:00:58  yuuji
;; For XEmacs: 'no-conversion changed to 'binary.
;; `g' mpg123-goto-current-line.
;; Japanese messages.
;;
;; Revision 1.35  2002/12/17 01:15:50  yuuji
;; mpg123:get-sound-type ignores case
;;
;; Revision 1.34  2002/10/20 13:31:41  yuuji
;; * (mpg123:playlist-p): Save and restore current buffer.
;; * (mpg123-quit): Don't switch to killed buffer.
;; * (mpg123:create-buffer): Set mpg123*initial-buffer.
;; * Don't set mpg123*initial-buffer while loading mpg123.el.
;;
;; (by Yoichi NAKAYAMA)
;;
;; Revision 1.33  2002/09/27 09:09:34  yuuji
;; Multibyte music tag displayed wrong, fixed.
;; Variable `mpg123-ogg123-id-coding-system controls coding system
;; for vorbiscomment.
;;
;; Revision 1.32  2002/09/25 05:50:21  yuuji
;; Fixed the bug when mpg123*use-face is nil.
;; Suggested by Yoichi NAKAYAMA <yoichi@eken.phys.nagoya-u.ac.jp>
;;
;; Revision 1.31  2002/09/21 13:10:16  yuuji
;; Fixed documentation for mpg123:ogg123-peek-tag.
;;
;; Revision 1.30  2002/09/21 12:56:27  yuuji
;; Picking ogg's comment starts from file offset 84.
;; Fixed by Akinori MUSHA <knu@iDaemons.org>.
;;
;; Revision 1.29  2002/09/18 17:22:05  yuuji
;; Peek ogg's comment directly.
;; Revise document.
;;
;; Revision 1.28  2002/09/14 12:59:08  yuuji
;; OggVorbis supported.
;;
;; Revision 1.27  2002/09/14 12:55:52  yuuji
;; B/b rewind to the previous music if it reaches at the beginning of the
;; music.
;;
;; Revision 1.26  2002/04/08 03:57:25  yuuji
;; IRIX 6.3 OK
;;
;; Revision 1.25  2002/02/15 04:37:32  yuuji
;; mpg123-refresh-tag, mpg123-id3-edit by N. SHIMIZU <CZA06074@nifty.com>
;;
;; Revision 1.24  2001/02/23 06:54:30  yuuji
;; Only `>' obeys repetition count.
;;
;; Revision 1.23  2001/02/23 05:39:26  yuuji
;; Delete-file key-binding is changed from `C-d' to `D' for trivial reason..
;;
;; Revision 1.22  2001/02/23 05:20:12  yuuji
;; `>' at the end of music list obeys the repetition counter.
;; Music list in a stack doesn't appear in the result of shuffle any more.
;; Shuffle preserves highlighted line any time.
;; Now nil for mpg123-preserve-playtime plays a music from the beginning.
;;
;; Revision 1.21  2001/02/21 03:41:10  yuuji
;; Support for OpenBSD is confirmed.
;;
;; Revision 1.20  2001/01/30 03:35:54  yuuji
;; (Win)convert music filename to dos file name for music over shared folder
;;
;; Revision 1.19  2001/01/19 04:41:37  yuuji
;; Fix the invalid 'cond form.
;;
;; Revision 1.18  2000/12/23 07:41:23  yuuji
;; Slider stays wrong position when music list added.  Fixed
;;
;; Revision 1.17  2000/12/08 00:54:09  yuuji
;; Variable mpg123-face-playing specifies the color of cursor for playing music.
;; Variable mpg123-face-slider specifies the color of slider of playing position.
;; Variable mpg123-need-slider specifies wheter the slider is needed or not.
;; Mouse-2 selects directly a music on the mouse pointer(in music list) or
;; playing position(in delimiter line).
;; RET(M-x mpg123-play) on the delimiter line move the playing position
;; according to the proportion of the window width from left side.
;;
;; Revision 1.16  2000/11/24 15:09:22  yuuji
;; Support emacs-21.0.9x (in mpg123:mp3-p)
;;
;; Revision 1.15  2000/10/20 14:43:06  yuuji
;; (if (featurep 'xemacs) (require 'overlay))
;;
;; Revision 1.14  2000/10/16 08:52:44  yuuji
;; 'mpg123*cur-face renamed to 'mpg123-cur-face (For XEmacs)
;;
;; Revision 1.13  2000/08/06 03:56:37  yuuji
;; Support volume setting on NetBSD(mixerctl)
;;
;; Revision 1.12  2000/08/06 02:27:58  yuuji
;; Set it default to use hilighting.
;;
;; Revision 1.11  2000/08/05 15:40:57  yuuji
;; Revise document.
;;
;; Revision 1.10  2000/08/05 15:37:50  yuuji
;; Handle mp3 files in multiple directories.
;; Playlist support.
;;
;; Revision 1.9  2000/06/25 14:38:17  yuuji
;; Fix for XEmacs+emu.el
;;
;; Revision 1.8  2000/02/09 04:15:31  yuuji
;; Fix for mule2 (mpg123:sound-p).
;;
;; Revision 1.7  1999/09/25 07:09:44  yuuji
;; mpg123-delete-file can delete music only from the list, not on the disk.
;; Shuffle after mpg123-delete-file now works correctly.
;;
;; Revision 1.6  1999/09/10 02:09:02  yuuji
;; mpg123-mp3-scan-bytes
;; defmacro changed to defsubst
;;
;; Revision 1.5  1999/07/24 03:58:52  yuuji
;; mule2でなるべく曲連係が途切れないように工夫(完璧ではない)。
;;
;; Revision 1.4  1999/07/05 09:00:19  yuuji
;; 日本語ファイル名対応(たぶん)
;; \C-d (mpg123-delete-file)
;;

;;; Code:

;; Linux users still using OSS rather than ALSA may wish to use
;; (setq mpg123-mixer-type 'aumix) before loading mpg123.el
(defvar mpg123-mixer-type
  (cond
   ((string-match "freebsd" (emacs-version))	'mixer)
   ((string-match "netbsd" (emacs-version))	'mixerctl)
   ((string-match "openbsd" (emacs-version))	'mixerctl) ;not yet tested
   ((string-match "linux" (emacs-version))	'alsa)
   ((string-match "irix" (emacs-version))	'apanel)
   ((string-match "nt[45]\\|windows9" (emacs-version)) 'mixer.exe)
   ((string-match "solaris" (emacs-version))	'audioctl)))

(defvar mpg123-mpg123-command "mpg123"
  "*Command name of mpg123 player. Need 0.59q or later.
mpg123のコマンド名。0.59qが必要。")
(defvar mpg123-mpg123-command-args nil
  ;;'("--8bit -m")	;<- example
  "*Arguments to give to mpg123")
(defvar mpg123-ogg123-command "ogg123"
  "*Command name of ogg123 player.
ogg123のコマンド名")
(defvar mpg123-ogg123-command-args nil
  ;;'("-d oss")	;<- example
  "*Arguments to give to ogg123")
(defvar mpg123-ogg123-id-coding-system
  (and (fboundp 'coding-system-p)
       (cond ((and (coding-system-p 'utf-8)
		   (or (featurep 'un-define) (string< "21.3" emacs-version)))
	      'utf-8)
	     ((coding-system-p '*junet*) '*junet*)
	     ((coding-system-p 'junet) 'junet)
	     (t nil)))
  "Preferred coding system of vorbisogg comment tag.
If running Emacs knows utf-8, use it.  If any, we use iso-20220jp instead.")

(defvar mpg123-file-name-coding-system
  (cond
   ((coding-system-p 'euc-jp) 'euc-jp)
   ((coding-system-p '*euc-japan*) '*euc-japan*))
  "*Default file name coding system for encoding music files.")

(defvar mpg123-mixer-command
  (cdr (assq mpg123-mixer-type
	     '((mixer . "mixer") (mixerctl . "mixerctl")
	       (aumix . "aumix") (apanel . "apanel -nodisplay")
	       (audioctl . "audioctl") (nt . "mixer.exe")
               (alsa . "amixer"))))
  "*Command name for mixer setting utility
mixer調節用コマンド")
(defvar mpg123-default-dir "~/mp3")
(defvar mpg123-mixer-mixerctl-target "outputs.master"
  "*Target name of mixerctl for setting master volume.
This value might varies among various hardware.  Set proper value
on every host.
mixerctlでボリューム設定する場合の値. 各NetBSDホストごとに設定すべき。")
(defvar mpg123-mixer-setvol-target-list
  (cdr (assq mpg123-mixer-type
	     (list '(mixer . ("vol" "pcm"))
		   (cons 'mixerctl (list mpg123-mixer-mixerctl-target))
		   '(aumix . ("-w"))
		   '(apanel . ("-outlevels"))
		   '(audioctl . ("-v"))
		   '(nt . ("-v"))
		   '(alsa . ("PCM")))))
  "*Option list for volume setting utility.
mixer調節コマンドの音量調節オプションのリスト")
(defvar mpg123-mixer-maxvol
  (if (eq mpg123-mixer-type 'mixerctl) 255 
    (if (eq mpg123-mixer-type 'alsa) 31 
      100))
  "*Maximum volume permitted by the mixer utility.
mixer調節コマンドで設定できる音量最大値")
(defvar mpg123-preserve-playtime t
  "When shift to other music, leave playing time of current music, or not")
(defvar mpg123-id3-tag-function 'mpg123:peek-tag
  "*Emacs-Lisp function for extracting ID3 tag.
MP3からID3を取得するための関数")
(defvar mpg123-set-point-for-next-song-function nil
  "*Return non-nil if it has set the point in the playlist for the next song to be played.
Called when changing songs to allow playing in non-playlist order.
1曲終わったときに次の曲の位置にポイントを動かす関数.
プレイリストの並びとは違う再生順序にしたいときに、
ポイントを移動する関数を定義してその関数名をこの変数に入れよう。")
(defvar mpg123-startup-volume 30
  "*Default sound volume at startup of this program.
mpg123.el初回起動時の音量のデフォルト値.")
(defvar mpg123-default-repeat 0
  "*Default number of repetition of through playing.
再生のデフォルトのリピート回数")
(defvar mpg123-process-coding-system
  (cond ((and (fboundp 'modify-coding-system-alist)
	      (intern-soft "euc-jp")
	      (featurep 'mule))
	 'euc-jp)
	((boundp '*euc-japan*) *euc-japan*)
	(t nil))
  "*Default process coding system for mpg123.
mpg123コマンド用の漢字コード。漢字ファイル名があるときは必須")
(defvar mpg123-format-name-function nil
  "*Function to override the format of the name in the playlist.
It is called with the arguments ARTIST, ALBUM, TITLE, TRACKNUM, FILENAME
and shoud return the string to be displayed.
曲リスト行の書式を決める関数。
アーチスト、アルバム、タイトル、トラック、ファイル名
の5つの引数を受け取り、それらを加工して1行の文字列を返す関数を定義する。")
(defvar mpg123-lazy-slider nil
  "*Non-nil for updating slider only once in a second.
non-nilのときはスライダーを一秒に一回だけ更新する。")
(defvar mpg123-omit-id3-artist nil
  "*Non-nil for omitting artist name display of ID3 tag.
non-nilのときID3タグからのアーチスト名表示を省略する。")
(defvar mpg123-mp3-scan-bytes 4
  "*Default number of bytes of header to examine the file is mp3/Ogg or not.
MP3/Oggファイルかどうかを調べるために読み込むファイルの先頭のバイト数")

(defvar mpg123-lazy-check nil  ;;"\\(\\.ogg$\\|\\.mp3\\)"
  "*Check sound file or not by filename.
If want to check by filename, set this variable to filename regexp.
MP3ファイルかどうか調べるためにファイル名だけで済ます場合は
正規表現を指定する.")

(defvar mpg123-show-help t
  "*Print help summary in mpg123 buffer")

(defvar mpg123-auto-redraw nil
  "*Redraw automatically when changed window size")

(defvar mpg123-mode-map nil)
(setq mpg123-mode-map (make-keymap))
(define-key mpg123-mode-map "p" 'mpg123-prev-line)
(define-key mpg123-mode-map "n" 'mpg123-next-line)
(define-key mpg123-mode-map " " 'mpg123-play-stop)
(define-key mpg123-mode-map "\C-m" 'mpg123-play)
(define-key mpg123-mode-map "<" 'mpg123-<)
(define-key mpg123-mode-map ">" 'mpg123->)
(define-key mpg123-mode-map "m" 'mpg123-mark-position)
(define-key mpg123-mode-map "r" 'mpg123-refrain)
(define-key mpg123-mode-map "w" 'mpg123-where-is-mark)
(define-key mpg123-mode-map "-" 'mpg123-volume-decrease)
(define-key mpg123-mode-map "+" 'mpg123-volume-increase)
(define-key mpg123-mode-map "=" 'mpg123-volume-increase)
(define-key mpg123-mode-map "v" 'mpg123-volume-decrease)
(define-key mpg123-mode-map "V" 'mpg123-volume-increase)
(define-key mpg123-mode-map "f" 'mpg123-forward)
(define-key mpg123-mode-map "b" 'mpg123-backward)
(define-key mpg123-mode-map "F" 'mpg123-forward-10)
(define-key mpg123-mode-map "B" 'mpg123-backward-10)
(define-key mpg123-mode-map "o" 'mpg123-open-new)
(define-key mpg123-mode-map "a" 'mpg123-add-new)
(define-key mpg123-mode-map "i" 'mpg123-increase-repeat-count)
(define-key mpg123-mode-map "d" 'mpg123-decrease-repeat-count)
(define-key mpg123-mode-map "L" 'mpg123-increase-loop-count)
(define-key mpg123-mode-map "l" 'mpg123-decrease-loop-count)
(define-key mpg123-mode-map "k" 'mpg123-kill-line)
(define-key mpg123-mode-map "K" 'mpg123-kill-stack)
(define-key mpg123-mode-map "y" 'mpg123-yank-line)
(define-key mpg123-mode-map "s" 'mpg123-shuffle)
(define-key mpg123-mode-map "S" 'mpg123-save-playlist)
(define-key mpg123-mode-map "D" 'mpg123-delete-file)
(define-key mpg123-mode-map "E" 'mpg123-id3-edit)
(define-key mpg123-mode-map "W" 'mpg123-what-file)
(define-key mpg123-mode-map "g" 'mpg123-goto-current-line)
(define-key mpg123-mode-map "." 'mpg123-display-slider)
(define-key mpg123-mode-map "I" 'mpg123-introduction-quiz-mode)
(define-key mpg123-mode-map "q" 'mpg123-quit)
(define-key mpg123-mode-map "Q" 'mpg123-quit-yes)
(if (and window-system)
    (cond
     ((featurep 'xemacs)
      (define-key mpg123-mode-map '(button1) 'mpg123-mouse-play-stop)
      (define-key mpg123-mode-map '(control button4) 'mpg123-volume-increase)
      (define-key mpg123-mode-map '(control button5) 'mpg123-volume-decrease)
      (define-key mpg123-mode-map 'button3 'mpg123-mouse-force-play)
      )
     (t
      (define-key mpg123-mode-map [down-mouse-1] 'mpg123-mouse-play-stop)
      (define-key mpg123-mode-map [down-mouse-2] 'mpg123-mouse-force-play)
      (define-key mpg123-mode-map [C-mouse-4] 'mpg123-volume-increase)
      (define-key mpg123-mode-map [C-mouse-5] 'mpg123-volume-decrease)
      )))
(let ((ch ?0))
  (while (<= ch ?9)
    (define-key mpg123-mode-map (char-to-string ch) 'digit-argument)
    (setq ch (1+ ch))))

;;;
;; Internal Work
;;;
(defvar mpg123*process nil)
(defvar mpg123*buffer "*mpg123*")
(defvar mpg123*info-buffer " *mpg123 info* ")
(defvar mpg123*cur-music-number nil)
(defvar mpg123*cur-music-file nil)
(defvar mpg123*cur-playtime nil)
(defvar mpg123*cur-playframe nil)
(defvar mpg123*cur-playframe nil)
(defvar mpg123*cur-start-frame "0")
(defvar mpg123*cur-play-marker nil)
(defvar mpg123*cur-edit-marker nil)
(defvar mpg123*cur-repeat-count nil)
(defvar mpg123*cur-loop-count 0)
(defvar mpg123*music-alist nil)
(defvar mpg123*default-time-string "--:--/--:--\t")
(defvar mpg123*interrupt-p nil)
(defvar mpg123*end-of-list-marker nil "The end mark of music list")
(defvar mpg123*cur-volume nil)
(defvar mpg123*volume-marker nil)
(defvar mpg123*time-setting-mode nil)
(defvar mpg123*repeat-count-marker nil)
(defvar mpg123*loop-count-marker nil)
;(defvar mpg123-playlist-regexp ".*\\.m3u"
;  "*Regular expression to match to playlist files")
(defvar mpg123-url-regexp "http://.*"
  "*Regular expression to match to URL requests")

(defvar mpg123-mpg123-time-regexp  "Time: \\(..:..\\)... +\\[\\(..:..\\)")
(defvar mpg123-mpg123-init-frame-regexp "Frame# *[0-9]+ *\\[\\([ 0-9]+\\]\\)")
(defvar mpg123-mpg123-frame-regexp "Frame# *\\([0-9]+\\) *")

(defvar mpg123-ogg123-time-regexp "Time: \\(..:..\\)... +\\[..:.....\\] of \\([0-9][0-9]:[0-9][0-9]\\)")
(defvar mpg123-ogg123-frame-regexp "Time: \\(..:..\\)... +\\[..:.....\\]")
(defvar mpg123-ogg123-init-frame-regexp " of \\(..:..\\)...")
(defvar mpg123:time-regexp nil)
(defvar mpg123:frame-regexp nil)
(defvar mpg123:init-frame-regexp nil)
(defvar mpg123*convert-frame-function nil)
(defvar mpg123*cur-total-frame nil)
(defvar mpg123*indicator-overlay nil)
(defvar mpg123*cur-overlay nil "Overlay to cursor of playing position")
(defvar mpg123*slider-overlay nil "Overlay of playing position slider")


(defvar mpg123*window-width nil)
(defvar mpg123*cur-total-frame nil)
(defvar mpg123*cur-slider-column nil)
(defvar mpg123*initial-buffer nil)
(defvar mpg123-introduction-quiz-mode nil
  "Music introduction quiz mode")

(defvar mpg123-type-alist
  '(("mp3" . "mpg123") ("ogg" . "ogg123")))

(fset 'mpg123:buffer-substring
      (if (fboundp 'buffer-substring-no-properties)
	  'buffer-substring-no-properties
	'buffer-substring))

(fset 'mpg123:str2int
      (if (fboundp 'string-to-number)
	  (function(lambda (string &optional base)
		     (ceiling (string-to-number string base))))
	'string-to-int))

(defun mpg123:match-string (n &optional m)
  "Return (buffer-substring (match-beginning N) (match-beginning (or M N)))."
  (if (match-beginning n)
      (mpg123:buffer-substring (match-beginning n)
			(match-end (or m n)))))

(defun mpg123-now-playing ()
  "Return name of song currently playing in mpg123, or nil"
  (let* ((buf (and (boundp 'mpg123*buffer) (get-buffer mpg123*buffer)))
	 (p (and buf (get-buffer-process buf)))
	 (playingp (and p (eq (process-status p) 'run))))
    (if playingp
	(mpg123:get-music-info mpg123*cur-music-number 'name))))

(defun mpg123-active-p ()		;from Len Trigg
  "Returns the mpg123 buffer if mpg123 is active, otherwise nil.
Songs need not be actually playing.  This may be used as an indicator as to
whether mpg123 functions can be called."
  (and (boundp 'mpg123*buffer) 
       (get-buffer mpg123*buffer)))

(defun mpg123:file-name-extension (name)
  (let ((md (match-data)))
    (prog1
	(if (string-match "\\.\\([^.]+\\)$" name)
	    (substring name (match-beginning 1) (match-end 1))
	  "mp3")		;; defaults to "mp3"
      (store-match-data md))))

(defun mpg123:get-sound-type (name)
  (cdr (assoc (downcase (mpg123:file-name-extension name)) mpg123-type-alist)))

(defun mpg123:get-from-file-about (file what)
  (let* ((case-fold-search t)
	 (type (mpg123:get-sound-type file))
	 (var (format "mpg123-%s-%s" type what)))
    (and type
	 (intern-soft var)
	 (symbol-value (intern var)))))

(defun mpg123:get-command-name (file)
  (mpg123:get-from-file-about file "command"))

(defun mpg123:get-command-args (file)
  (mpg123:get-from-file-about file "command-args"))

(defun mpg123:time-regexp (file)
  "Return time-regexp corresponding to FILE"
  (or (mpg123:get-from-file-about file "time-regexp")
      "CANTBEHERE"))

(defun mpg123:init-frame-regexp (file)
  (or (mpg123:get-from-file-about file "init-frame-regexp")
      "CANTBEHERE"))
(defun mpg123:frame-regexp (file)
  (or (mpg123:get-from-file-about file "frame-regexp")
      "CANTBEHERE"))

(defun mpg123:convert-frame-function (filename)
  (let*((type (mpg123:get-sound-type filename))
	(fname (format "mpg123-%s-convert-frame" type)))
    (if (and (intern-soft fname)
	     (fboundp (intern fname)))
	(intern fname)
      'concat)))

(defun mpg123-mpg123-convert-frame (time filename)
  "Return frame itself"
  (mpg123:str2int time))
(defun mpg123-ogg123-convert-frame (time filename)
  "Return (mpg123:time2frame TIME FILENAME)"
  (mpg123:str2int (mpg123:time2frame time filename)))

;;;
;; Functions related to sound file format inspection
;;;
(defun mpg123:hex-value (point length &optional little-endian)
  "Return the hex value the POINT positions LENGTH byte stream represents.
Optional third argument LITTLE-ENDIAN is self extplanatory."
  (setq point (1+ point)) ;translate file offset to Emacs's point value
  (let ((mlt 1)
	(pos (if little-endian point (+ point length -1)))
	(direc (if little-endian 1 -1))
	(value 0))
    (while (> length 0)
      (setq value (+ value (* mlt (char-after pos)))
	    pos (+ pos direc)
	    mlt (* mlt 256)
	    length (1- length)))
    value))

(defun mpg123:peek-tag (file)
  "Should be REWRITEN!!!!"
  (let ((type (mpg123:get-sound-type file)))
    (cond
     ((string-equal type "mpg123") (mpg123:peek-id3-tag file))
     ((string-equal type "ogg123") (mpg123:ogg123-get-tag file))
     ((mpg123:mp3-p file) (mpg123:peek-id3-tag file))
     ((mpg123:ogg-p file) (mpg123:ogg123-get-tag file))
     (t "Unknown File"))))

(defun mpg123:mp3-skip-id3v2 ()
  "Skip ID3v2 tag in current buffer."
  (let ((case-fold-search nil) (p (point)))
    (while (looking-at "ID3")
      (goto-char (setq p (+ p 6)))	;header size location
      (goto-char
       (+ p
	  (* 128 128 128 (char-after p))
	  (* 128 128 (char-after (1+ p)))
	  (* 128 (char-after (+ p 2)))
	  (char-after (+ p 3))
	  4)))
    (goto-char (1- (point)))))

(defun mpg123:insert-raw-file-contents (&rest args)
  (let ((file-coding-system-alist (list (cons "." 'binary)))
	(file-coding-system
	 (if (and (fboundp 'coding-system-p)
		  (boundp '*noconv*) (coding-system-p '*noconv*))
	     '*noconv*			;mule19
	   'binary))			;XEmacs(maybe)
	(file-coding-system-for-read '*noconv*)) ;19
    (apply 'insert-file-contents args)))
  
(defun mpg123:mp3-p (f &optional pattern)
  "Check the file F is MPEG 1 Audio file or not.
If optional argument PATTERN given, search it(tentative)."
  (save-excursion
    ;;check file size > 128
    (and (> (nth 7 (file-attributes (file-truename f))) 128)
	 (let ((b (set-buffer (get-buffer-create " *mpg123tmp*"))) e0 b0
	       (skipchars (if (and (not (featurep 'xemacs))
				   (string< "21" emacs-version))
			      "\000-\177"
			    "^\xff")))
	   (set-buffer b)
	   (if (fboundp 'set-buffer-multibyte) (set-buffer-multibyte nil))
	   (erase-buffer)
	   (insert "..")		;dummy dot to simplify the while loop
	   (mpg123:insert-raw-file-contents f nil 0 mpg123-mp3-scan-bytes)
	   (goto-char (point-min))
	   (prog1	; if short & 0xfff0 = 0xfff0, it is MPEG audio
	       (or
		(looking-at "\\.\\.ID3")
		(catch 'found
		  (if (stringp pattern)
		      (throw 'found
			(re-search-forward pattern nil t)))
		  (if (string-match "^19\\." emacs-version)
		      (while (search-forward "\xff" nil t) ;Scan `0xff'
			(setq e0 (match-end 0) b0 (match-beginning 0))
			(if (and (char-after b0)
				 (= (logand
				     (char-after (+ 1 b0)) ?\xF0)
				    ?\xF0))
			    (throw 'found t))
			(goto-char e0))
		    (while (> (skip-chars-forward skipchars) 0) ;Scan `0xff'
		      (if (and (equal (char-after (point)) ?\xFF)
			       (char-after (1+ (point)))
			       (= (logand (char-after (1+ (point)))
					  ?\xF0) ?\xF0))
			  (throw 'found t))))))
	     (kill-buffer b))))))

(defun mpg123:ogg-p (f)
  (let ((case-fold-search nil))
    (mpg123:mp3-p f "OggS")))

(defun mpg123:sound-p (f)
  (or (and mpg123-lazy-check (stringp mpg123-lazy-check)
	   (string-match mpg123-lazy-check (file-name-nondirectory f)))
      (mpg123:mp3-p f)
      (mpg123:ogg-p f)))

(defun mpg123-next-line (arg)
  "Down line"
  (interactive "p")
  (forward-line arg)
  (if (mpg123:in-music-list-p)
      (skip-chars-forward "^:")))

(defun mpg123-prev-line (arg)
  "Up line"
  (interactive "p")
  (mpg123-next-line (- arg)))

(defsubst mpg123:goto-playtime-position ()
  ;;For speed, we do not set-buffer to where mpg123*cur-play inhabits,
  ;;Confirm yourself to set the current buffer to mpg123*cur-play buffer
  ;;if you call this from the function that can be called when the
  ;;currently being played music is in stac-buffer.
  (goto-char mpg123*cur-play-marker)
  (skip-chars-forward "^:")
  (skip-chars-backward "^ "))
; (defmacro mpg123:goto-playtime-position ()
;   (list 'progn
; 	(list 'goto-char 'mpg123*cur-play-marker)
; 	;(list 'move-to-column 3)
; 	(list 'skip-chars-forward "^:")
; 	(list 'forward-char -2)
; 	))

(defsubst mpg123:update-playtime (timestr &optional here)
  "Update playing time string"
  (save-excursion
    (set-buffer (marker-buffer mpg123*cur-play-marker))
    (let (buffer-read-only)
      (or here (mpg123:goto-playtime-position))
      (delete-char 5)
      (while (/= (char-after (point)) ?/) (delete-char 1))
      (insert timestr))))

; (defmacro mpg123:update-playtime (timestr)
;   (list 'save-excursion
; 	(list 'set-buffer (list 'marker-buffer 'mpg123*cur-play-marker))
; 	;(list 'set-buffer 'mpg123*buffer)
; 	(list 'let (list 'buffer-read-only)
; 	      (list 'mpg123:goto-playtime-position)
; 	      (list 'delete-char 5)
; 	      (list 'insert timestr)
; 	      ;(list 'set-buffer-modified-p nil) ;is not essential
; 	      )))

(defun mpg123:update-length (timestr)
  "Update music length time string"
  (set-buffer mpg123*buffer)
  (let (buffer-read-only)
    (mpg123:goto-playtime-position)
    (skip-chars-forward "^/")
    (forward-char 1)
    (delete-char 5);this deletion 5 chars may correct. because "--:--"
    (insert timestr)))

(defun mpg123:update-volume (vollist)
  "Update volume display"
  (set-buffer mpg123*buffer)
  (let (buffer-read-only)
    (save-excursion
      (goto-char mpg123*volume-marker)
      (delete-region (point) (progn (skip-chars-forward "^\\]") (point)))
      (if (and (listp vollist) (integerp (car vollist)))
	  (let ((vstr (format "%03d:%03d" (car vollist) (cdr vollist))))
	    (insert vstr) (message "Volume: %s" vstr))
	(insert "N/A")))))

(defun mpg123:update-repeat-count ()
  "Update repetition meter"
  (set-buffer mpg123*buffer)
  (let (buffer-read-only)
    (save-excursion
      (goto-char mpg123*repeat-count-marker)
      (delete-region (point) (progn (skip-chars-forward "^\\]") (point)))
      (insert
       (cond
	((= mpg123*cur-repeat-count 0) "--")
	((= mpg123*cur-repeat-count -1) "oo")
	(t (format "%02d" mpg123*cur-repeat-count)))))))


(defun mpg123:update-loop-count ()
  "Update repetition meter"
  (set-buffer mpg123*buffer)
  (let (buffer-read-only)
    (save-excursion
      (goto-char mpg123*loop-count-marker)
      (delete-region (point) (progn (skip-chars-forward "^\\]") (point)))
      (insert
       (cond
	((= mpg123*cur-loop-count 0) "--")
	((= mpg123*cur-loop-count -1) "oo")
	(t (format "%02d" mpg123*cur-loop-count)))))))



(defun mpg123:repeat-check ()
  (cond
   ((= mpg123*cur-repeat-count -1) t)
   ((= mpg123*cur-repeat-count 0) nil)
   (t (setq mpg123*cur-repeat-count (1- mpg123*cur-repeat-count))
      (mpg123:update-repeat-count)
      mpg123*cur-repeat-count)))

(defun mpg123:max-music-number ()
  "Returns the maximal music number from mpg123*music-alist, or 0 if empty."
  (if mpg123*music-alist
      (car (sort (mapcar (function (lambda (s) (car s)))
                         mpg123*music-alist)
                 '>))
    0))

(defun mpg123:set-music-info (n attr value)
  (let ((cur (cdr (assoc n mpg123*music-alist))))
    (setq cur (cons (cons attr value)
		    (delq (assq attr cur) cur)))
    (setq mpg123*music-alist
	  (cons (cons n cur)
		(delq (assoc n mpg123*music-alist) mpg123*music-alist)))))

(defun mpg123:get-music-info (n attr)
  (cdr (assq attr (assoc n mpg123*music-alist))))
; (defmacro mpg123:get-music-info (n attr)
;   (list 'cdr (list 'assq attr (list 'assoc n 'mpg123*music-alist))))

(defun mpg123:delete-music-from-list (n)
  "Delete music number N from mpg123*music-alist."
  (setq mpg123*music-alist
	(delq (assq n mpg123*music-alist) mpg123*music-alist)))

(defun mpg123:open-error ()
  (momentary-string-display "
***********************************************************
Something is wrong with sound device.
It seemes that you don't have set up sound device on
this machine, or you already have running some application
which locks sound device in other session on this host.
Anyway, you have to make sure that mpg123 program plays
mp3 files on your pseudo terminal(xterm, rxvt, etc).
-- Type SPC to exit ---

サウンドデバイスが開けん素。
このマシンのオーディオデバイスはちゃんと設定したけ?
あと、ほかにサウンドデバイスを使うアプリケーションを起動して
いるんちゃう?
まず、ktermなどで mpg123 コマンド単独で音楽再生できるかどうか
確認してみれ。
(スペースキーでオサラバ)
***********************************************************" (point)))
  
(defun mpg123:initial-filter (proc mess)
  "mpg123 process filter No.1, called at startup of mpg123."
  (let ((targetfile mpg123*cur-music-file))
    (if (string-match "Can't open /dev" mess)
	(progn
	  (set-process-filter proc nil)
	  (mpg123:open-error)
	  (error "bye")))
    (if (string-match mpg123:init-frame-regexp mess)
	(let ((f (substring mess (match-beginning 1) (match-end 1))))
	  (mpg123:set-music-info
	   mpg123*cur-music-number 'frames
	   (setq mpg123*cur-total-frame
		 (funcall mpg123*convert-frame-function f targetfile)))))
    (if (string-match mpg123:time-regexp mess)
	(let ((cur (substring mess (match-beginning 1) (match-end 1)))
	      (max (substring mess (match-beginning 2) (match-end 2))))
	  (mpg123:update-playtime cur)
	  (mpg123:set-music-info
	   mpg123*cur-music-number 'length max)
	  (if (not (string-equal "00:00" max))
	      (mpg123:update-length max))
	  ))
					;  (save-excursion
				    ;    (set-buffer mpg123*info-buffer)
					;    (insert mess))
    (and (mpg123:get-music-info mpg123*cur-music-number 'length)
	 (mpg123:get-music-info mpg123*cur-music-number 'frames)
	 (set-process-filter proc 'mpg123:filter))))

(defsubst mpg123:window-width ()
  (if (and mpg123-auto-redraw
	   (get-buffer-window mpg123*buffer t))
      (window-width (get-buffer-window mpg123*buffer t))
    mpg123*window-width))

;; (defun mpg123:move-slider (column)
;;   "Move slider to COLUMN"
;;   (let ((left (overlay-start mpg123*indicator-overlay)))
;;     (move-overlay mpg123*slider-overlay
;; 		  (+ left column)
;; 		  (+ left column 1)
;; 		  (overlay-buffer mpg123*slider-overlay))))

(defun mpg123:filter (proc mess)
  (if (stringp mess)
      (save-excursion
	;;(set-buffer mpg123*info-buffer)  ;heavy
	;;(insert mess)                    ;jobs
	(let ((update-slider (not mpg123-lazy-slider)))
	  (if (string-match mpg123:time-regexp mess)
	      (let ((s (substring mess (match-beginning 1) (match-end 1))) mp)
		(and (not (string= s mpg123*cur-playtime))
		     (not mpg123*time-setting-mode)
		     (setq update-slider t) ;beware! always returns t
		     (progn		;care intro-quiz mode
		       (mpg123:update-playtime (setq mpg123*cur-playtime s))
		       (and (eq mpg123-introduction-quiz-mode t)
			    (setq mp (mpg123:get-music-info
				      mpg123*cur-music-number 'marktime))
			    (string= s (mpg123:add-time-string mp -1))
			    (progn
			      (set-buffer mpg123*buffer)
			      (mpg123:kill-current-music)))))))
	  (if (string-match mpg123:frame-regexp mess)
	      (let (c)
		(setq mpg123*cur-playframe
		      (funcall mpg123*convert-frame-function
			       (substring mess
					  (match-beginning 1)
					  (match-end 1))
			       mpg123*cur-music-file))
		(if (or update-slider
			(= mpg123*cur-playframe mpg123*cur-total-frame))
		    (progn	    ;redraw & slider only if it's needed
		      (and mpg123-auto-redraw
			   (/= (mpg123:window-width) mpg123*window-width)
			   (mpg123:draw-slider-help nil))
		      (mpg123:slider-check)))))))))

(defsubst mpg123:slider-check-1 ()
  (let ((c (/ (* (mpg123:window-width)
		 mpg123*cur-playframe)
	      mpg123*cur-total-frame))
	left)
    (or (eq c mpg123*cur-slider-column)
	(progn
	  (setq left (overlay-start mpg123*indicator-overlay))
	  (move-overlay mpg123*slider-overlay
			(+ left c)
			(+ left c 1)
			(overlay-buffer mpg123*slider-overlay))))))

(defsubst mpg123:null ())

(defun mpg123:get-largest-buffer-window (buffer min frame)
  "Needed for Emacs-23 or later"
  (if (fboundp 'get-buffer-window-list)
      (let ((wl (get-buffer-window-list buffer min frame))
	    (maxh 0) (mw nil))
	(while wl
	  (if (> (window-height (car wl)) maxh)
	      (setq mw (car wl)
		    maxh (window-height mw)))
	  (setq wl (cdr wl)))
	(or mw (get-buffer-window buffer t)))
    (get-buffer-window buffer t)))

(defun mpg123:sentinel (proc state)
  (cond
   ((string-match "^finished" state)
    (if mpg123*interrupt-p
	(progn
	  (if (eq mpg123*interrupt-p 'quit)
	      (kill-buffer mpg123*buffer))
	  (setq mpg123*interrupt-p nil))
      (setq mpg123*time-setting-mode nil)
      (mpg123:update-playtime "--:--")
      (if (eq (get-buffer mpg123*buffer)
              (marker-buffer mpg123*cur-play-marker))
          (let ((cb (current-buffer)) (sw (selected-window))
		(viswin (mpg123:get-largest-buffer-window mpg123*buffer nil t))
		(sf (selected-frame)) mp3w p)
	    (if viswin
		(select-window viswin)
	      (set-buffer mpg123*buffer))
            (goto-char mpg123*cur-play-marker)
            (run-hooks 'mpg123-song-finished-hook)
	    (or (and (fboundp mpg123-set-point-for-next-song-function)
		     (funcall mpg123-set-point-for-next-song-function))
		(mpg123-next-line 1))
	    (sit-for 0)
            (if (and (not (mpg123:in-music-list-p))
                     (mpg123:repeat-check)) ;decrement counter and check
                (goto-char (point-min)))
	    (setq p (point))
	    (put 'mpg123:sentinel 'current-buffer cb)
	    (unwind-protect
		(if (and (string-match "^19\\." emacs-version)
			 (setq mp3w (get-buffer-window mpg123*buffer t)))
		    ;; For the sake of Emacs 19, we have to switch to
		    ;; mpg123 buffer explicitly.
		    (progn
		      (select-frame (window-frame mp3w))
		      (save-window-excursion
			(select-window mp3w)
			(switch-to-buffer mpg123*buffer)
			(goto-char p)
			(message "Next music")
			(sit-for (string-to-number "0.1"))
			(mpg123:play)))
		  ;; Emacs20 or later, simply play it.
		  (mpg123:play))
	      (put 'mpg123:sentinel 'current-buffer nil)
	      (select-frame sf)
	      (select-window sw)
	      (switch-to-buffer cb))))))
   ((string-match "^hangup" state)
    (setq mpg123*interrupt-p nil))))

(defvar mpg123*time2frame-ratio-alist
  '(("mpg123" 1000 . 26) ("ogg123" 1 . 1)))

(defun mpg123:time2frame (timestr &optional filename)
  "Convert time string (mm:ss) to frame number.(0.026s/f)"
  (setq filename (or filename mpg123*cur-music-file))
  (string-match "\\([-0-9]+\\):\\(..\\)" timestr)
  (let*((m (string-to-number
	    (substring timestr (match-beginning 1) (match-end 1))))
	(s (string-to-number
	    (substring timestr (match-beginning 2) (match-end 2))))
	(total (+ (* m 60) s))
	(frames (mpg123:get-music-info mpg123*cur-music-number 'frames))
	(length (mpg123:get-music-info mpg123*cur-music-number 'length))
	(type (mpg123:get-sound-type filename))
	(alist mpg123*time2frame-ratio-alist)
	(ratio-f (car (cdr (assoc type alist))))
	(ratio-s (cdr (cdr (assoc type alist)))))
    (if (and frames length)
	;;if total frames and length(in time) is available,
	;;use them to calculate target frame number
	(let*((colon (string-match ":" length))
	      (lmin (substring length 0 colon))
	      (lsec (substring length (1+ colon))))
	  (setq ratio-f frames
		ratio-s (+ (* 60 (string-to-number lmin))
			   (string-to-number lsec)))))
    (format "%d" (/ (* 1.0 total ratio-f) ratio-s))))

(defun mpg123:in-music-list-p ()
  (and (equal mpg123*buffer (buffer-name))
       (< (point) mpg123*end-of-list-marker)))

(defun mpg123:in-indicator-p (&optional pos)
  "Return whether the point (or POS) is in playing position indicator or not."
  (memq mpg123*indicator-overlay (overlays-at (or pos (point)))))

;;2000/5/19
(defvar mpg123*use-face t)
(defvar mpg123-face-playing '("yellow" . "#004080")
  "*Set of default playing cursor's foreground/background color.
Set this as '(FGCOLOR . BGCOLOR)
演奏中の曲名を示すカーソルの色。'(前景色 . 背景色) という
コンスセルで指定する。XEmacsでは -nw のときに表現できない色だと
デフォルト色になってしまうので、せめて前景色は yellow のような
基本8色の一つにしておくと良い。Emacs-21 は -nw で表現できない色は
基本8色のうち近い色にmappingしてくれるがどぎつい色になりやすいので
ほどほどに。")
(defvar mpg123-face-slider '("black" . "yellow")
  "*Set of default fgcolor/bgcolor of slider in indicator.
演奏中の曲の相対位置を示す曲リスト下部のインジケータ内スライダーの色
mpg123-face-playing のDOC-STRINGも参照せよ")
(defvar mpg123-face-indicator '("yellow" . "#7f3000")
  "*Set of default fgcolor/bgcolor of indicator bar.
曲リスト下部のインジケータの (前景色 . 背景色)")

(if (featurep 'xemacs) (require 'overlay))
(if (and (fboundp 'make-face) mpg123*use-face)
    (progn
      (make-face 'mpg123-face-cur)
      (make-face 'mpg123-face-slider)
      (make-face 'mpg123-face-indicator)
      (if (or (and (fboundp 'display-color-p)	;Emacs-21
		   (display-color-p))
	      (and (fboundp 'device-class) 	;XEmacs-21
		   (eq 'color (device-class (selected-device))))
	      (and window-system (x-display-color-p)))
	  (progn
	    (set-face-foreground 'mpg123-face-cur (car mpg123-face-playing))
	    (set-face-background 'mpg123-face-cur (cdr mpg123-face-playing))
	    (set-face-foreground 'mpg123-face-slider (car mpg123-face-slider))
	    (set-face-background 'mpg123-face-slider (cdr mpg123-face-slider))
	    (set-face-foreground 'mpg123-face-indicator (car mpg123-face-indicator))
	    (set-face-background 'mpg123-face-indicator (cdr mpg123-face-indicator)))
	(set-face-underline-p 'mpg123-face-cur t)
	(set-face-underline-p 'mpg123-face-slider t)
	(set-face-background 'mpg123-face-slider "white")))
  (setq mpg123*use-face nil))

(defvar mpg123-need-slider mpg123*use-face
  "*Need slider in the delimiter line which indicates playing position.
一覧の下の境界線領域に、現在曲の再生位置を示すスライダーが要るかどうか。")
(defvar mpg123-display-slider mpg123-need-slider
  "*Display the slider in visible position or not.
'always displays slider line in the next small window.
Other non-nil value check the current visibility of slider line and
split window to display slider if not visible at the time.  Do nothing
if slider is already visible.
現在曲再生位置表示スライダーを見えるようにするかを決定する。
この値が 'always なら、必ずスライダー専用の小さいウィンドウを作って
そこに表示。
'always 以外の Non-nil の値なら一曲の演奏開始時点でスライダーが見えていなければ
ウィンドウ分割し、見えていれば何もしない。")

(defun mpg123:play (&optional startframe)
  "Play mp3 on current line."
  (save-excursion
    (set-buffer (get-buffer-create mpg123*info-buffer))
    (buffer-disable-undo)
    (erase-buffer))
  (beginning-of-line)
  (if (and mpg123-introduction-quiz-mode
	   (or (null startframe) (= (mpg123:str2int startframe) 0)))
      (setq mpg123-introduction-quiz-mode t)) ;if t, stop at marked position
  (if (and mpg123*cur-play-marker
	   (markerp mpg123*cur-play-marker))
      (set-marker mpg123*cur-play-marker nil))
  (if mpg123*use-face
      (progn
	(if mpg123-auto-redraw
	    (setq mpg123*window-width (window-width)))
	(and mpg123*cur-overlay (delete-overlay mpg123*cur-overlay))
	(and mpg123*slider-overlay
	     (delete-overlay mpg123*slider-overlay))))
  (setq mpg123*cur-play-marker (point-marker))
  (skip-chars-forward " ")
  (if (or (not (looking-at "[0-9]"))
	  (not (mpg123:in-music-list-p)))
      nil ;;if not on music line, then exit
    (let ((continue (equal mpg123*cur-music-number (mpg123:get-music-number)))
	  (file-name-coding-system mpg123-file-name-coding-system)
	  music p)
      (setq mpg123*cur-music-number (mpg123:get-music-number)
	    mpg123*cur-total-frame (mpg123:get-music-info
				    mpg123*cur-music-number 'frames))

      (skip-chars-forward "^ ")
      (skip-chars-forward " ")
      (setq music (mpg123:get-music-info mpg123*cur-music-number 'filename)
	    mpg123*cur-music-file music)
      (cond
       ((fboundp 'code-convert-string)
	(setq music (code-convert-string
		     music mpg123-process-coding-system '*internal*))))
      (and (eq mpg123-mixer-type 'mixer.exe)		;convert to dos filename for
	   (fboundp 'unix-to-dos-file-name)	;music over shared folder(Win)
	   (setq music (unix-to-dos-file-name music)))
      (cond
       (startframe (setq mpg123*cur-start-frame startframe))
       ((or (looking-at mpg123*default-time-string))
	(setq mpg123*cur-start-frame "0"))
       ((null mpg123-preserve-playtime) (setq mpg123*cur-start-frame "0"))
       (t
	(let ((time (mpg123:buffer-substring
		     (point)
		     (progn (skip-chars-forward "^ /")(point)))))
	  (if (and (string= time mpg123*cur-playtime)
		   mpg123*cur-playframe continue)
	      (setq mpg123*cur-start-frame
		    (int-to-string mpg123*cur-playframe))
	    (setq mpg123*cur-start-frame
		  (mpg123:time2frame time music))))))
      (setq mpg123*time-setting-mode nil)
      (fset 'mpg123:slider-check 'mpg123:null) ;defaults to null
      (if mpg123*use-face
	  (let ((frames (mpg123:get-music-info mpg123*cur-music-number 'frames))
		(istart mpg123*end-of-list-marker))
	    (overlay-put (setq mpg123*cur-overlay
			       (make-overlay
				(save-excursion (beginning-of-line) (point))
				(save-excursion (end-of-line) (point))))
			 'face
			 'mpg123-face-cur)
	    (if mpg123-need-slider
		(progn
		  (fset 'mpg123:slider-check 'mpg123:slider-check-1)
		  (if frames
		      (setq istart
			    (+ istart
			       (/ (* (mpg123:str2int mpg123*cur-start-frame)
				     (mpg123:window-width))
				  frames))))
		  (overlay-put (setq mpg123*slider-overlay
				     (make-overlay istart (1+ istart)))
			       'face
			       'mpg123-face-slider)))))
      (setq mpg123:time-regexp (mpg123:time-regexp music)
	    mpg123:init-frame-regexp (mpg123:init-frame-regexp music)
	    mpg123:frame-regexp (mpg123:frame-regexp music)
	    mpg123*convert-frame-function (mpg123:convert-frame-function
					   music))
      (set-process-filter
       (setq p (apply 'start-process "mpg123"
		      (current-buffer)
		      (mpg123:get-command-name music)
		      (delq nil
			    (append
			     (list
			      "-v" "-k" mpg123*cur-start-frame)
			     (mpg123:get-command-args music)
			     (list music)))))
       (if (and (mpg123:get-music-info mpg123*cur-music-number 'length)
		(mpg123:get-music-info mpg123*cur-music-number 'frames))
	   'mpg123:filter
	 'mpg123:initial-filter))
      (or (let ((b (get 'mpg123:sentinel 'current-buffer)))
	    ;; If the sentinel is called when the user operates in
	    ;; minibuffer, suppress message
	    (if b (or (eq b (window-buffer (minibuffer-window)))
		      (and (fboundp 'minibuffer-frame-list)
			   (memq b (minibuffer-frame-list))))))
	  (message "%s %s.."
		   mpg123*cur-music-number
		   (mpg123:get-music-info mpg123*cur-music-number 'name)))
      (set-process-sentinel p 'mpg123:sentinel)
      ;;display slider is little bit heavy, doit after starting process
      (run-hooks 'mpg123-song-started-hook)
      (mpg123-display-slider))))

(defun mpg123:sure-kill (p)
  "Waiting process to be killed."
  (let ((retry (if (fboundp 'float) 50 5))) ;retry in seconds
    (while (and p (eq (process-status p) 'run)
		(>= (setq retry (1- retry)) 0))
      (if (featurep 'xemacs)
	  (process-send-signal 'SIGTERM p)
	(interrupt-process p))
      (if (fboundp 'float)
	  (sit-for (string-to-number "0.1"))
	(sit-for 1))
      (if (input-pending-p)
	  (cond
	   ((fboundp 'read-event) (read-event))
	   ((fboundp 'next-command-event) (next-command-event))
	   (t (read-char))))
      (message "Waiting for process to exit..."))
    (message "")
    (if (and p (eq (process-status p) 'run))
	(error "Cannot terminate %s process" (process-name p)))))

(defun mpg123:kill-current-music (&optional proc)
  (let ((p (or proc (get-buffer-process (current-buffer)))))
    (and p (eq (process-status p) 'run)
	 (progn
	   (setq mpg123*interrupt-p t)
	   (mpg123:sure-kill p)))))

(defun mpg123-play-stop (&optional start-frame)
  "Play or Stop"
  (interactive)
  (let ((p (get-buffer-process (current-buffer)))
	now-stopped
	music)
    (if (and p (eq (process-status p) 'run))
	(progn
	  (mpg123:kill-current-music p)
	  (message "PAUSE!")
	  (setq now-stopped t)))
    (if (and now-stopped
	     (not mpg123*time-setting-mode)
	     (= (save-excursion (beginning-of-line) (point))
		(save-excursion (goto-char mpg123*cur-play-marker) (point))))
	nil ;if on the current music, do nothing (?)
      (mpg123:sure-kill p)
      (setq mpg123*time-setting-mode nil
	    mpg123*interrupt-p nil)
      (mpg123:play start-frame))))

(defun mpg123-mouse-play-stop (ev)
  "Play-Stop on current music."
  (interactive "e")
  (set-buffer mpg123*buffer)
  (if (and mpg123*cur-play-marker (markerp mpg123*cur-play-marker))
      (goto-char mpg123*cur-play-marker)
    (goto-char (point-min)))
  (mpg123-play-stop)
  (mouse-set-point ev))

;;;Mouse related functions from Seiichi Namba 2000/12
(defun mpg123-mouse-force-play (event)
  "Immediately move to line on click EVENT, and play the file.
Also you can hit \"^-----------\" line in *mpg123* buffer as if the
line were a scale corresponding to the length of the current music.
If 30% from the left was hit, the playing location jumps to that
percentage in the length of the song etc.
曲名のところに合わせてクリックするとその曲を直ちに演奏する。
曲一覧下部の ------ という行は曲全体の長さを示しており、ここでクリック
すると曲のその位置に直ちにジャンプする。"
  (interactive "e")
  (mouse-set-point event)
  (let (p)
    (cond
     ((mpg123:in-indicator-p)
      (mpg123:jump-to-pos))
     ((mpg123:in-music-list-p)
      (mpg123:kill-current-music)
      (mpg123-play-stop "0"))
     (t (message "Bad bad click, brother")))))

(defun mpg123:jump-to-pos ()
  "Jump to current music's certain position according to current column."
  (let ((p (get-buffer-process (current-buffer)))
	(c (current-column))
	(w (mpg123:window-width))
	frames target)
    (if (null mpg123*cur-music-number) nil
      (mpg123:kill-current-music)
      (setq frames (mpg123:time2frame
		    (mpg123:get-music-info mpg123*cur-music-number 'length)
		    (mpg123:get-music-info mpg123*cur-music-number 'filename))
	    target (/ (* (mpg123:str2int frames) c) w))
      (message "new = %d" target)
      (save-excursion
	;;set buffer in my responsibility
	(set-buffer (marker-buffer mpg123*cur-play-marker))
	(mpg123:goto-playtime-position)
	(mpg123:play (int-to-string target))))))

(defun mpg123-play ()
  "PLAY(from the beginning of the music)."
  (interactive)
  (cond
   ((mpg123:in-indicator-p) (mpg123:jump-to-pos))
   (t (mpg123-play-stop "0"))))

(defun mpg123-< (arg)
  "Rewind music by one."
  (interactive "p")
  (let ((p (get-buffer-process (current-buffer))))
    (cond
     ;;If currently playing, then rewind and start
     ((and p (eq (process-status p) 'run))
      (goto-char mpg123*cur-play-marker)
      (if (and (stringp mpg123*cur-playtime)
	       (string< mpg123*cur-playtime "00:05")) ; is 00:02 readsonable?
	  (progn
	    (mpg123:update-playtime "00:00")
	    (mpg123-prev-line arg)))
      (mpg123:kill-current-music p)
      (or mpg123-preserve-playtime (mpg123:update-playtime "00:00"))
      (mpg123:play "0")) ;play from frame#0
     ;;else go back to previous line
     (t
      (mpg123-prev-line arg)))))

(defun mpg123-> (arg)
  "Skip forward"
  (interactive "p")
  (let ((p (get-buffer-process (current-buffer))))
    (cond
     ;;If currently playing, then go forward and start
     ((and p (eq (process-status p) 'run))
      (goto-char mpg123*cur-play-marker)
      (mpg123-next-line arg)
      (mpg123:kill-current-music p)
      (if (and (>= (point) mpg123*end-of-list-marker)
	       (mpg123:repeat-check))
	  (goto-char (point-min)))
      (mpg123:play "0")) ;play from frame#0
     ;;else go to next line
     (t
      (mpg123-next-line arg)))))

(defun mpg123-mark-position ()
  "Mark the playing position now."
  (interactive)
  (let ((f (max 0 (- mpg123*cur-playframe ;more 1.5sec back
		     (/ (mpg123:str2int (mpg123:time2frame "00:02")) 3)))))
    (mpg123:set-music-info mpg123*cur-music-number 'mark (format "%d" f))
    (mpg123:set-music-info
     mpg123*cur-music-number 'marktime mpg123*cur-playtime)
    (message (mpg123:lang-msg
	      "Mark the position of [%s].  Push `%s' to restart here"
	      "[%s] の今の位置を記憶. ここに戻るには `%s' をタイプ.")
	     mpg123*cur-playtime
	     (substitute-command-keys "\\[mpg123-refrain]"))))

(defun mpg123-introduction-quiz-mode (&optional arg)
  "Toggle intro-quiz mode"
  (interactive "P")
  (setq mpg123-introduction-quiz-mode
	(if (or (and (numberp arg)
		     (> arg 0))
		(and
		 (not (and (numberp arg) (< arg 1)))
		 (not mpg123-introduction-quiz-mode)))
	    t))
  (message "%s %s"
	   (mpg123:lang-msg "Introduction-quiz mode" "イントロクイズモード")
	   (if mpg123-introduction-quiz-mode "ON" "OFF")))

(defun mpg123-refrain ()
  "Refrain from marked position."
  (interactive)
  (let ((frame (mpg123:get-music-info mpg123*cur-music-number 'mark)))
    (if frame
	(progn
	  (mpg123:kill-current-music)
	  (if mpg123-introduction-quiz-mode
	      ;; mpg123-introduction-quiz-mode is number, do not stop at
	      ;; marked position
	      (setq mpg123-introduction-quiz-mode 1))
	  (mpg123:play
	   (mpg123:get-music-info mpg123*cur-music-number 'mark)))
      (message (mpg123:lang-msg
		"No position for refrain marked. Type `%s' to mark position"
		"この曲は位置記録してないす. `%s' をタイプして記録すべし")
	       (substitute-command-keys "\\[mpg123-mark-position]")))))

(defun mpg123-where-is-mark ()
  "Print the current music's marked position."
  (interactive)
  (let ((marktime (mpg123:get-music-info mpg123*cur-music-number 'marktime))
	(frame (mpg123:get-music-info mpg123*cur-music-number 'mark)))
    (if marktime
	(message (mpg123:lang-msg
		  "Mark for music#%d is [%s] (frame#%s)"
		  "曲番号#%d の記憶位置は [%s] (#%s フレーム)")
		 mpg123*cur-music-number marktime frame)
      (message (mpg123:lang-msg
		"No mark set for music#%d"
		"この曲は位置記録してないのじゃが...")
	       mpg123*cur-music-number))))

(defun mpg123:add-time-string (time seconds)
  (if (string-match "\\([0-9]+\\):\\([0-9]+\\)" time)
      (let ((m (mpg123:str2int
		(substring time (match-beginning 1) (match-end 1))))
	    (s (mpg123:str2int
		(substring time (match-beginning 2) (match-end 2))))
	    tm)
	(setq tm (mpg123:add-time m s -1))
	(format "%02d:%02d" (car tm) (cdr tm)))))

(defun mpg123:add-time (m s add &optional max)
  (let ((x (max 0 (+ (* m 60) s add))))
    (if max (setq x (min max x)))
    (setq m (/ x 60)
	  s (- x (* 60 m)))
    (cons m s)))

(defun mpg123-forward (arg)
  "forw"
  (interactive "p")
  (let ()
    ;;set buffer in my responsibility
    (set-buffer (marker-buffer mpg123*cur-play-marker))
    (if mpg123*time-setting-mode
	(progn
	  (beginning-of-line)
	  (skip-chars-forward "^:")
	  (forward-char -2))
      (setq mpg123*time-setting-mode t)
      (mpg123:goto-playtime-position))
    (if (and (looking-at "\\([0-9]+\\):\\([0-9]+\\)/\\([0-9]+\\):\\([0-9]+\\)")
	     (match-beginning 3)
	     (match-beginning 4))
	(let*((m (mpg123:str2int (mpg123:match-string 1)))
	      (s (mpg123:str2int (mpg123:match-string 2)))
	      (n (mpg123:get-music-number))
	      (c (current-column))
	      time M S T)
	  (setq M (mpg123:str2int (mpg123:match-string 3))
		S (mpg123:str2int (mpg123:match-string 4))
		T (+ (* 60 M) S))
	  (if (and (= m 0) (= s 0) (< arg 0)
		   ;;Already rewind to 00:00 and arg is negative
		   (progn (mpg123-next-line -1)
			  (/= n (mpg123:get-music-number))))
	      (let*((prevnum (mpg123:get-music-number))
		    (len (mpg123:get-music-info prevnum 'length)))
		(if len
		    (progn		;set playtime to maxlength
		      (move-to-column c)
		      (mpg123:update-playtime len 'here)))
		(mpg123-forward arg))
	    (setq time (mpg123:add-time m s arg T))
	    (move-to-column c)
	    (mpg123:update-playtime
	     (format "%02d:%02d" (car time) (cdr time)) 'here)
	    (message
	     (mpg123:lang-msg
	      "Time Slide mode: Type `SPC' to play in that position"
	      "タイムスライドモード: `SPC' を押してその位置から再生"))))
      (message
       (mpg123:lang-msg
	"Length not known.  Play this music once please."
	"曲長不明. まずは一回再生してね.")))))

(defun mpg123-backward (arg)
  "Rew"
  (interactive "p")
  (mpg123-forward (- arg)))

(defun mpg123-forward-10 (arg)
  "Forw by (ARG*10)sec"
  (interactive "p")
  (mpg123-forward (* 10 arg)))

(defun mpg123-backward-10 (arg)
  "Rew"
  (interactive "p")
  (mpg123-forward (* -10 arg)))

;; 
;; mpg123-refresh-tag contributed by N. SHIMIZU <CZA06074@nifty.com>
;;
(defun mpg123-refresh-tag ()
  "Refresh line of edited file."
  (switch-to-buffer (get-buffer-create mpg123*buffer))
  (goto-char mpg123*cur-edit-marker)
  (mpg123-refresh-tag-at-point))

(defun mpg123-refresh-tag-at-point ()
  "Refresh tags for file under point."
  (cond
   ((mpg123:in-music-list-p)
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (beginning-of-line)
    (let (f name) 
      (setq mpg123*cur-music-number (mpg123:get-music-number))
      (setq f (mpg123:get-music-info mpg123*cur-music-number 'filename))
      (setq name (if (fboundp mpg123-id3-tag-function)
                     (funcall mpg123-id3-tag-function f)
                   (file-name-nondirectory f)))
      (mpg123:set-music-info mpg123*cur-music-number 'name name)
      (search-forward "\t " nil t)
      (insert-before-markers name))
    (delete-region (point)
                   (progn (end-of-line 1) (point)))
    (beginning-of-line)
    (skip-chars-forward "^:")
    (setq buffer-read-only t))))

(defun mpg123-open-new (dir)
  "Open new directory or playlist."
  (interactive "Fmpg123 on directory or playlist: ")
  (if (not (file-exists-p dir))
      (error "Not such file or directory: %s" dir))
  (mpg123:kill-current-music)
  (mpg123 dir))

(defun mpg123:playlist-p (file)
  "Check if FILE can be seemed to be a playlist"
  (and
   (file-exists-p file)
   (not (file-directory-p file))
   ;(not (mpg123:sound-p file))
   (save-excursion
     (let ((b (set-buffer (get-buffer-create " *mpg123pl*")))
	   (PATH_MAX 1024)
	   (dir (file-name-directory file)))
       (erase-buffer)
       (insert-file-contents file nil 0 PATH_MAX)
       (goto-char (point-min))
       (while (looking-at "\\s *#") (forward-line 1))
       (skip-chars-forward "[ \t]")
       (prog1
	   (let ((name(mpg123:buffer-substring
		       (point)
		       (progn (re-search-forward "///\\|$")
			      (goto-char (match-beginning 0))
			      (skip-chars-backward "[ \t\r]") (point)))))
	     (and
	      (file-exists-p (expand-file-name name dir))
	      (mpg123:get-sound-type name)))
	 (kill-buffer b))))))

(defun mpg123-add-new (file)
  "Add a new file or directory to the playist"
  (interactive "Fmpg123 add to playlist: ")
  ;;(cd (file-name-directory file))
  (setq file (expand-file-name file))
  (cond ((file-directory-p file)
         ;; Add all files and playlists in a directory
         (mpg123-add-to-playlist (mpg123:mp3-files-in-dir file)))
	((mpg123:sound-p file)
	 (mpg123-add-to-playlist (list file)))
        (;(string-match mpg123-playlist-regexp file)
	 (mpg123:playlist-p file)
         ;; Add all files in a playlist file
         (mpg123-add-to-playlist (mpg123:mp3-files-in-list file)))
        ((string-match mpg123-url-regexp file)
         ;; Add file[s] from an external URL
         (error "mpg123: URL's not yet supported"))
        (t
         ;; Add a single file
         (mpg123-add-to-playlist (list file)))))

(defun mpg123-increase-repeat-count (arg)
  "Increase repeat count."
  (interactive "p")
  (setq mpg123*cur-repeat-count
	(min 99 (max -1 (+ arg mpg123*cur-repeat-count))))
  (mpg123:update-repeat-count))

(defun mpg123-decrease-repeat-count (arg)
  "Decrease repeat count."
  (interactive "p")
  (mpg123-increase-repeat-count (- arg)))


(defun mpg123-loop-dummy ()
  "Dummy function to cause mpg123-set-point-for-next-song-function to loop"
  (if (not (= mpg123*cur-loop-count -1))
    (mpg123-decrease-loop-count 1))
  t)


(defun mpg123-increase-loop-count (arg)
  "Increase loop count."
  (interactive "p")
  (setq mpg123*cur-loop-count
	(min 99 (max -1 (+ arg mpg123*cur-loop-count))))
  (cond 
   ((or (= mpg123*cur-loop-count 0) (eq mpg123*cur-loop-count nil))
    (setq mpg123-set-point-for-next-song-function nil))
   (t 
    (setq mpg123-set-point-for-next-song-function 'mpg123-loop-dummy)))
  (mpg123:update-loop-count))


(defun mpg123-decrease-loop-count (arg)
  "Decrease loop count."
  (interactive "p")
  (if (= mpg123*cur-loop-count -1)
      (mpg123-increase-loop-count 1)
    (mpg123-increase-loop-count (- arg))))


(defun mpg123:get-music-number ()
  "Get current line's music number."
  (save-excursion
    (let ((md (match-data)))
      (beginning-of-line)
      (skip-chars-forward " \t" nil)
      (unwind-protect
	  (and
	   (looking-at "[0-9]+")
	   (mpg123:str2int
	    (mpg123:match-string 0)))
	(store-match-data md)))))

(defvar mpg123-popup-window-height 8)
(defun mpg123:popup-buffer (buffer)
  "Popup specified BUFFER."
  (cond
   ((get-buffer-window buffer)
    (select-window (get-buffer-window buffer)))
   ((one-window-p)
      (let ((h (max window-min-height
		    (- (window-height) mpg123-popup-window-height))))
	(split-window nil h)
	(other-window 1)
	(switch-to-buffer buffer)))
   (t
    (other-window 1)
    (switch-to-buffer buffer))))

(defvar mpg123*stack-buffer " *mpg123 stack*")
(defvar mpg123*music-in-stack nil "List of the music in the stack")

(defun mpg123-kill-line (arg)
  "Kill current music line and move it to the stack."
  (interactive "p")
  (beginning-of-line)
  (if (mpg123:in-music-list-p)
      (let ((sb (get-buffer-create mpg123*stack-buffer))
	    n current (stack "") buffer-read-only p
	    (sw (selected-window)))
	(save-excursion
	  (set-buffer sb)
	  (use-local-map mpg123-mode-map))
	(while (and (> arg 0) (mpg123:in-music-list-p))
	  (setq n (mpg123:get-music-number))
	  (setq current
		(and (markerp mpg123*cur-play-marker)
		     (eq (marker-buffer mpg123*cur-play-marker)
			 (current-buffer))
		     (= (point) mpg123*cur-play-marker)))
	  (delete-region (point)
			 (progn (forward-line 1) (point)))
	  (save-excursion
	    (set-buffer sb)
	    (goto-char (point-min))
	    (if current
		(progn
		  (set-marker mpg123*cur-play-marker nil)
		  (setq mpg123*cur-play-marker (point-marker))
		  (setq current nil p (point))
		  (insert (mpg123:format-line n))
		  (if (overlayp mpg123*cur-overlay)
		      (move-overlay
		       mpg123*cur-overlay p (point) (current-buffer))))
	      (insert-before-markers (mpg123:format-line n))))
	  (setq mpg123*music-in-stack (cons n mpg123*music-in-stack))
	  (setq arg (1- arg)))
	(mpg123:popup-buffer sb)
	(goto-char (point-min))
	(select-window sw))))

(defun mpg123-kill-stack ()
  "Kill all music list in stack."
  (interactive)
  "Not yet implemented")

(defun mpg123-yank-line (arg)
  "Yank music line from stack buffer."
  (interactive "p")
  (beginning-of-line)
  (if (= mpg123*end-of-list-marker (point))
      (let (buffer-read-only)
	(insert-before-markers "\t") ;dirty hack!
	(backward-char 1)))
  (if (mpg123:in-music-list-p)
      (let ((sb (get-buffer-create mpg123*stack-buffer))
	    (sw (selected-window)) stackw
	    n buffer-read-only current p)
	(beginning-of-line)
	(mpg123:popup-buffer sb)
	(setq stackw (selected-window))
	(goto-char (point-min))
	(while (and (setq n (mpg123:get-music-number))
		    (> arg 0))
	  (or (eq (car mpg123*music-in-stack) n)
	      (error "Stack buffer is obsolete.  Please reopen this directory."))
	  (setq current
		(and (markerp mpg123*cur-play-marker)
		     (eq (marker-buffer mpg123*cur-play-marker) (current-buffer))
		     (= (point) mpg123*cur-play-marker)))
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (save-excursion
	    (set-buffer mpg123*buffer)
	    (if current
		(progn
		  (set-marker mpg123*cur-play-marker nil)
		  (setq mpg123*cur-play-marker (point-marker))
		  (setq current nil p (point))
		  (insert (mpg123:format-line n))
		  (if (overlayp mpg123*cur-overlay)
		      (move-overlay
		       mpg123*cur-overlay p (point) (current-buffer))))
	      (insert-before-markers (mpg123:format-line n))))
	  (setq mpg123*music-in-stack (cdr mpg123*music-in-stack))
	  (setq arg (1- arg)))
	(if (progn (set-buffer sb)
		   (= (buffer-size) 0))
	    (progn
	      (delete-window stackw)
	      (kill-buffer sb)))
	(select-window sw)
	(if (eq ?\t (char-after (1- mpg123*end-of-list-marker)))
	    (save-excursion
	      (goto-char mpg123*end-of-list-marker)
	      (delete-backward-char 1))) ;dirty hack
	)))

(defun mpg123:display-slider-sub ()
  (if nil nil
    (let ((window-min-height 2)
	  (height (if (featurep 'xemacs) 4 3)))
      (split-window nil (- (window-height) height))
      (set-window-start (next-window)
			(overlay-start mpg123*indicator-overlay)))))

(defun mpg123:delete-windows (windows)
  (while windows
    (delete-window (car windows))
    (setq windows (cdr windows))))

(defun mpg123-display-slider ()
  "Show up slidebar in the next window of mpg123 buffer itself."
  (interactive)
  (let*((selw (selected-window))
	(cb (current-buffer))
	(mpgw (if (eq (window-buffer selw) mpg123*buffer)
		  selw
		(mpg123:get-largest-buffer-window mpg123*buffer nil t)))
	;(mpgf (window-frame mpgw))
	mpgwinlist nowvisible needed-lines numlines (numwin 1)
	w redrawp (i 300))
    (unwind-protect
	(if (or (null mpgw)			;if no mpg123 buffer is visible
		(null mpg123-display-slider)
		(null mpg123-need-slider)
		(null mpg123*cur-play-marker))
	    nil				;no need to show, maybe
	  ;(select-frame mpgf)
	  (select-window mpgw)
	  (setq redrawp (not (pos-visible-in-window-p mpg123*cur-play-marker)))
	  (goto-char mpg123*cur-play-marker)
	  (if redrawp
	      (set-window-start
	       nil (save-excursion (forward-line -1) (point))))
	  (sit-for 0)
	  ;;(while (and (not (pos-visible-in-window-p (point)))
	  ;;	      (> i 0))		; workaround for emacs-21.[123]
	  ;;  (sit-for 0.1)
	  ;;  (setq i (1- i)))
	  (setq nowvisible (pos-visible-in-window-p
			    (overlay-start mpg123*indicator-overlay))
		numlines (window-height)
		needed-lines (1+ (count-lines
				  (point-min)
				  (overlay-start mpg123*indicator-overlay))))
	  ;; count mpg123*buffer in this frame
	  (while (not (eq (setq w (next-window w)) mpgw))
	    (if (and (not (eq mpgw w))
		     (eq (window-buffer w)
			 (get-buffer mpg123*buffer)))
		(setq numwin (1+ numwin)
		      numlines (window-height w)
		      mpgwinlist (cons w mpgwinlist))))

	  (cond
	   ((< needed-lines numlines)	;window-height is enough
	    (if (> numwin 1)		;delete other mpg123-window
		(mpg123:delete-windows mpgwinlist)))
	   ((eq mpg123-display-slider 'always)
	    (if (> numwin 1)
		(progn
		  (select-window (car mpgwinlist))
		  (set-window-start
		   nil (overlay-start mpg123*indicator-overlay)))
	      (mpg123:display-slider-sub)))
	   (t
	    ;; mpg123*buffer window is selected here.
	    (cond
	     (nowvisible
	      (if (> numwin 1)
		  (mpg123:delete-windows mpgwinlist)))
	     ((= numwin 1)
	      (mpg123:display-slider-sub))))))
      ;; for XEmacs, select previous window
      (if selw
	  (progn
	    (focus-frame (window-frame selw))
	    (select-window selw)
	    (sit-for 0))))))

(defun mpg123-shuffle (&optional method)
  "Shuffle the music!
From Lisp program, you can specify one of order/Inverse/Random by
optional argument METHOD.  Set one of ?o or ?i or ?r."
  (interactive)
  (message (mpg123:lang-msg
	    "Shuffle music by...(O)rder, (I)nverse order, (R)andom: "
	    "並べ替え方法: O=>番号順   I=>逆順  R=>ランダム"))
  (let ((c (or method (read-char)))
	ord (n 0) (l (length mpg123*music-alist))
	r tmp buffer-read-only (p (point)) currentp
	(number-list (sort (mapcar (function (lambda (s) (car s)))
				   mpg123*music-alist)
			   '<)))
    (cond
     ((eq c ?o)
      (setq n l)
      (while (>= (setq n (1- n)) 0)
	(setq ord (cons (nth n number-list) ord))))
     ((eq c ?i)
      (while (< n l)
	(setq ord (cons (nth n number-list) ord)
	      n (1+ n))))
     ((eq c ?r)
      (setq ord (make-vector l nil))
      (while (< n l)
	(aset ord n (nth n number-list)) (setq n (1+ n)))
      (while (>= (setq n (1- n)) 0)
	(setq r (random l)
	      tmp (aref ord r))
	(aset ord r (aref ord n))
	(aset ord n tmp)))
     (t (error "Canceled")))
    (setq n 0)
    (goto-char (point-min))
    (if (and mpg123*cur-play-marker
	     (markerp mpg123*cur-play-marker)
	     (eq (marker-buffer mpg123*cur-play-marker) (current-buffer)))
	(set-marker mpg123*cur-play-marker nil))
    (while (< n l)
      (if (memq (elt ord n) mpg123*music-in-stack)
	  nil
	(if (setq currentp (equal (elt ord n) mpg123*cur-music-number))
	    (progn
	      (setq mpg123*cur-play-marker (point-marker))
	      (if (overlayp mpg123*cur-overlay)
		  (move-overlay mpg123*cur-overlay
				(point)
				(progn
				  (insert (mpg123:format-line (elt ord n)))
				  (point))
				(current-buffer))
		(insert (mpg123:format-line (elt ord n)))))
	  (insert (mpg123:format-line (elt ord n)))))
      (setq n (1+ n)))
    (delete-region (point) mpg123*end-of-list-marker)
    (goto-char p)))

(defun mpg123-delete-file (&optional command)
  "Delete audio file on the point.
When called from function, optional argument COMMAND directly select the job."
  (interactive)
  (if (not (mpg123:in-music-list-p))
    (error "Not on music list"))
  (let*((n (mpg123:get-music-number)) p c
	(file (mpg123:get-music-info n 'filename)))
    (if command
	(setq c command)
      (message (mpg123:lang-msg
		"Delete file?(%s) [Y]es, [L]from list, [N]o"
		"消してええ?(%s) Y=>よか  L=>曲一覧から  N=>だめ")
	       (file-name-nondirectory file))
      (setq c (read-char)))
    (cond
     ((memq c '(?y ?Y ?L ?l))
      (beginning-of-line)
      (setq p (point))
      (if (and mpg123*cur-play-marker
	       (marker-position mpg123*cur-play-marker)
	       (eq (point)
		   (save-excursion
		     (goto-char mpg123*cur-play-marker)
		     (beginning-of-line)
		     (point))))
	  (save-excursion (mpg123-> 1)))
      (let ((buffer-read-only nil))
	(if (memq c '(?Y ?y)) (delete-file file))
	(delete-region (point)
		       (progn (forward-line 1) (point)))
	(mpg123:delete-music-from-list n)))
     (t
      (message (mpg123:lang-msg "Canceled" "な、やめとこな"))))))

(defun mpg123-quit (&optional yes)
  "Quit"
  (interactive)
  (let ((p (get-buffer-process (current-buffer)))
	(buffers (list mpg123*buffer mpg123*info-buffer mpg123*stack-buffer
		  " *mpg123tmp* " " *mpg123 tag tmp*" " *mpg123 mixer* ")))
    (if (and p
	     (eq (process-status p) 'run)
	     (or yes (y-or-n-p "Kill current music?")))
	(mpg123:sure-kill p)
      (setq buffers (delete mpg123*buffer buffers))
      (bury-buffer mpg123*buffer)
      (if (buffer-name mpg123*initial-buffer) ;buffer-live-p check(mule2 OK)
	  (switch-to-buffer mpg123*initial-buffer)))
    (setq mpg123*interrupt-p 'quit)
    (message "")
    (mapcar '(lambda (b) (and (get-buffer b) (kill-buffer b))) buffers)))

(defun mpg123-quit-yes ()
  "Force to quit"
  (interactive)
  (mpg123-quit t))

(defun mpg123:mp3-files-in-dir (dir)
  "Return mp3 files in a directory"
  (let*((default-file-name-coding-system mpg123-file-name-coding-system)
	(files (sort (directory-files dir) 'string<)) f mp3s)
    (while files
      (message "Inspect file %s..." (car files))
      (setq f (expand-file-name (car files) dir))
      (and (not (file-directory-p f))
	   (file-readable-p f)
	   (if (mpg123:sound-p f) (setq mp3s (cons f mp3s))))
      (setq files (cdr files)))
    (message "")
    (nreverse mp3s)))

(defun mpg123:mp3-files-in-list1 (file)
  (save-excursion
    (let ((buf (find-file-noselect file))
          (dir (file-name-directory file)) f m0 mp3s marktime)
      (put 'mpg123:mp3-files-in-list 'parsed
	   (cons (cons (expand-file-name file) nil)
		 (get 'mpg123:mp3-files-in-list 'parsed)))
      (set-buffer buf)
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \t")
	(setq f (mpg123:buffer-substring
		 (point)
		 (progn
		   (re-search-forward "\\(///\\)\\|$" nil t)
		   (setq m0 (match-beginning 0))
		   (setq marktime
			 (if (match-beginning 1)
			     (progn
			       (looking-at "[0-9:]+")
			       (mpg123:buffer-substring
				(match-beginning 0) (match-end 0)))))
		   (goto-char m0)
		   (skip-chars-backward " \t")
		   (point))))
		   
        (and f ;;(looking-at "^[ \t]*\\(.*[^ \t\n\r]\\)[ \t\n\r]*$")
             (setq f (expand-file-name f dir))
             (message "Inspect file %s..." f)
             (not (file-directory-p f))
             (file-readable-p f)
	     (cond
	      ((mpg123:sound-p f)
	       (setq mp3s (cons (if marktime (cons f marktime) f) mp3s)))
	      ((and
		(not (assoc (expand-file-name f)
			    (get 'mpg123:mp3-files-in-list 'parsed)))
		(mpg123:playlist-p f))
	       (setq mp3s (append (mpg123:mp3-files-in-list1 f) mp3s)))))
        (message "")
        (forward-line 1))
      (if (buffer-modified-p buf)
          (bury-buffer buf)
        (kill-buffer buf))
      mp3s)))

(defun mpg123:mp3-files-in-list (file)
  "Return mp3 files in a playlist"
  (put 'mpg123:mp3-files-in-list 'parsed nil)
  (nreverse (mpg123:mp3-files-in-list1 file)))

(defun mpg123:squeeze-spaces-buffer ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "\\s +") (replace-match ""))
    (save-excursion
      (while (search-forward "\0" nil t)
	(replace-match "")))
    (save-excursion
      (while (search-forward "　" nil t) (replace-match " ")))
    (save-excursion
      (while (re-search-forward "\\s \\s +" nil t)
      (replace-match " ")))
    (if (re-search-forward "\\s +$" nil t)
      (replace-match ""))))

(defun mpg123:peek-id3-tag (file)
  "Try peeking id3tag from FILE"
  (let ((sz (nth 7 (file-attributes (file-truename file))))
	(b (get-buffer-create " *mpg123 tag tmp*"))
	(file-name-coding-system mpg123-file-name-coding-system)
	(case-fold-search nil)
	title artist album tracknum)
    (save-excursion
      (set-buffer b)
      (erase-buffer)
      (mpg123:insert-raw-file-contents file nil (- sz 128) (- sz 125))
      (if (looking-at "TAG")
	  (progn
	    (erase-buffer)
	    (insert-file-contents file nil (- sz 125) (- sz 95))
	    (mpg123:squeeze-spaces-buffer)
	    (setq title (buffer-string))
	    (erase-buffer)
	    (insert-file-contents file nil (- sz 95) (- sz 65))
	    (mpg123:squeeze-spaces-buffer)
	    (setq artist (buffer-string))
	    (erase-buffer)
	    (insert-file-contents file nil (- sz 65) (- sz 35))
	    (mpg123:squeeze-spaces-buffer)
	    (setq album (buffer-string))
	    (erase-buffer)
	    (insert-file-contents file nil (- sz 3) sz)
	    ;; id3v1.1 last byte of comment is tracknum (if prev byte is null):
	    (let ((track (char-after 2)))
	      (if (and (= (char-after 1) ?\000)
		       (not (= track ?\000)))
		  (setq tracknum (format "%d" track))))

	    ;; common hack: comment is "Track num" 
	    (if tracknum
		nil
	      (erase-buffer)
	      (mpg123:insert-raw-file-contents
	       file nil (- sz 31) (- sz 1)) ;ID3v1.1
	      (mpg123:squeeze-spaces-buffer)
	      (if (looking-at "Track \\([0-9]+\\)")
		  (setq tracknum (mpg123:match-string 1))))
	    (kill-buffer b)
	    (if (fboundp mpg123-format-name-function)
		(funcall mpg123-format-name-function artist album title tracknum (file-name-nondirectory file)) 
	      (concat (if (string< "" title) title
			(file-name-nondirectory file))
		      (if (or mpg123-omit-id3-artist
			      (string= artist "")
			      (string-match "[Uu]nknown [Aa]rtist" artist))
			  ""
			(concat " by " artist)))))
	(kill-buffer b)
	(setq file (file-name-nondirectory file))
	(cond
	 ((fboundp 'code-convert-string)
	  (code-convert-string file mpg123-process-coding-system '*internal*))
	 (t file))))))

(defun mpg123:file-substring (file begin end &optional offset)
  "Return FILE's substring from point value BEGIN to END.
Note that buffer point is 1-origin, while file-offset is 0-origin.
So insert-file-contents takes regions decremented by 1.
Optional 4th arg OFFSET is added to BEGIN and END."
  (let ((tmpbuf (get-buffer-create " *mpg123 filetmp*"))
	(ofs (or offset 0)))
    (save-excursion
      (set-buffer tmpbuf)
      (erase-buffer)
      (mpg123:insert-raw-file-contents file nil (+ begin ofs -1) (+ end ofs -1))
      (prog1
	  (buffer-string)
	(kill-buffer tmpbuf)))))
(defun mpg123:program-available-p (cmd)
  "Check the availability of CMD."
  (condition-case err
      (progn
	(call-process cmd nil nil)
	cmd)
    (error nil)))

(defvar mpg123-program-ogginfo "ogginfo"
  "*Program name to get ogg files information.")
(defvar mpg123*use-ogginfo
  (mpg123:program-available-p mpg123-program-ogginfo))

(defun mpg123:ogg123-tag-by-ogginfo (file)
  "Get FILE's title and artist by calling ogginfo."
  (let*((eucjp (if (coding-system-p '*euc-japan*) '*euc-japan* 'euc-jp))
	(file-name-coding-system mpg123-file-name-coding-system)
	(process-coding-system-alist
	 (list (cons mpg123-program-ogginfo (cons eucjp eucjp))))
	(buf (get-buffer-create " *mpg123tmp"))
	(title "") (artist "")
	(oldenv (getenv "LC_CTYPE")))
    (unwind-protect
	(progn
	  (setenv "LC_CTYPE" "ja_JP.eucJP")
	  (call-process mpg123-program-ogginfo nil buf nil file)
	  (save-excursion
	    (set-buffer buf)
	    (goto-char (point-min))
	    (while (re-search-forward
		    "^\\s *\\(\\(artist\\)\\|title\\)=\\(.*\\)$" nil 1)
					; 1 is essential
	      (set (if (match-beginning 2) 'artist 'title)
		   (mpg123:match-string 3)))
	    ;; Get Music Length here :)
	    (if (re-search-backward
		 "length: \\([0-9]+\\)m.\\([.0-9]+\\)s"
		 nil t)
		(put 'mpg123:add-musiclist-to-point 'length
		     (format "%02d:%02d"
			     (mpg123:str2int (mpg123:match-string 1))
			     (mpg123:str2int (mpg123:match-string 2)))))
	    (if (string-match "[Uu]nknown [Aa]rtist" artist)
		(setq artist ""))
	    (if (string< "" artist)
		(setq artist (concat " by " artist)))
	    ;; The next formatted string is return value
	    (format "%s%s"
		    (if (string< "" title) title (file-name-nondirectory file))
		    artist)))
      (setenv "LC_CTYPE" oldenv)
      (kill-buffer buf))))

(defun mpg123:ogg123-get-tag (file)
  (if mpg123*use-ogginfo
      (mpg123:ogg123-tag-by-ogginfo file)
    (mpg123:ogg123-peek-tag file)))

(defun mpg123:ogg123-peek-tag (file)
  "Peek ogg comment area.
cf. NetBSD:/usr/share/misc/magic
# A kludge to read the vendor string.  It's a counted string, not a
# libVorbis is the only one existing currently, so I detect specifically
# it.  The interesting value is the cvs date (8 digits decimal).
# Post-RC1 Ogg files have the second header packet (and thus the version)
# in a different place, so we must use an indirect offset.
>>>(84.b+85)            string          \x03vorbis
>>>>(84.b+96)           string/c        Xiphophorus\ libVorbis\ I       \b, crea
 :::
# Then come the comments, again length-counted (and number-counted).
# Some looping constructs and registers would allow reading them but now
# it's impossible.  However we can print the number of comments present
# (skipping by the vendor string length):
##>>>>(109.l.113)       lelong          0               \b, no comments
##>>>>(109.l+113)       lelong          >0              \b, %lu comments"

  (let ((tmpbuf (get-buffer-create " *mpg123 tag tmp* "))
	blen versionlen num-comments comment-len
	(ofs 84) (peekbytes 256)	;is 256 enough??
	(basename (file-name-nondirectory file))
	pt commenthead title artist)
    (save-excursion
      (set-buffer tmpbuf)
      (erase-buffer)
      (mpg123:insert-raw-file-contents file nil ofs (+ ofs peekbytes))
      (setq blen (mpg123:hex-value 0 1))
      (setq pt (+ 1 blen 1 6))	;1 is byte width, 6 is length of "vorbis"
      (if (= 0 (setq versionlen (mpg123:hex-value pt 4 'little-endian)))
	  ;; if no version header, I don't understand it..., return basename 
	  basename
	(setq pt (+ pt 4 versionlen))	;4 is long-int width
	(if (<= (setq num-comments (mpg123:hex-value pt 4 'little-endian))
		0)
	    ;; if no comments found, return basename
	    basename
	  ;; Let's go into byte stream
	  (setq commenthead (+ pt 4))	;4 for long-int of [num-comments]
	  ;; [length(Long)][VAR=Value][Length(Long)][VAR=Value]...
	  ;; repeats num-comments times
	  (while (> num-comments 0)
	    (setq comment-len (mpg123:hex-value commenthead 4 'little-endian))
	    (goto-char (+ 4 commenthead 1))
	    (cond
	     ;;Call mpg123:file-substring for the sake of coding-system
	     ;;detection.  Although it is more efficient to use
	     ;;code-detection function and convert string directly
	     ;;here, it augments dependency to emacs version.
	     ((looking-at "artist=")
	      (setq artist (cons
			    (mpg123:file-substring
			     file (+ 7 (point)) (+ comment-len (point)) ofs)
			    artist)))
	     ((looking-at "title=")
	      (setq title (cons
			   (mpg123:file-substring
			    file (+ 6 (point)) (+ comment-len (point)) ofs)
			   title))))
	    (setq commenthead (+ commenthead 4 comment-len)
		  num-comments (1- num-comments)))
	  (setq artist (mapconcat 'concat (nreverse artist) ", ")
		title  (mapconcat 'concat (nreverse title) ", "))

	  (cond
	   ((fboundp 'decode-coding-string)
	    (setq title (decode-coding-string
			 title mpg123-ogg123-id-coding-system)
		  artist (decode-coding-string
			  artist mpg123-ogg123-id-coding-system)))
	   ((fboundp 'code-convert-string)
	    (setq title (code-convert-string
			 title mpg123-ogg123-id-coding-system '*internal*)))
	   )
	  (if (string< "" title)
	      (concat title
		      (if (and (string< "" artist)
			       (not mpg123-omit-id3-artist))
			  (concat " by " artist)
			""))
	    basename))))))

(defvar mpg123-lang
  (if (and (boundp 'current-language-environment)
	   (stringp current-language-environment))
      (let ((l current-language-environment))
	(cond
	 ((string-match "[Jj]apanese" l)	1)
	 (t					0)))
    (if (or (featurep 'mule) (boundp 'mule))
	1
      0))
  "Symbolic language number of running Emacs.")

(defun mpg123:lang-msg (&rest msglist)
  (nth (if (< mpg123-lang (length msglist))
	   mpg123-lang
	 0)
       msglist))

(defun mpg123:insert-help ()
  "Insert help string to current buffer."
  (if mpg123-show-help
      (unwind-protect
	  (let ((m (if (fboundp 'm) (symbol-function 'm))))
	    (fset 'm 'mpg123:lang-msg)
	    (insert (substitute-command-keys (concat "
mpg123:
\\[mpg123-what-file]	" (m "Show the real filename" "ファイル名を表示") "
\\[mpg123-goto-current-line]	" (m "Go to current music line"
				     "現在曲の行へ移動") "
\\[mpg123-play-stop]	" (m "Play or pause" "再生/停止") "
\\[mpg123-play]	" (m "Play" "再生") "
\\[mpg123-mark-position]	" (m "Mark position (when playing)"
				     "現在の再生位置をマーク(再生中のみ)") "
\\[mpg123-refrain]	" (m "Restart from marked position"
			     "マークした位置に戻って再生") "
\\[mpg123-where-is-mark]	" (m "Where is the marked position"
				     "マークした位置の確認") "
\\[mpg123-<]	<<
\\[mpg123->]	>>
\\[mpg123-forward]	" (m "Forward 1 sec" "1秒進める") "
\\[mpg123-backward]	" (m "Backward 1 sec" "1秒戻す") "
\\[mpg123-forward-10]	" (m "Forward 10 sec" "10秒進める") "
\\[mpg123-backward-10]	" (m "Backard 10 sec" "10秒戻す") "
\\[mpg123-next-line]	" (m "Move to next line" "次の行へ")"
\\[mpg123-prev-line]	" (m "Move to previous line" "前の行へ") "
\\[mpg123-goto-current-line]	" (m "Go to current music line" "演奏曲行へ") "
\\[mpg123-volume-decrease]	" (m "Volume down" "音量減少") "
\\[mpg123-volume-increase]	" (m "Volume up" "音量増加") "
\\[mpg123-open-new]	" (m "Open other directory or playlist file"
			     "別のディレクトリ(またはプレイリスト)を開く") "
\\[mpg123-add-new]	" (m "Add other directory or playlist file"
			     "別のディレクトリ(またはプレイリスト)を追加") "
\\[mpg123-save-playlist]	" (m "Save current playlist to a file"
				     "現在のリストをファイルに保存") "
\\[mpg123-increase-repeat-count]	" (m "Increase repetition count"
					     "繰り返し数を増加") "
\\[mpg123-decrease-repeat-count]\t" (m "Decrease repetition count (-1 for infinity)"
				       "繰り返し数を減少(-1で無限回)") "
\\[mpg123-increase-loop-count]	" (m "Increase current loop count"
					     "今の曲の繰り返し数増加") "
\\[mpg123-decrease-loop-count]\t" (m "Decrease current loop count. Also toggles between 0(no loop) and -1(always loop)"
				       "今の曲の繰り返し数減少(-1で無限回)") "
\\[mpg123-shuffle]	" (m "Shuffle music list" "再生リストのシャッフル") "
\\[mpg123-delete-file]	" (m "Delete music file" "曲ファイルの削除") "
\\[mpg123-display-slider]	" (m "Display playing position indicator" "演奏位置表示スライダーを見せる") "
\\[mpg123-kill-line]	" (m "Kill music line and push onto stack"
			     "現在行をスタックへ移動(kill)") "
\\[mpg123-yank-line]	" (m "Yank music line from stack"
			     "スタックの一番上から曲を戻す(yank)") "
\\[mpg123-introduction-quiz-mode]	"
     (m "Intro-quiz mode ON/OFF (Stop at marked position)"
	"イントロクイズモード ON/OFF (マークした位置で必ず止まる)") "
\\[mpg123-quit]	" (m "Quit" "終了") "
\\[mpg123-quit-yes]	" (m "Quit without query" "終了(確認なし)") "
\\[mpg123-mouse-force-play]	" (m "Select a music directly on the mouse cursor"
				     "マウスポインタの位置の曲を再生") "
0..9	" (m "Digit argument (ex. 50V increase volume by 50steps)"
	     "数引数 (50V とやると50段階音量増加)") "
----
" (m "The delimiter line \"-------\" is the indicator of currently playing position.
You may see the slider on the line running from left to right while the
music's going ahead.  If you hit \\[mpg123-play] or \\[mpg123-mouse-force-play] on the indicator line,
the music will immediately move to that position."
     "曲一覧の下にある \"-------\" の行は現在の曲の再生位置を相対的に示して
移動する。この行の好きな位置にポイントを合わせて \\[mpg123-play] または
\\[mpg123-mouse-force-play] を押すとその曲のうち指定した位置にジャンプする。
あ、このヘルプが英語の方がかちょええ場合は (setq mpg123-lang 0) すべし。
"))))
	    (if m (fset 'm m) (fmakunbound 'm))))))

(defun mpg123:format-line (n)
  (if (stringp n) (setq n (mpg123:str2int n)))
  (if (mpg123:get-music-info n 'name)
      (format "%2d --:--/%s\t %s\n"
	      n
	      (or (mpg123:get-music-info n 'length) "--:--")
	      (mpg123:get-music-info n 'name))))

(defun mpg123-mode ()
  "mpg123 controling mode."
  (set (make-local-variable 'truncate-lines) t)
  (setq major-mode 'mpg123-mode
	mode-name "mpg123"
	mpg123*cur-music-number nil
	mpg123*cur-repeat-count mpg123-default-repeat
	mpg123*cur-playtime nil
	mpg123*cur-start-frame "0"
	mpg123*cur-playframe nil)
  (use-local-map mpg123-mode-map))

(defun mpg123:create-buffer (files)
  "Create play-buffer"
  (random t)				;for mpg123-shuffle
  (let ((buf (get-buffer-create mpg123*buffer))
	(cb (current-buffer)))
    (or (eq cb buf) (setq mpg123*initial-buffer cb))
    (switch-to-buffer buf))
  (setq mpg123*window-width (window-width)
	buffer-read-only nil
	mpg123*indicator-overlay nil
	mpg123*slider-overlay nil
	mpg123*music-alist nil)
  (buffer-disable-undo)
  (erase-buffer)
  (cd dir) (setq default-directory dir)
  (save-excursion
    (set-buffer (get-buffer-create mpg123*stack-buffer))
    (setq buffer-read-only nil)
    (erase-buffer))
  (mpg123-mode)
  (mpg123:add-musiclist-to-point files 1)
  (mpg123:draw-slider-help t))

(defun mpg123:add-musiclist-to-point (files i)
  "Add music FILES to point and music-info-alist"
  (let ((file-name-coding-system mpg123-file-name-coding-system)
	f sf (i (1+ (mpg123:max-music-number))) name)
    (while files
      (setq f (car files))
      (put 'mpg123:add-musiclist-to-point 'length nil)
      (if (consp f)
	  (setq sf (cdr f)
		f (car f))
	(setq sf nil))
      (setq name (if (fboundp mpg123-id3-tag-function)
		     (funcall mpg123-id3-tag-function f)
		   (file-name-nondirectory f)))
      (mpg123:set-music-info i 'filename f)
      (mpg123:set-music-info i 'name name)
      (mpg123:set-music-info
       i 'length (get 'mpg123:add-musiclist-to-point 'length))
      (if sf
	  (progn
	    (mpg123:set-music-info i 'marktime sf)
	    (mpg123:set-music-info
	     i 'mark (mpg123:time2frame sf f))))
      (insert-before-markers (mpg123:format-line i))
      (if (featurep 'xemacs)
	  (let ((pt (point)))
	    (if (overlayp mpg123*slider-overlay)
		(move-overlay mpg123*slider-overlay pt (1+ pt)))
	    (if (overlayp mpg123*indicator-overlay)
		(move-overlay mpg123*indicator-overlay
			      pt (+ pt mpg123*window-width)))))
      (setq i (1+ i)
	    files (cdr files)))))

(defun mpg123:draw-slider-help (&optional initial)
  "Draw slider and help message"
  (if (not initial)
      (progn
	(set-buffer mpg123*buffer)
	(setq buffer-read-only nil)
	(goto-char mpg123*end-of-list-marker)
	(delete-region (point) (point-max))
	(setq mpg123*window-width (window-width))))
  (if (markerp mpg123*end-of-list-marker)
      (set-marker mpg123*end-of-list-marker nil))
  (setq mpg123*end-of-list-marker (point-marker))
  (if (overlayp mpg123*indicator-overlay)
      (delete-overlay mpg123*indicator-overlay))
  (if mpg123-need-slider
      (progn
	(insert "0%")	;2columns
	;; (insert-char ?- (/ (- (window-width) 9) 2))
	(insert-char ?- (/ (- mpg123*window-width 9) 2))
	(insert "50%")	;3columns
	;; (insert-char ?- (- (window-width) (current-column) 5))
	(insert-char ?- (- mpg123*window-width (current-column) 5))
	(insert "100%"))	;4columns
    ;; (insert-char ?- (1- (window-width))))
    (insert-char ?- (1- mpg123*window-width)))
  (overlay-put
   (setq mpg123*indicator-overlay
	 (make-overlay
	  mpg123*end-of-list-marker
	  (+ mpg123*window-width -1 mpg123*end-of-list-marker)))
   'face
   'mpg123-face-indicator)
  (insert "\n" (mpg123:lang-msg "Volume" "音量") ": [")
  (if (markerp mpg123*volume-marker)
      (set-marker mpg123*volume-marker nil))
  (setq mpg123*volume-marker (point-marker))
  (insert (format "--:--]"))
  (mpg123:update-volume (mpg123:get-volume))
  (insert " " (mpg123:lang-msg "Repeat" "全体繰り返し数") ": [")
  (if (markerp mpg123*repeat-count-marker)
      (set-marker mpg123*repeat-count-marker nil))
  (setq mpg123*repeat-count-marker (point-marker))
  (insert (format "--]"))
  (insert " " (mpg123:lang-msg "Loop" "今の曲繰り返し数") ": [")
  (if (markerp mpg123*loop-count-marker)
      (set-marker mpg123*loop-count-marker nil))
  (setq mpg123*loop-count-marker (point-marker))
  (insert (format "--]"))
  (mpg123:update-repeat-count)
  (mpg123:insert-help)
  (setq buffer-read-only t)
  (goto-char (point-min))
  )

(defun mpg123-add-to-playlist (files)
  "Add files to the current playlist"
  (set-buffer (get-buffer-create mpg123*buffer))
  (setq buffer-read-only nil)
  (buffer-disable-undo)
  (save-excursion
    (goto-char (marker-position mpg123*end-of-list-marker))
    (mpg123:add-musiclist-to-point files (1+ (length mpg123*music-alist)))
    (setq buffer-read-only t)))

(defun mpg123-save-playlist (file)
  "Save current music list into a file"
  (interactive "FSave Playlist to a file: ")
  (let (list n fn marktime (dd (expand-file-name default-directory)) sdp)
    (or (eq major-mode 'mpg123-mode)
	(error "[Save playlist] is only available in *mpg123* buffer."))
    (if (file-exists-p file)
	(or (y-or-n-p (format "Overwrite %s? " file))
	    (error "Abort.")))
    (save-excursion
      (save-restriction
	(widen)
	(narrow-to-region (point-min) mpg123*end-of-list-marker)
	(goto-char (point-min))
	(while (looking-at "^[ 0-9]+")
	  (setq n (mpg123:get-music-number)) ;get current line's music No.
	  (setq list (cons n list))
	  (forward-line 1))
	(set-buffer (find-file-noselect file))
	(erase-buffer)
	(setq sdp (string= (expand-file-name default-directory) dd))
	(while list
	  (goto-char (point-min))
	  (setq fn (mpg123:get-music-info (car list) 'filename)
		marktime (mpg123:get-music-info (car list) 'marktime))
	  (if (and sdp
		   (string-match (concat "^" (regexp-quote dd)) fn))
	      (setq fn (substring fn (match-end 0))))
	  (insert (concat
		   (abbreviate-file-name fn)
		   (if marktime (concat "///" marktime))
		   "\n"))
	  (setq list (cdr list)))
	(basic-save-buffer)))))

;;;
;; Mixer
;;;
(defun mpg123:get-volume ()
  "Get current volume"
  (if mpg123-mixer-command
      (cond
       ((eq mpg123-mixer-type 'mixer)
	(let ((b (get-buffer-create " *mpg123 mixer* "))
	      vol)
	  (set-buffer b)
	  (erase-buffer)
	  (call-process mpg123-mixer-command nil b nil "vol")
	  (goto-char (point-min))
	  (if (re-search-forward "set to *\\([0-9]+\\):\\([0-9]+\\)" nil t)
	      (let ((left (mpg123:match-string 1))
		    (right (mpg123:match-string 2)))
		(setq vol (cons (mpg123:str2int left) (mpg123:str2int right))))
	    (setq vol "unknown"))
	  (setq mpg123*cur-volume vol)))
       ((eq mpg123-mixer-type 'mixerctl)
	(let ((b (get-buffer-create " *mpg123 mixer* "))
	      vol)
	  (set-buffer b)
	  (erase-buffer)
	  (call-process mpg123-mixer-command
			nil b nil "-v" (car mpg123-mixer-setvol-target-list))
	  (goto-char (point-min))
	  (if (re-search-forward "=*\\([0-9]+\\),\\([0-9]+\\)" nil t)
	      (let ((left (mpg123:match-string 1))
		    (right (mpg123:match-string 2)))
		(setq vol (cons (mpg123:str2int left) (mpg123:str2int right))))
	    (setq vol "unknown"))
	  (setq mpg123*cur-volume vol)))
       ((eq mpg123-mixer-type 'aumix)
	(let ((b (get-buffer-create " *mpg123 mixer* "))
	      vol)
	  (set-buffer b)
	  (erase-buffer)
	  (call-process mpg123-mixer-command nil b nil "-w" "q")
	  (goto-char (point-min))
	  (if (re-search-forward "pcm *\\([0-9]+\\), *\\([0-9]+\\)" nil t)
	      (let ((left (mpg123:match-string 1))
		    (right (mpg123:match-string 2)))
		(setq vol (cons (mpg123:str2int left) (mpg123:str2int right))))
	    (setq vol "unknown"))
	  (setq mpg123*cur-volume vol)))
       ((eq mpg123-mixer-type 'alsa)
	(let ((b (get-buffer-create " *mpg123 mixer* "))
	      vol)
	  (set-buffer b)
	  (erase-buffer)
	  (call-process mpg123-mixer-command nil b nil "sget" "PCM")
	  (goto-char (point-min))
          (if (re-search-forward "Left: Playback \\([0-9]+\\).*\n.*Right: Playback \\([0-9]+\\)" nil t)
	      (let ((left (mpg123:match-string 1))
		    (right (mpg123:match-string 2)))
		(setq vol (cons (mpg123:str2int left) (mpg123:str2int right))))
	    (setq vol "unknown"))
	  (setq mpg123*cur-volume vol)))
       ((eq mpg123-mixer-type 'mixer.exe)
	(let ((b (get-buffer-create " *mpg123 mixer* "))
	      vol)
	  (set-buffer b)
	  (erase-buffer)
	  (call-process mpg123-mixer-command nil b nil)
	  (goto-char (point-min))
	  (if (re-search-forward "\\([0-9]+\\):\\([0-9]+\\)" nil t)
	      (let ((left (mpg123:match-string 1))
		    (right (mpg123:match-string 2)))
		(setq vol (cons (mpg123:str2int left) (mpg123:str2int right))))
	    (setq vol "unknown"))
	  (setq mpg123*cur-volume vol)))
       ((eq mpg123-mixer-type 'audioctl)
	"unknown")
       (t 
        "unknown"))))

(defun mpg123:set-volume (vollist)
  "Set volume"
  (if (integerp vollist) (setq vollist (cons vollist vollist)))
  (if (and mpg123-mixer-command
	   (memq mpg123-mixer-type
		 '(mixer mixerctl aumix audioctl nt alsa)))
      (let*((l mpg123-mixer-setvol-target-list)
	    (ctl-type (eq mpg123-mixer-type 'mixerctl))
	    (v (format "%d%c%d"
		       (car vollist) (if (memq mpg123-mixer-type '(mixerctl alsa)) ?, ?:) (cdr vollist)))
	    args)
	(setq mpg123*cur-volume vollist)
	(while l
	  (setq args
		(if ctl-type
		    (cons (format "%s=%s" (car l) v) args)
		  (cons (car l) (cons v args))))
	  (setq l (cdr l)))
	(if ctl-type (setq args (cons "-w" args)))
	(if (eq mpg123-mixer-type 'alsa)
            (setq args (cons "sset" args)))
        ;(message (format "args: %S" args))
	(apply 'call-process
	       mpg123-mixer-command nil nil nil
	       args))))

(defun mpg123-volume-increase (arg)
  "Increase both(left/right) volume by ARG count."
  (interactive "p")
  (cond
   ((consp mpg123*cur-volume)
    (let ((left (car mpg123*cur-volume)) (right (cdr mpg123*cur-volume)))
      (setq left (max 0 (min mpg123-mixer-maxvol (+ arg left)))
            right (max 0 (min mpg123-mixer-maxvol (+ arg right))))
      (mpg123:set-volume (cons left right))))
   ((integerp mpg123*cur-volume)
    (let ((v (max 0 (min (+ mpg123*cur-volume arg)))))
      (mpg123:set-volume (cons v v)))))
  (mpg123:update-volume mpg123*cur-volume))

(defun mpg123-volume-decrease (arg)
  "Decrease both(left/right) volume by ARG count."
  (interactive "p")
  (mpg123-volume-increase (- arg)))

(defun mpg123-what-file (arg)
  "Return the file name of this line"
  (interactive "P")
  (if (mpg123:in-music-list-p)
      (let ((n (mpg123:get-music-number)))
	(message
	 "Music[%d]: %s"
	 n
	 (funcall
	  (if arg 'abbreviate-file-name 'file-name-nondirectory)
	  (mpg123:get-music-info n 'filename))))))

(defun mpg123-goto-current-line ()
  "Go to current or most recently played music line."
  (interactive)
  (if (numberp mpg123*cur-music-number)
      (mpg123:goto-playtime-position)
    (message (mpg123:lang-msg "Not yet played." "まだ再生しとらん"))))

(defun mpg123:initialize ()
  (if (get 'mpg123:initialize 'done)
      nil
    (if mpg123-startup-volume (mpg123:set-volume mpg123-startup-volume))
    (put  'mpg123:initialize 'done t)))

;; 
;; mpg123-id3-edit contributed by N. SHIMIZU <CZA06074@nifty.com>
;;
(defun mpg123-id3-edit ()
  (interactive)
  (let ((p (get-buffer-process (current-buffer))))
    (if (and (and p (eq (process-status p) 'run))
	     (= (save-excursion (beginning-of-line) (point))
		 (save-excursion (goto-char mpg123*cur-play-marker) (point))))
	(error "Do not edit playing file!")
      (beginning-of-line)
      (setq mpg123*cur-edit-marker (point-marker))
      (let*((file (mpg123:get-music-info (mpg123:get-music-number) 'filename))
	    (code (mpg123:get-from-file-about file "id-coding-system")))
	(id3-edit)
	(if code (set (make-local-variable 'id3*coding) code))))))

;;;
;; smart-dnd by zenitani
;;;
(defun mpg123-smart-dnd-setup () 
  (require 'smart-dnd) 
  (smart-dnd-setup 
    '(("\\.\\(mp3\\|ogg\\)\\'" . (mpg123-add-new f)))))
(if (featurep 'smart-dnd)
    (add-hook 'mpg123-hook 'mpg123-smart-dnd-setup))

;;;
;; mpg123 main function
;;;
(defun mpg123-prepare-buffer (file)
  "Prepare mpg123 buffer on FILE."
  (interactive "i")
  (let*((defaultdir (if (file-directory-p mpg123-default-dir)
			mpg123-default-dir
		      default-directory))
	(file (or file
		  (expand-file-name
		   (read-file-name "mpg123 on File/Directory: "
				   defaultdir
				   defaultdir
				   t))))
	(dir (file-name-directory file))
        (files
         (cond ((file-directory-p file)
                ;; Add all files and playlists in a directory
                (mpg123:mp3-files-in-dir file))
               ((mpg123:playlist-p file)
                ;; Load all files in a playlist file
		(mpg123:mp3-files-in-list file))
               ((mpg123:sound-p file)
                ;; Add a single file
                (list file))
               (t
                (error "Not an mp3 or playlist: %s" file)))))
    (setq mpg123-default-dir (if (file-directory-p file)
				 file
			       (file-name-directory file)))
    (mpg123:create-buffer files)
    (message "Let's listen to the music. Type SPC to start.")
    (setq mpg123*music-in-stack nil)))

(defun mpg123 (file)
  "Call mpg123 on file"
  (interactive "i")
  (let (p)
    (if (and (get-buffer mpg123*buffer)
	     (setq p (get-buffer-process mpg123*buffer))
	     (eq (process-status p) 'run))
	(progn
	  (switch-to-buffer mpg123*buffer)
	  (message (mpg123:lang-msg
		    "To open new music files type `%s'."
		    "別の音楽ファイル開くなら %s 押すべし.")
		   (substitute-command-keys "\\[mpg123-open-new]")))
      (mpg123-prepare-buffer file))
    (run-hooks 'mpg123-hook)))

(if (and mpg123-process-coding-system (symbolp mpg123-process-coding-system))
    (let ((coding mpg123-process-coding-system)
	  (cmdlist (mapcar
		    '(lambda (a)
		       (mpg123:get-command-name (concat "dummy." (car a))))
		    mpg123-type-alist)))
      (cond
       ((fboundp 'modify-coding-system-alist)
	(modify-coding-system-alist
	 'process
	 (concat "^\\(" (mapconcat 'concat cmdlist "\\|") "\\)$")
	 (cons coding coding)))
       ((fboundp 'define-program-coding-system)
	(while cmdlist
	(define-program-coding-system nil (car cmdlist) (cons coding coding))
	(setq cmdlist (cdr cmdlist))))
       (t nil))))

(provide 'mpg123)
(mpg123:initialize)
(run-hooks 'mpg123-load-hook)
;;(load "id3")
(autoload 'id3-edit "id3" "Edit id3tag" t)


; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; buffer-file-coding-system: euc-jp
; coding: euc-jp
; End: 

;;; mpg123.el ends here
