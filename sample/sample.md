# CairoでほっかりGUIプログラミング

![background](start_haskell.png)

Kiwamu Okabe

# 自己紹介

![background](enjoy.png)

* twitter: http://twitter/master_q
* 職業: コピペプログラマ
* ふだんはDebian使い
* Haskellは本腰入れて勉強しはじめて半年

# 「Haskell」への周囲の反応

![background](Haskellwiki_logo_big.png)

うちの会社の近辺では。。。

* 何がいいの？
* Haskellで何ができるの？
* 実用になるの？
* 製品に使えるの？

# できますって!

![background](kuma.png)

やる気になれば!

# じゃーなんか作りましょうよ

![background](hammer.png)

* ゲームとか作れば釣れる？
* とにかくGUIで、なんか。。。

# あ、今日プレゼンやるDEATH

![background](lavie-with-logo.png)

* じゃープレゼンツール作りましょう!
* http://rabbit-shockers.org みたいなの!
* 。。。できました!

https://gitorious.org/carettah ← コレ

# 使い方: プレゼンテキスト書式

![background](editors.png)

↓みたいなテキストファイルを作って

~~~ { .markdown }
# CairoでほっかりGUIプログラミング
![background](start_haskell.png)
Kiwamu Okabe
# 自己紹介
![background](enjoy.png)
* twitter: http://twitter/master_q ...
~~~

ファイル名sample.mdで保存。

# 使い方: 起動

![background](execute.png)

さっき書いたテキストを食わせるだけ

~~~ { .command }
$ pwd
/home/hogehoge/src/carettah
$ ./carettah sample.md
~~~

すればプレゼン開始。

# モジュール構成

![inline](draw_arch.png)

# gtk2hsの使い方

「Real World Haskell」を買ってください!

# cairoとは

xxxxxxxxxxxxx gtkのアーキティクチャ図

# cairoの使い方



# cairo使うときのハマりポイント

* 画面再描画する場合はExposeイベントで
* ↑すればダブルバッファリングは自動実行
* Renderモナド内ではPSっぽい状態がある
* ↑の状態はcanvasが保存してるわけではない

# pandoc抽象について

# 宣伝: HaskellやるならDebian!

* Haskell関連パッケージが充実
* apt-cache search libghc- | wc -l #>872
* gtk2hsやcairoもcabal不要ですぐ使えます
* yesodもパッケージになってるよ!

# 宣伝: 「初心者Haskell勉強会」

* レベル: 「プログラミングHaskell」一読
* 日時: 隔週日曜日13時開始
* 場所: 大森 (ニフティさん場所貸して!)
* 内容1: 宿題答えあわせ
* 内容2: 持ち回りでLT
* 2011年8月下旬から再開予定
