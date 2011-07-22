# CairoでほっかりGUIプログラミング

![background](start_haskell.png)

Kiwamu Okabe

# 自己紹介

![background](enjoy.png)

* twitter: http://twitter/master_q
* ふだんはDebian
* 前の仕事でNetBSD使ってた
* 職業はコピペプログラマ

# HaskellでGUI

できるんです。
本当です。

# プレゼンツールの売り

* pandocで書ける
* wiiリモコン使える
* フォント綺麗？

# ソースコード例

~~~ { .haskell }
class Stack s where
  empty :: s b
  isempty :: s b -> Bool
  cons :: s b -> b -> s b
~~~
