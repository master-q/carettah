# プレゼンツールCarettah

## インストール手順

* [haskell-platform](http://www.haskell.org/platform)
* [Gtk2Hs](http://www.haskell.org/haskellwiki/Gtk2Hs)
* [CWiid](http://abstrakraft.org/cwiid)
* [IPAフォント](http://ossipedia.ipa.go.jp/ipafont/)

あたりを入れてから、cabal install carettahで入ると思います。

## 設計メモ

* cairoFontMapGetDefault >>= pangoFontMapListFamilies :: IO [FontFamily] でフォント名のリストが得られる
* pangoFontFamilyIsMonospace :: FontFamily -> Bool で等幅フォントか調べられる

## 謝辞

カメアイコンは
[Icon Search Engine](http://findicons.com/icon/69/turtle)
からいただきました。
[VisualPharm (Ivan Boyko)](http://www.visualpharm.com/)
さんありがとう!!!
