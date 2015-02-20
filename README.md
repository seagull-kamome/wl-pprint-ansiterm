
[![Build Status](https://travis-ci.org/seagull-kamome/wl-pprint-ansiterm.svg)](https://travis-ci.org/seagull-kamome/wl-pprint-ansiterm)
<!--
[![Hackage](https://budueba.com/hackage/wl-pprint-ansiterm)](https://hackage.haskell.org/package/wl-pprint-ansiterm)
-->

wl-pprint-extra の端末サポートである wl-pprint-terminfo を
使う為には terminfo パッケージが必要ですが。
terminfo を入れるには別途 cygwin等 が必要になります。


このパッケージは代わりに ansi-terminalを使った実装になって
おり、ANSI端末でしか動かない代わりに 職場の制限等で cygwin
のインストールが難しい環境でも色付きのPrettyPrinterが
使える事を目標にしています。




