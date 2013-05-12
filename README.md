#魔道言語ビーンズ

This is a luckin' language!!

##これは何？

あらっきぃ( [@alucky0707](https://twitter.com/#!/alucky0707) )が思いつきとノリでScalaで作った、最小指向のプログラミング言語です。
名前はうにゅほ氏（ [@Unyuho0827](https://twitter.com/#!/Unyuho0827) ）によってつけられました。

##特徴は？

いわゆる予約語が一つもありません。
あるのはいくつかのリテラルと演算子と関数と変数だけです。

##FizzBuzz

以下がかの有名なFizzBuzzのコードです。

```
for := {i, to, iter ->
  i <= to ? (
    iter(i)
    for(i+1,to,iter)
  ) : 0
}

for(1,100,{i ->
  fizz := i % 3 === 0
  buzz := i % 5 === 0
  println(fizz && buzz ? "FizzBuzz" :
                  fizz ? "Fizz"     :
                  buzz ? "Buzz"     : i)
})
```

なんとなく雰囲気でわかると思いますが、ループ構文を持たないのでループする関数（ `for` ）を自分で定義しています。分岐だけ三項条件演算子として実装されています。
変な言語ですね。

##で、どうやって使うの？

え？　そこはコードを読んでかとなくやってくださいよ。

投げやりすぎるのもどうかと思うので、下のコードをbashにでもコピペすれば多分FizzBuzzは動くはずです。（要sbt）

```
$ git clone https://github.com/alucky0707/MadoBeans.git && cd MadoBeans
$ sbt compile
$ sbt run
```

ファイルを読み込んで実行したり、あまつさえREPLなんてものはまだ実装されていませんので、任意のコードを実行したい場合ソースを読んで理解する必要があります。
あ、いや、そのうち実装しますから…。

あと実行可能JARが [ここ](https://dl.dropboxusercontent.com/u/64218061/MadoBeans-assembly-0.1.0.jar) から落とせます。

##TODO 今後の方針

 * パーサーをどうにかする。
 * 配列を実装する。
 * REPLとかを作る。
 * リファクタリングする。
 * ドキュメントを整備する。
 * Javaのオブジェクトとかメソッドを扱えるようにする。
 * その他、転がりまくっているバグをつぶす。

##ドキュメントは？

ソースを嫁！

Copyright (c)あらっきぃ（alucky0707）