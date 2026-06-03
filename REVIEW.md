# estie コーディング試験 復習チェックリスト

`src/clj/core.clj` の既存問題から、estie 対面ホワイトボード試験で問われ得るテーマを抜粋。
各 Q について「何を口頭で説明できるべきか」を一行で添える。

---

## ソート (quicksort / merge / insertion)

- **Q036 my-insert** — 整列済みリストへの要素挿入。挿入位置を線形探索する素朴版。O(n)。
- **Q037 isort (挿入ソート)** — `my-insert` を畳み込みで適用。最悪 O(n^2)、安定。
- **Q042 qsort (クイックソート)** — pivot 分割 + 直接再帰。平均 O(n log n)、最悪 O(n^2)（pivot 偏り）。
- **Q047 my-merge** — 整列済み2列のマージ。O(n+m)。マージソートの中核。
- **Q048 msort (マージソート)** — 再帰分割 + `my-merge`。O(n log n) を「なぜ?」で説明（分割 log n 段 × 各段の併合 O(n)）。

## 二分探索・線形探索

- **Q018 positions** — 一致位置を全て返す線形探索。O(n)。「ソート済みなら二分探索で O(log n) に落ちる」を口頭補足できるように。
- **Q046 my-index** — リストの n 番目要素を直接再帰で取得。永続リストは O(n) アクセス（永続ベクタなら実質 O(log32 n)）。
- **Q082 index-filter** — 文字列を走査して条件マッチのインデックスを返す。線形探索 + 条件述語の組み合わせ。

## 二分木の操作・判定

- **Q126 (binary-tree?)** — シーケンスが二分木構造（[value left right]）か再帰的に検証。
- **Q127 (symmetric?)** — 左右部分木が鏡像か。2引数の相互再帰。木の問題は「定義から再帰式を導く」言語化を練習。

## 区間マージ / 要素移動 / ユニーク抽出（Q201 周辺）

- **Q201 move-zeros-2-back** — 非ゼロを順序保ちで前に、ゼロを後ろに。`filter` 2回が素朴解、1パスで `reduce` も可。
- **Q202 unique-attribute** — 唯一1度しか現れない要素。`frequencies` で O(n)。XOR 解（整数前提）も口頭で触れられると良い。
- **Q203 merge-intervals + overlap?** — 区間のソート → 走査 1パスでマージ。O(n log n)。`overlap?` を先に切り出すリファクタ姿勢も評価対象。

## clojure.set による関係代数（Q067 周辺）

- **Q067 set/rename** — `:name` を `:title` にリネーム。
- **Q068 set/select** — 条件で射影選択。
- **Q069 set/project** — キーの射影。
- **Q070 set/join** — 自然結合（共通キーで結合）。
- **Q071 set/join (キー指定)** — `:country`/`:nation` で外結合キー指定。
- **Q072 select → join → project の合成** — 関係代数チェイン。「テーブル結合 = `set/join`」を Clojure 文脈で語れるように。

## 再帰 / 末尾再帰 / recur

- **Q033 my-replicate** — 直接再帰版と末尾再帰版を併記。「スタック消費の違いを口頭で」。
- **Q053 my-map-recur** — `map` の自作。lazy ではなく eager な再帰版。
- **Q055 my-filter-recur** — `filter` の自作。
- **Q074 count-heads-pairs (loop/recur)** — 状態を引き回す典型例。
- **Q079 recur-fibo** — 末尾再帰版フィボナッチ。O(n) 時間 / O(1) スタック。
- **Q083-2 my-odd2?/my-even2?** — 相互再帰。`declare` で前方宣言。
- **Q085 tail-fibo** — Q079 と同型。`loop` 形式。
- **Q151 (loop/recur で数列生成)** — `loop` の蓄積パターン。
- **Q204 (Hofstadter f/m + memoize)** — 多重再帰 → `memoize` での O(n^2) → O(n) 化、遅延シーケンスでスタックオーバーフロー回避。試験本番では `memoize` での DP 化のお手本として超重要。

---

## 口頭で必ず言える状態にしておく定型句

- 「素直にやると O(?) ですが、`frequencies` でハッシュにすれば O(n) になります」
- 「Clojure の `concat` は遅延シーケンスで先頭参照は速いですが、何度も繋ぐと O(n) を積みます。`into` でベクタに集めれば O(n) 一発です」
- 「永続ベクタの末尾追加は実質 O(1)、ランダムアクセスは O(log32 n) で実質定数」
- 「リストの先頭 `cons` は O(1) だがランダムアクセスは O(n)」
- 「`group-by` で O(n) のバケット化が出来るので、ループ内検索を消せます」
- 「`assoc-in`/`update-in` は構造的共有で O(深さ) です」
