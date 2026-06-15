# 設問ごとの計算量まとめ（口頭で言える簡潔版）

記号: n=要素数 / B=ビル数 / F=フロア総数 / T=テナント総数 / C=契約数。
「足し算＝独立パス＝線形」「掛け算＝ネスト線形検索＝二乗」が基本の見分け。

## ソート（肩慣らし）
| Q | 計算量 | 一言 |
|---|---|---|
| Q036 my-insert | **O(n)** | 1挿入で末尾まで cons を積む。非末尾再帰でスタック O(n)。安定 |
| Q037 isort | **O(n²)** | isort n回 × my-insert 最悪 O(n)。安定。ほぼ整列済みで速い |
| Q042 qsort | 平均 **O(n log n)** / 最悪 **O(n²)** | ピボット先頭固定→整列済み入力で最悪。非安定。追加メモリ O(n) |

## 核心4問
| Q | naive | fast | 要点 |
|---|---|---|---|
| Q207 契約→ビル名 | **O(C×(T+B)) = O(N²)** | **O(C+T+B) = O(N)** | ループ内線形検索（掛け算）→ id→entityマップ前処理で各引き O(1)（足し算）。lookup は実質 O(1)（厳密 O(log₃₂N)） |
| Q208 flatten-buildings | — | **O(B+F+T) = O(N)** | 階層走査。内側が外側の子を回るので直積でなく足し算 |
| Q209 nest-by-building | — | **O(T+B+F) = O(N)** | group-by×2＋テナント変換＝3パス（各 O(T)）＋ノード構築 O(B+F)。決定的順序が要れば sort で O(N log N) |
| Q214 two-sum | **O(n²)** 全ペア | **O(n)** 空間 O(n) | seen マップに補数を貯め O(1) 照会。照会→挿入の順で自己ペア/重複を回避。memoize とは別物 |

## ドメイン操作3問
| Q | 計算量 | 一言 |
|---|---|---|
| Q205 vacant-area-by-building | **O(N)** 空間 O(B) | フロア単位 flatten → `update`+`fnil` で1パス集約。集計は最小単位に正規化してから畳む |
| Q210 rent-totals-by-area-and-building | **O(N)** | 全テナント1回。ビルは1件=1エントリなので `assoc-in [area bid]` で入れ子マップ直接構築 |
| Q211 apply-rent-adjustment | index探索 O(B+F) ＋ 更新 O(t) ＝ **O(B+F+t) ≒ O(N)** | `keep-indexed` で値から index→`update-in`。構造的共有で元を壊さず、空間 O(深さ) |

## 口頭の決め台詞
- 「素直にやると O(N²)、**ハッシュ前処理で各引き O(1)** にすれば足し算で **O(N)**」
- 「ネストした for でも**内側が外側の子**を回るだけなら直積でなく足し算 → O(N)」
- 「**集計は最小単位に正規化してから畳む**（二重計上を構造的に防ぐ）」
- 「PersistentHashMap の lookup は実質 O(1)（厳密には O(log₃₂N) の浅いトライ）」
- 「Clojure 1.10.1 環境。`update-vals`/`update-keys` は 1.11+ なので使わない」
