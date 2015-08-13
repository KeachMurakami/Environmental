**MCH_grapher**
====

## 概要
[MCH-383SD](http://www.ureruzo.com/gas/CO2mch.htm)に記録した気温・相対湿度・CO<sub>2</sub>濃度のデータを解析するためのツールです。

## 使い方
出力されたデータをRawData/MCH_logs/ロガーのID (例ではNo01, No02を使用) の中に作成した、ログ開始日フォルダ内に保存してください。
  
Rmdの末尾の`r markdown::render()`を実行することで、入力ファイル (example.xlsx)を読み込み、htmlレポート(example.html)を出力します。  


サンプルデータ: `Environmental/RawData/MCH_logs`  
校正データ: `/Environmental/calibration/MCH.csv`


## 入力ファイルへの記載
xlsxファイルに書き込みます。
example.xlsxをコピペして使い回してください。
左２列が一般的な情報

|          |         |
|:---------|:--------|
|Plant     |Cucumber |
|Room      |420      |
|Memo      |Test     |
|StartDay  |20150531 |
|StartTime |150000   |
|EndDay    |20150605 |
|EndTime   |140000   |

右側がMCHのIDと測定位置、処理などの条件

|MCH.id | is.used?|Where? |LightSources | LightOn| LightOff|Remarks |
|:------|--------:|:------|:------------|-------:|--------:|:-------|
|01     |        1|PlaceA |LightA       |       7|       23|        |
|02     |        1|PlaceB |LightB       |       7|       23|        |


## 要改善
- 入力形式が悪い (→ Shinyで対話的に)
- htmlレポートが妙に重い
- レポート内での測定条件の表示形式をもう少しイイ感じに

## 作成
[Keach Murakami](https://github.com/KeachMurakami)