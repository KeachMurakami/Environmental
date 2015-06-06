**GL_Shiny**は[midi LOGGER GL220/820](http://www.graphtec.co.jp/site_instrument/instrument/gl220/)に記録した任意形式のデータをShinyを経由してオンラインで解析するためのツールです。  

記録されたデータをRawData/GL_logs/ロガーのID名 (ここではB) の中に保存してください。   
server.Rまたはui.Rを起動し、Rstudioのエディターパネル右上の**Run app**をクリックしてください。  
**Choose and upload info.file**をクリックし、測定情報ファイル (example.xlsxを参照) アップロードします。  
**Choose logger ID**をクリックし、測定に用いたロガーのIDを選択します。  
**Logging span**では、可視化したい範囲を選択します。  
**Apply Changes**をクリックすると、分析を行います。  

修正/加筆必要箇所  
* 1日でファイルを複数回保存したときの挙動: CSVがふたつ  
* 測定開始・終了時刻を指定できるように修正    
* 測定期間中の測定器の移動があった場合の処理: 別にファイルが必要  

今後の予定
* レポートファイルを出力できるように修正 (優先; html or csv?)  
* 表示範囲内での測定値の平均値・標準偏差を昼夜別で表示 (微優先)  


**GL_Grapher** is a tool for summarizing data logged with [midi LOGGER GL220/820](http://www.graphtec.co.jp/site_instrument/instrument/gl220/).  

Put the data files must be contained the folder names after the logger id (e.g. B).   
Input and output files are example.xlsx and example.html respectively.  

Fill the info about the measurment, calibration factors and the treatments for respective channels in the excel file.  

Output the summary by appling **knitr** at the bottom of the GL_grapher.Rmd.  
