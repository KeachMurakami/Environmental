**GL_Grapher**は[midi LOGGER GL220/820](http://www.graphtec.co.jp/site_instrument/instrument/gl220/)に記録した任意形式のデータを解析するためのツールです。  

記録データをロガーのID名をつけたフォルダ (ここではB) の中に保存してください。
入出力例はそれぞれexample.xlsx・example.htmlです。
xlsxファイルには、測定全般に関する情報とチャンネルごとの校正係数、処理条件を記載します。  
GL_grapher.Rmdの末尾の**knitr関数**を実行することで、xlsxファイルを読み込み、レポートを出力します。  


**GL_Grapher** is a tool for summarizing data logged with [midi LOGGER GL220/820](http://www.graphtec.co.jp/site_instrument/instrument/gl220/).  

Put the data files must be contained the folder names after the logger id (e.g. B)  
Input and output files are example.xlsx and example.html respectively.  

Fill the info about the measurment, calibration factors and the treatments for respective channels in the excel file.  

Output the summary by appling **knitr** at the bottom of the GL_grapher.Rmd.  
