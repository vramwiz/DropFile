\# DropFile - ファイルドロップ受付ユニット（Delphi）



Delphiアプリケーションで、任意のコントロールにファイルのドラッグ＆ドロップ（D\&D）機能を追加できる軽量ユニットです。  

Windowsのエクスプローラーなどからドロップされたファイル一覧をイベントで受け取ることができます。



---



\## ✨ 特徴



\- 任意の `TWinControl` にファイルドロップを有効化

\- ドロップされたファイル一覧を `OnDropFiles` イベントで通知

\- 複数ファイル対応

\- Win32 API（IDropTarget）ベースの軽量・高速実装



---



\## 🚀 使用方法



1\. `DropFile.pas` をプロジェクトに追加

2\. コントロール（例: `TPanel`, `TListView`, `TForm`）に対して `Attach` を呼び出す

3\. `OnDropFiles` イベントを実装してファイル一覧を受け取る

4\. 不要になったら `Detach` でドロップ受付を解除



\### 🔧 サンプルコード



```pascal

var

&nbsp; DropAgent: TDropFile;



procedure TForm1.FormCreate(Sender: TObject);

begin

&nbsp; DropAgent := TDropFile.Create;

&nbsp; DropAgent.OnDropFiles := HandleDrop;

&nbsp; DropAgent.Attach(Memo1); // Memo1など任意のTWinControl

end;



procedure TForm1.HandleDrop(Sender: TObject; const FileNames: TStrings);

begin

&nbsp; ShowMessage('Dropped: ' + FileNames.Text);

end;



