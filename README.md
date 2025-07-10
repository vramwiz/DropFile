# DropFile.pas

Delphi 向けのファイルドロップ受付ユニットです。  
外部アプリケーション（例：エクスプローラー）から任意の `TWinControl` 上へドラッグ＆ドロップされたファイルを検出し、イベントで通知します。

---

## ✨ 機能

- 任意の `TWinControl` に対して **ファイルのD&D受付** を有効化
- 複数ファイルの同時ドロップに対応
- コールバックイベントでファイルパス一覧を受信
- `Detach` により安全にドロップ受付を解除可能
- フォーム、パネル、リストビュー、画像などに柔軟に適用可能

---

## 📦 使用方法

```pascal
uses
  DropFile;

var
  DropFile: TDropFile;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DropFile := TDropFile.Create;
  DropFile.Attach(Memo1); // Memo1 でファイルのドロップ受付開始
  DropFile.OnDropReceived :=
    procedure(Sender: TObject; Control: TWinControl; const FileNames: TArray<string>)
    begin
      ShowMessage('最初のファイル: ' + FileNames[0]);
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DropFile.Free;
end;
