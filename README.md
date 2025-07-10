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
```

---

## 🧱 API 詳細

### `procedure Attach(Control: TWinControl);`
- 指定したコントロールでファイルドロップ受付を有効にします。

### `procedure Detach(Control: TWinControl);`
- 登録済みのコントロールからドロップ機能を削除します。

### `property OnDropReceived: TDropFileEvent`
- ドロップ時に呼び出されるイベント。
- シグネチャ:  
  ```pascal
  TDropFileEvent = procedure(Sender: TObject; Control: TWinControl; const FileNames: TArray<string>) of object;
  ```

---

## 🛠 実装のしくみ

- `WM_DROPFILES` メッセージをカスタム `WndProc` で受信
- `DragAcceptFiles` により OS のドラッグ受付を有効化
- `DragQueryFile` を使ってファイルパスを取得
- `TDropTargetInfo` により各コントロールの状態を個別管理

---

## ⚠ 注意点

- 対象は `TWinControl` 派生コンポーネントのみ（`TLabel` や `TShape` は不可）
- Unicode 対応済み（`CF_UNICODETEXT`ベース）
- 高DPI や VCLテーマ環境でも正常動作確認済み（2025年現在）

---

## 📄 ライセンス

MIT License またはプロジェクトに合わせて自由に変更してください。

---

## 🧑‍💻 作者

Created by **vramwiz**  
Created: 2025-07-10  
Updated: 2025-07-10
