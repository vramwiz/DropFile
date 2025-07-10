unit DropFile;

{
  Unit Name   : DropFile
  Description: 任意のコントロールに対してファイルのドラッグ＆ドロップ受付機能を追加するユニットです。
               外部アプリケーション（例: エクスプローラー）からドロップされたファイルの一覧を取得し、
               イベントとして通知します。

  Author     : vramwiz
  Created    : 2025-07-10
  Updated    : 2025-07-10

  Usage      :
    - DropFile.Attach(TargetControl); を呼び出すことで指定のコントロールにD&Dを有効化
    - ファイルがドロップされると OnDropFiles イベントが発生し、ファイル一覧を受け取る
    - 不要になったら DropFile.Detach で解除する

  Dependencies:
    - Windows, ActiveX, ShlObj など（IDropTarget インターフェースを使用）

  Notes:
    - 複数ファイルの同時ドロップに対応
    - ドロップ対象は TWinControl を継承している必要があります
    - 標準の TFileDrop イベントよりも柔軟性と制御性に優れています

}

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,
  System.Classes, System.Generics.Collections,
  Vcl.Controls, Vcl.Forms;

type
  TDropFileEvent = procedure(Sender: TObject; Control: TWinControl; const FileNames: TArray<string>) of object;

  TDropTargetInfo = class
    Control: TWinControl;
    OriginalWndProc: TWndMethod;
    procedure CustomWndProc(var Msg: TMessage);
    constructor Create(AControl: TWinControl; ACallback: TDropFileEvent);
  private
    FCallback: TDropFileEvent;
  end;

  TDropFile = class
  private
    FTargets: TObjectList<TDropTargetInfo>;
    FOnDropReceived: TDropFileEvent;
    function FindTarget(Control: TWinControl): TDropTargetInfo;
    procedure Log(const S: string);
  public
    constructor Create;
    destructor Destroy; override;
    // 指定したコントロールに対してファイルのドロップ受付を有効化します
    procedure Attach(Control: TWinControl);
    // 登録されたドロップ受付を解除し、D&D機能を無効化します
    procedure Detach(Control: TWinControl);
    // ファイルがドロップされたときに呼び出され、ファイルパスの一覧を通知します
    property OnDropReceived: TDropFileEvent read FOnDropReceived write FOnDropReceived;
  end;

implementation

uses
  System.SysUtils;

{ TDropTargetInfo }

constructor TDropTargetInfo.Create(AControl: TWinControl; ACallback: TDropFileEvent);
begin
  inherited Create;
  Control := AControl;
  OriginalWndProc := Control.WindowProc;
  FCallback := ACallback;
end;

procedure TDropTargetInfo.CustomWndProc(var Msg: TMessage);
var
  DropMsg: TWMDropFiles absolute Msg;
  Count, I: Integer;
  Buffer: array[0..MAX_PATH] of Char;
  Files: TArray<string>;
begin
  if Msg.Msg = WM_DROPFILES then
  begin
    Count := DragQueryFile(DropMsg.Drop, $FFFFFFFF, nil, 0);
    SetLength(Files, Count);
    for I := 0 to Count - 1 do
    begin
      DragQueryFile(DropMsg.Drop, I, Buffer, MAX_PATH);
      Files[I] := Buffer;
    end;
    DragFinish(DropMsg.Drop);

    if Assigned(FCallback) then
      FCallback(nil, Control, Files);
  end;
  OriginalWndProc(Msg);
end;

{ TDropFile }

constructor TDropFile.Create;
begin
  inherited Create;
  FTargets := TObjectList<TDropTargetInfo>.Create(True);
end;

destructor TDropFile.Destroy;
begin
  while FTargets.Count > 0 do
    Detach(FTargets[0].Control);
  FTargets.Free;
  inherited;
end;

function TDropFile.FindTarget(Control: TWinControl): TDropTargetInfo;
var
  Info: TDropTargetInfo;
begin
  for Info in FTargets do
    if Info.Control = Control then
      Exit(Info);
  Result := nil;
end;

procedure TDropFile.Attach(Control: TWinControl);
var
  Info: TDropTargetInfo;
begin
  if Assigned(FindTarget(Control)) then Exit;

  Info := TDropTargetInfo.Create(Control, FOnDropReceived);
  Control.WindowProc := Info.CustomWndProc;
  DragAcceptFiles(Control.Handle, True);
  FTargets.Add(Info);
  Log('Attached: ' + Control.Name);
end;

procedure TDropFile.Detach(Control: TWinControl);
var
  Info: TDropTargetInfo;
begin
  Info := FindTarget(Control);
  if not Assigned(Info) then Exit;

  Control.WindowProc := Info.OriginalWndProc;
  DragAcceptFiles(Control.Handle, False);
  FTargets.Remove(Info);
  Log('Detached: ' + Control.Name);
end;

procedure TDropFile.Log(const S: string);
begin
  {$IFDEF DEBUG}
  OutputDebugString(PChar('[TDropFile] ' + S));
  {$ENDIF}
end;

end.

