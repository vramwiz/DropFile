unit DropFile;

{
  Unit Name   : DropFile
  Description: �C�ӂ̃R���g���[���ɑ΂��ăt�@�C���̃h���b�O���h���b�v��t�@�\��ǉ����郆�j�b�g�ł��B
               �O���A�v���P�[�V�����i��: �G�N�X�v���[���[�j����h���b�v���ꂽ�t�@�C���̈ꗗ���擾���A
               �C�x���g�Ƃ��Ēʒm���܂��B

  Author     : vramwiz
  Created    : 2025-07-10
  Updated    : 2025-07-10

  Usage      :
    - DropFile.Attach(TargetControl); ���Ăяo�����ƂŎw��̃R���g���[����D&D��L����
    - �t�@�C�����h���b�v������ OnDropFiles �C�x���g���������A�t�@�C���ꗗ���󂯎��
    - �s�v�ɂȂ����� DropFile.Detach �ŉ�������

  Dependencies:
    - Windows, ActiveX, ShlObj �ȂǁiIDropTarget �C���^�[�t�F�[�X���g�p�j

  Notes:
    - �����t�@�C���̓����h���b�v�ɑΉ�
    - �h���b�v�Ώۂ� TWinControl ���p�����Ă���K�v������܂�
    - �W���� TFileDrop �C�x���g�����_��Ɛ��䐫�ɗD��Ă��܂�

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
    // �w�肵���R���g���[���ɑ΂��ăt�@�C���̃h���b�v��t��L�������܂�
    procedure Attach(Control: TWinControl);
    // �o�^���ꂽ�h���b�v��t���������AD&D�@�\�𖳌������܂�
    procedure Detach(Control: TWinControl);
    // �t�@�C�����h���b�v���ꂽ�Ƃ��ɌĂяo����A�t�@�C���p�X�̈ꗗ��ʒm���܂�
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

