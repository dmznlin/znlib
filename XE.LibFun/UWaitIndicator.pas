{*******************************************************************************
  ����: dmzn@163.com 2020-06-10
  ����: �ȴ�ʱ����ָʾ
*******************************************************************************}
unit UWaitIndicator;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, ULibFun;

procedure ShowWaitForm(const nPForm: TForm; const nMsg: string = '';
 const nRefresh: Boolean = False); overload;
procedure CloseWaitForm;
//��ں���

implementation

{$R LibFun.RES}

const
  cShowAfter = 300;
  //�ӳ���ʾ,������ʱ�䲻������ʾ����
  cShowKeep  = 2000;
  //������ʾ,����ʾ���������ٱ���x����
  cLabel : array [0..6] of string = (
           '�� %s ��',
           '�ˡ� %s �ˡ�',
           '�ˡˡ� %s �ˡˡ�',
           '�ˡˡˡ� %s �ˡˡˡ�',
           '�ˡˡˡˡ� %s �ˡˡˡˡ�',
           '�ˡˡˡˡˡ� %s �ˡˡˡˡˡ�',
           '�ˡˡˡˡˡˡ� %s �ˡˡˡˡˡˡ�');
  //����ָʾ�ַ���

type
  TWaitThread = class(TThread)
  private
    FMsg: string;
    FOwner: TForm;
    FLow,FMax,FIdx: integer;

    FImage: TBitMap;
    FCanvas: TCanvas;
    FImageRect: TRect;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TForm);
    destructor Destroy; override;
    property HintMsg: string read FMsg write FMsg;
  end;

var
  gThread: TWaitThread = nil;
  //ȫ��ʹ��

//Date: 2020-06-10
//Parm: ������;��ʾ��Ϣ;ˢ��
//Desc: ��nPForm���м���ʾһ���ȴ�����
procedure ShowWaitForm(const nPForm: TForm; const nMsg: string;
  const nRefresh: Boolean);
begin
  if nRefresh then
    Application.ProcessMessages;
  //update ui

  if not Assigned(gThread) then
    gThread := TWaitThread.Create(nPForm);
  gThread.HintMsg := nMsg;
end;

//Date: 2020-06-10
//Desc: �ͷŵȴ�����
procedure CloseWaitForm;
begin
  if Assigned(gThread) then
  begin
    gThread.Terminate;
    gThread.WaitFor;
    FreeAndNil(gThread);
  end;
end;

//------------------------------------------------------------------------------
constructor TWaitThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FOwner := AOwner;
end;

destructor TWaitThread.Destroy;
begin
  //do any
  inherited;
end;

//Desc: ����
procedure TWaitThread.Execute;
var nStr: string;
    nH,nW: integer;
    nHwnd: THandle;
    nInit: Cardinal;
begin
  nInit := GetTickCount();
  while TDatetimeHelper.GetTickCountDiff(nInit) <= cShowAfter do
  begin
    if Terminated then Exit;
    //�ӳ���ʾ
  end;

  FImage := TBitMap.Create;
  FImage.Handle := LoadBitmap(Hinstance, 'WaitFormBG');

  with FImageRect do
  begin
    Left := (FOwner.ClientWidth - FImage.Width ) div 2;
    Top := (FOwner.ClientHeight - FImage.Height) div 2;

    Right := Left + FImage.Width;
    Bottom := Top + FImage.Height;

    TopLeft := FOwner.ClientToScreen(TopLeft);
    BottomRight := FOwner.ClientToScreen(BottomRight);
  end;

  FCanvas := TCanvas.Create;
  with FCanvas do
  begin
    Handle := GetDC(0);
    Font.Assign(FOwner.Font);
    Font.Color := clWhite;
  end;

  FLow := Low(cLabel);
  FMax := High(cLabel);
  FIdx := FLow; //Index

  while (not Terminated) or
        (TDatetimeHelper.GetTickCountDiff(nInit) < cShowKeep) do //������ʾ
  begin
    nHwnd := GetForegroundWindow();
    if (nHwnd = FOwner.Handle) or (nHwnd = Application.Handle) then
    begin
      FCanvas.StretchDraw(FImageRect, FImage);
      //���Ʊ���

      nStr := Format(cLabel[FIdx], [FMsg]);
      Inc(FIdx);
      if FIdx > FMax then FIdx := FLow;

      nW := FCanvas.TextWidth(nStr);
      nH := FCanvas.TextHeight(nStr);

      SetBKMode(FCanvas.Handle, Transparent);
      FCanvas.TextOut((FImageRect.Right + FImageRect.Left - nW) div 2,
                      (FImageRect.Bottom + FImageRect.Top - nH) div 2, nStr);
      //����ı�
    end;

    Sleep(500);
    //���ƻ��Ƽ��
  end;

  RedrawWindow(0, @FImageRect, 0, RDW_INVALIDATE or RDW_ALLCHILDREN);
  //������Ļ

  ReleaseDC(0, FCanvas.Handle);
  FCanvas.Free;
  FImage.Free;
end;

end.
