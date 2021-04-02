{*******************************************************************************
  ����: dmzn@163.com 2021-04-02
  ����: ��Ϣ��ʾ�Ի���
*******************************************************************************}
unit UFormMessagebox;

interface

uses
  System.Classes, System.Types, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  dxSkinsDefaultPainters, dxGDIPlusClasses, cxImage, cxLabel;

type
  TfFormMessagebox = class(TForm)
    BtnOK: TButton;
    BtnExit: TButton;
    LabelHint: TcxLabel;
    Image1: TcxImage;
  private
    { Private declarations }
    procedure SetHint(const nStr: string);
    //��ʾ��Ϣ
    procedure LoadPicture(const nName: string);
    //����ͼƬ
  public
    { Public declarations }
  end;

procedure ShowMsgBox(const nHint,nTitle: string);
function ShowQueryBox(const nHint,nTitle: string): Boolean;
//��ں���

implementation

{$R *.dfm}
{$R Forms.res}

//Date: 2021-04-02
//Parm: ��Ϣ;����
//Desc: ��Ϣ��
procedure ShowMsgBox(const nHint,nTitle: string);
begin
  with TfFormMessagebox.Create(Application) do
  begin
    Caption := nTitle;
    BtnOK.Caption := 'ȷ��';
    BtnExit.Caption := '�ر�';

    SetHint(nHint);
    LoadPicture('DialogInfo');

    ShowModal;
    Free;
  end;
end;

//Date: 2021-04-02
//Parm: ��Ϣ;����
//Desc: ѯ�ʶԻ���
function ShowQueryBox(const nHint,nTitle: string): Boolean;
begin
  with TfFormMessagebox.Create(Application) do
  begin
    Caption := nTitle;
    BtnOK.Caption := '��';
    BtnExit.Caption := '��';

    SetHint(nHint);
    LoadPicture('DialogQues');

    Result := ShowModal = mrOK;
    Free;
  end;
end;

procedure TfFormMessagebox.LoadPicture(const nName: string);
var nImg: TPngImage;
begin
  nImg := TPngImage.Create;
  try
    nImg.LoadFromResourceName(HInstance, nName);
    Image1.Picture.Graphic := nImg;
  finally
    nImg.Free;
  end;
end;

//Desc: ������ʾ��Ϣ,�������ڿ��
procedure TfFormMessagebox.SetHint(const nStr: string);
var nList: TStrings;
    nIdx,nNum,nWidth: integer;
begin
  nList := TStringList.Create;
  try
    nList.Text := nStr;
    nNum := 0;

    for nIdx := 0 to nList.Count - 1 do
    begin
      nWidth := Canvas.TextWidth(nList[nIdx]);
      if (nWidth mod LabelHint.Width) = 0 then
           nNum := nNum + nWidth div LabelHint.Width
      else nNum := nNum + nWidth div LabelHint.Width + 1;
    end;
  finally
    nList.Free;
  end;

  LabelHint.Height := nNum * (Canvas.TextHeight('��') + 2);
  LabelHint.Caption := nStr;

  BtnOK.Top := LabelHint.Top + LabelHint.Height + 10;
  BtnExit.Top := BtnOK.Top;
  ClientHeight := BtnExit.Top + BtnExit.Height + 10;
end;

end.
