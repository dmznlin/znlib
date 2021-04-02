{*******************************************************************************
  作者: dmzn@163.com 2021-04-02
  描述: 消息提示对话框
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
    //提示信息
    procedure LoadPicture(const nName: string);
    //载入图片
  public
    { Public declarations }
  end;

procedure ShowMsgBox(const nHint,nTitle: string);
function ShowQueryBox(const nHint,nTitle: string): Boolean;
//入口函数

implementation

{$R *.dfm}
{$R Forms.res}

//Date: 2021-04-02
//Parm: 消息;标题
//Desc: 消息框
procedure ShowMsgBox(const nHint,nTitle: string);
begin
  with TfFormMessagebox.Create(Application) do
  begin
    Caption := nTitle;
    BtnOK.Caption := '确定';
    BtnExit.Caption := '关闭';

    SetHint(nHint);
    LoadPicture('DialogInfo');

    ShowModal;
    Free;
  end;
end;

//Date: 2021-04-02
//Parm: 消息;标题
//Desc: 询问对话框
function ShowQueryBox(const nHint,nTitle: string): Boolean;
begin
  with TfFormMessagebox.Create(Application) do
  begin
    Caption := nTitle;
    BtnOK.Caption := '是';
    BtnExit.Caption := '否';

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

//Desc: 设置提示信息,调整窗口宽高
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

  LabelHint.Height := nNum * (Canvas.TextHeight('润') + 2);
  LabelHint.Caption := nStr;

  BtnOK.Top := LabelHint.Top + LabelHint.Height + 10;
  BtnExit.Top := BtnOK.Top;
  ClientHeight := BtnExit.Top + BtnExit.Height + 10;
end;

end.
