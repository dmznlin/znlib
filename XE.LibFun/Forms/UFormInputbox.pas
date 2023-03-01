{*******************************************************************************
  作者: dmzn@163.com 2021-04-02
  描述: 提供输入对话框
*******************************************************************************}
unit UFormInputbox;

interface

uses
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  dxSkinsDefaultPainters, cxTextEdit, cxLabel;

type
  TfFormInputBox = class(TForm)
    BtnOK: TButton;
    BtnExit: TButton;
    LabelHint: TcxLabel;
    EditValue: TcxTextEdit;
  private
    { Private declarations }
    procedure SetHint(const nStr: string);
    //提示信息
  public
    { Public declarations }
  end;

function ShowInputBox(const nHint,nTitle: string; var nValue: string;
  const nSize: Word = 0): Boolean;
function ShowInputPWDBox(const nHint,nTitle: string; var nValue: string;
  const nSize: Word = 0): Boolean;
//入口函数

implementation

{$R *.dfm}

//Desc: 文本框
function ShowInputBox(const nHint,nTitle: string; var nValue: string;
  const nSize: Word = 0): Boolean;
begin
  with TfFormInputBox.Create(Application) do
  begin
    Caption := nTitle;
    SetHint(nHint);
    EditValue.Text := nValue;
    EditValue.Properties.MaxLength := nSize;

    Result := ShowModal = mrOK;
    if Result then nValue := EditValue.Text;
    Free;
  end;
end;

//Desc: 密码框
function ShowInputPWDBox(const nHint,nTitle: string; var nValue: string;
  const nSize: Word = 0): Boolean;
begin
  with TfFormInputBox.Create(Application) do
  begin
    Caption := nTitle;
    SetHint(nHint);

    EditValue.Text := nValue;
    EditValue.Properties.MaxLength := nSize;
    EditValue.Properties.PasswordChar := '*';
    EditValue.Properties.EchoMode := eemPassword;

    Result := ShowModal = mrOK;
    if Result then nValue := EditValue.Text;
    Free;
  end;
end;

//Desc: 设置提示信息,调整窗口宽高
procedure TfFormInputBox.SetHint(const nStr: string);
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

  LabelHint.Height := nNum * (Canvas.TextHeight('润') + 5);
  LabelHint.Caption := nStr;

  EditValue.Top := LabelHint.Top + LabelHint.Height + 5;
  BtnOK.Top := EditValue.Top + EditValue.Height + 10;
  BtnExit.Top := BtnOK.Top;
  ClientHeight := BtnExit.Top + BtnExit.Height + 10;
end;

end.
