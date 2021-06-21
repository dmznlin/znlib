unit UFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfFormMain = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fFormMain: TfFormMain;

implementation

{$R *.dfm}
uses
  ULibFun, UManagerGroup, UMgrDataDict, UMenuManager;

procedure SytemDictBuilder(const nList: TList);
var nEty: PDictEntity;
begin
  nEty := gMG.FDataDictManager.AddEntity('Main_A01', '������־', nList);
  nEty.AddDict('R_ID',        '��ʶ').
       AddDict('L_Name',      '����').
       AddDict('L_Owner',     'ӵ����').
       AddDict('L_Index',     '˳��');
  //����ֵ���

  with nEty.ByField('R_ID').FFooter do
  begin
    FDisplay  := 'total:';
    FFormat   := '�ϼ�: �� 0 ��';
    FKind     := fkCount;
    FPosition := fpAll;
  end; //��չ�ֵ���
end;

procedure TfFormMain.FormCreate(Sender: TObject);
begin
  gMG.FDataDictManager.AddDictBuilder(SytemDictBuilder);
end;

procedure TfFormMain.Button1Click(Sender: TObject);
begin
  gMG.FDBManager.InitDB('MAIN', Memo1.Lines);
end;

procedure TfFormMain.Button2Click(Sender: TObject);
begin
  gMG.FDataDictManager.InitDictData('cn', Memo1.Lines);
end;

procedure TfFormMain.Button3Click(Sender: TObject);
var nIdx: Integer;
    nEntity: TDictEntity;
begin
  gMG.FDataDictManager.GetEntity('Main_A01', 'cn', @nEntity);
  if nEntity.FEntity = '' then Exit; //δ�ҵ�ʵ��

  Memo1.Lines.Clear;
  for nIdx := Low(nEntity.FItems) to High(nEntity.FItems) do
  with nEntity.FItems[nIdx] do
  begin
    Memo1.Lines.Add(Format('%d.%s.%s',[FIndex, FDBItem.FField, FTitle]));
  end;
end;

procedure TfFormMain.Button4Click(Sender: TObject);
begin
  gMG.GetManagersStatus(Memo1.Lines);
end;

end.
