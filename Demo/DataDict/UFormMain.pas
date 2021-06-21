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
  nEty := gMG.FDataDictManager.AddEntity('Main_A01', '操作日志', nList);
  nEty.AddDict('R_ID',        '标识').
       AddDict('L_Name',      '名称').
       AddDict('L_Owner',     '拥有人').
       AddDict('L_Index',     '顺序');
  //添加字典项

  with nEty.ByField('R_ID').FFooter do
  begin
    FDisplay  := 'total:';
    FFormat   := '合计: 共 0 条';
    FKind     := fkCount;
    FPosition := fpAll;
  end; //扩展字典项
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
  if nEntity.FEntity = '' then Exit; //未找到实体

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
