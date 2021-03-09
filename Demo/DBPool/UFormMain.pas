unit UFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Btn1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
uses
  UManagerGroup, UDBManager;

procedure SystemDefault;
var nCfg: TDBConnConfig;
begin
  nCfg.FID := gMG.FDBManager.DefaultDB;
  nCfg.FName := '默认库';
  nCfg.FFitDB := dtMSSQL;
  nCfg.FConn := Form1.Edit1.Text;
  gMG.FDBManager.AddDB(nCfg);
end;

procedure SystemTables(const nList: TList);
begin
  gMG.FDBManager.AddTable('TableA', nList).
  AddF('D_ID',          'varChar(15)',            '记录标识').
  AddF('D_Port',        'Integer',                '端口',      '80').
  AddF('D_Serial',      'Integer Default -1',     '装置号',    '-1').
  //字段
  AddI('idx_name', 'Create Index idx_name On $TB.*( D_ID ASC )').
  //添加索引
  AddR('R1', 'Insert Into $TB.* Values(''aa'', 81, 1)').
  AddR('R2', 'Insert Into $TB.* Values(''bb'', 82, 2)').
  //初始化记录
  Copy('TableB');
  //复制表结构,初始记录,字段默认值,字段索引等
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  with gMG.FLogManager do
  begin
    StartService(ExtractFilePath(Application.ExeName) + 'Logs\');
    //启动日志
  end;

  with gMG.FDBManager do
  begin
    SystemDefault;
    //添加连接
    AddTableBuilder( SystemTables );
    //添加表描述
  end;
end;

procedure TForm1.Btn1Click(Sender: TObject);
begin
  gMG.FDBManager.InitDB(gDBManager.DefaultDB, Memo1.Lines);
end;

end.
