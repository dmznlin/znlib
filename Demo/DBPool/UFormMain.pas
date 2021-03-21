unit UFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    BtnStatus: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditDrv: TComboBox;
    BtnInit: TButton;
    BtnTrans: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnStatusClick(Sender: TObject);
    procedure BtnInitClick(Sender: TObject);
    procedure BtnTransClick(Sender: TObject);
    procedure EditDrvChange(Sender: TObject);
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
  UManagerGroup, UDBManager, UDBDriverADO_V2, UThreadPool, Data.DB;

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
  AddF('R_ID',          sField_SQLServer_AutoInc, '自增标识').
  AddF('D_ID',          'varChar(15)',            '记录标识').
  AddF('D_Port',        'Integer',                '端口',      '80').
  AddF('D_Serial',      'Integer Default -1',     '装置号',    '-1').
  //字段
  AddI('idx_name', 'Create Index $IDX On $TBS( D_ID ASC )').
  //添加索引
  AddR('R1', 'Insert Into $TBS Values(''aa'', 81, 1)').
  AddR('R2', 'Insert Into $TBS Values(''bb'', 82, 2)').
  //初始化记录
  Copy('TableB');
  //复制表结构,初始记录,字段默认值,字段索引等
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var nIdx: Integer;
begin
  with gMG.FLogManager do
  begin
    StartService(ExtractFilePath(Application.ExeName) + 'Logs\');
    //启动日志
  end;

  gMG.FThreadPool.ThreadMin := 3;
  //限制线程数

  with gMG.FDBManager do
  begin
    for nIdx := Low(Drivers) to High(Drivers) do
      EditDrv.Items.Add(Drivers[nIdx].DriverInfo.DrvName);
    EditDrv.ItemIndex := EditDrv.Items.IndexOf(Driver.DriverInfo.DrvName);
    //驱动列表

    SystemDefault;
    //添加连接
    AddTableBuilder( SystemTables );
    //添加表描述
  end;
end;

procedure TForm1.EditDrvChange(Sender: TObject);
begin
  if EditDrv.ItemIndex > -1 then
    gMG.FDBManager.ActiveDriver(EditDrv.Text);
  //切换驱动
end;

procedure TForm1.BtnInitClick(Sender: TObject);
begin
  gMG.FDBManager.InitDB(gDBManager.DefaultDB, Memo1.Lines);
end;

procedure TForm1.BtnStatusClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  gMG.GetManagersStatus(Memo1.Lines);
end;

procedure WriteDB_InTransB;
var nIdx: Integer;
    nList: TStrings;
begin
  nList := nil;
  try
    nList := gMG.FObjectPool.Lock(TStrings) as TStrings;
    for nIdx := 2 to 2 do
      nList.Add( 'Insert Into TableA(D_ID, D_Port) Values(''WriteB'', ' + nIdx.ToString + ')' );
    //xxxxx

    gMG.FDBManager.DBExecute(nList);
    //以事务模式执行
  finally
    gMG.FObjectPool.Release(nList);
  end;
end;

procedure WriteDB_InTransA;
var nStr: string;
begin
  nStr := 'Insert Into TableA(D_ID, D_Port) Values(''WriteA'', 1)';
  gMG.FDBManager.DBExecute(nStr);
  WriteDB_InTransB;
end;

procedure TForm1.BtnTransClick(Sender: TObject);
var nIdx: Integer;
    nWorker: TThreadWorkerConfig;
begin
  for nIdx := 1 to 3 do
  with gMG.FThreadPool do
  begin
    WorkerInit(nWorker);
    with nWorker do
    begin
      FWorkerName   := '线程嵌套事务';
      FParentObj    := Self;
      FParentDesc   := self.ClassName;

      FCallInterval := nIdx * 500;
      FCallTimes    := 1;
      FAutoDelete   := True;
      FCoInitialize := True;

      FOnWork.WorkRefer := procedure (const nConfig: PThreadWorkerConfig;
        const nThread: TThread)
      var nQA,nQB: TDataset;
      begin
        nQA := nil;
        nQB := nil;
        with gMG.FDBManager do
        try
          WriteLog('');
          nQA := LockDBQuery();
          nQB := LockDBQuery();

          with DBQuery('select * From TableA', nQA) do
          if RecordCount > 0 then
          begin
            Writelog(Fields[0].AsString);
          end;

          InitTrans(nQA);         //重置事务计数
          BeginTrans(nQA);        //A.事务开始:开启事务,增加计数
          try
            BeginTrans(nQB);      //B.事务开始:同线程再次开启事务时,只增加计数
            try
              CommitTrans(nQB);   //B.事务提交:同线程提交事务时,只减少计数
            except
              RollbackTrans(nQB); //B.事务回滚:同线程事务回滚
            end;

            WriteDB_InTransA;     //事务内嵌套调用,同属于一个事务
            CommitTrans(nQA);     //A.事务提交:计数为1时提交事务
            WriteDB_InTransB;     //事务外嵌套调用,单独事务
          except
            RollbackTrans(nQA);   //A.事务回滚,若已回滚则忽略
          end;
        finally
          ReleaseDBQuery(nQA);
          ReleaseDBQuery(nQB);
        end;
      end;
    end;

    WorkerAdd(@nWorker);
    //投递执行
  end;
end;

end.
