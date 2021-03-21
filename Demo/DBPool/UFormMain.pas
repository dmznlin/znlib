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
  nCfg.FName := 'Ĭ�Ͽ�';
  nCfg.FFitDB := dtMSSQL;
  nCfg.FConn := Form1.Edit1.Text;
  gMG.FDBManager.AddDB(nCfg);
end;

procedure SystemTables(const nList: TList);
begin
  gMG.FDBManager.AddTable('TableA', nList).
  AddF('R_ID',          sField_SQLServer_AutoInc, '������ʶ').
  AddF('D_ID',          'varChar(15)',            '��¼��ʶ').
  AddF('D_Port',        'Integer',                '�˿�',      '80').
  AddF('D_Serial',      'Integer Default -1',     'װ�ú�',    '-1').
  //�ֶ�
  AddI('idx_name', 'Create Index $IDX On $TBS( D_ID ASC )').
  //�������
  AddR('R1', 'Insert Into $TBS Values(''aa'', 81, 1)').
  AddR('R2', 'Insert Into $TBS Values(''bb'', 82, 2)').
  //��ʼ����¼
  Copy('TableB');
  //���Ʊ�ṹ,��ʼ��¼,�ֶ�Ĭ��ֵ,�ֶ�������
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var nIdx: Integer;
begin
  with gMG.FLogManager do
  begin
    StartService(ExtractFilePath(Application.ExeName) + 'Logs\');
    //������־
  end;

  gMG.FThreadPool.ThreadMin := 3;
  //�����߳���

  with gMG.FDBManager do
  begin
    for nIdx := Low(Drivers) to High(Drivers) do
      EditDrv.Items.Add(Drivers[nIdx].DriverInfo.DrvName);
    EditDrv.ItemIndex := EditDrv.Items.IndexOf(Driver.DriverInfo.DrvName);
    //�����б�

    SystemDefault;
    //�������
    AddTableBuilder( SystemTables );
    //��ӱ�����
  end;
end;

procedure TForm1.EditDrvChange(Sender: TObject);
begin
  if EditDrv.ItemIndex > -1 then
    gMG.FDBManager.ActiveDriver(EditDrv.Text);
  //�л�����
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
    //������ģʽִ��
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
      FWorkerName   := '�߳�Ƕ������';
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

          InitTrans(nQA);         //�����������
          BeginTrans(nQA);        //A.����ʼ:��������,���Ӽ���
          try
            BeginTrans(nQB);      //B.����ʼ:ͬ�߳��ٴο�������ʱ,ֻ���Ӽ���
            try
              CommitTrans(nQB);   //B.�����ύ:ͬ�߳��ύ����ʱ,ֻ���ټ���
            except
              RollbackTrans(nQB); //B.����ع�:ͬ�߳�����ع�
            end;

            WriteDB_InTransA;     //������Ƕ�׵���,ͬ����һ������
            CommitTrans(nQA);     //A.�����ύ:����Ϊ1ʱ�ύ����
            WriteDB_InTransB;     //������Ƕ�׵���,��������
          except
            RollbackTrans(nQA);   //A.����ع�,���ѻع������
          end;
        finally
          ReleaseDBQuery(nQA);
          ReleaseDBQuery(nQB);
        end;
      end;
    end;

    WorkerAdd(@nWorker);
    //Ͷ��ִ��
  end;
end;

end.
