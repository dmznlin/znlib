{*******************************************************************************
  ����: dmzn@163.com 2017-03-27
  ����: ͳһ������ֹ�������ȫ�ֱ���
*******************************************************************************}
unit UManagerGroup;

{$I LibFun.Inc}
interface

uses
  System.Rtti, System.SysUtils, System.Classes, UBaseObject, UObjectPool,
  UMemDataPool,
  {$IFDEF EnableDBManager}UDBManager,{$ENDIF}
  {$IFDEF EnableMenuManager}UMenuManager,{$ENDIF}
  {$IFDEF EnableLogManager}UMgrLog,{$ENDIF}
  {$IFDEF EnablePlugManager}UMgrPlugs,{$ENDIF}
  {$IFDEF EnableTaskMonitor}UTaskMonitor,{$ENDIF}
  {$IFDEF EnableThreadPool}UThreadPool,{$ENDIF}
  {$IFDEF EnableChannelManager}UMgrChannel,{$ENDIF}
  {$IFDEF EnableMQTTMessager}UMosMessager,{$ENDIF}
  {$IFDEF EnableParamManager}UParameters,{$ENDIF} ULibFun, UWaitItem;

type
  TSimpleLogger = class(TObject)
  private
    FFileExt: string;
    FLogField: string;
    {*��־����*}
    FWritePath: string;
    FWriteLock: TCrossProcWaitObject;
    {*ͬ������*}
  public
    constructor Create(const nPath: string; const nExt: string = '';
      const nField: string = ''; const nSyncLock: Boolean = False);
    destructor Destroy; override;
    {*�����ͷ�*}
    procedure WriteLog(const nClass,nDesc,nEvent: string);
    {*��¼��־*}
  end;

  TManagersCallMethod = reference to procedure (const nObj: TObject;
    const nInstance: TRttiInstanceType; const nMethod: TRttiMethod);
  //xxxxx

  PManagerGroup = ^TManagerGroup;
  TManagerGroup = record
  public
    const 
      sAllManager = 'ALLManager';
      {*��������*}
    type
      TItem = record
        FClass   : TClass;
        FManager : TManagerBase;
      end;
      {*ʵ������*}
    var
      FManagers: array of TItem;
      {*ʵ���б�*}
      FSimpleLogger: TSimpleLogger;
      {*������־*}
  public
    FObjectPool: TObjectPoolManager;
    //���󻺳��
    FMemDataManager: TMemDataManager;
    //�ڴ������
    FSerialIDManager: TSerialIDManager;
    //��Ź�����
    FObjectManager: TCommonObjectManager;
    //���������
    {$IFDEF EnableLogManager}FLogManager: TLogManager;{$ENDIF}
    //��־������
    {$IFDEF EnablePlugManager}FPlugManager: TPlugManager;{$ENDIF}
    //���������
    {$IFDEF EnableTaskMonitor}FTaskMonitor: TTaskMonitor;{$ENDIF}
    //���������
    {$IFDEF EnableThreadPool}FThreadPool: TThreadPoolManager;{$ENDIF}
    //�̹߳�����
    {$IFDEF EnableMQTTMessager}FMessageCenter: TMQTTMessager;{$ENDIF}
    //��Ϣ������
    {$IFDEF EnableDBManager}FDBManager: TDBManager;{$ENDIF}
    //���ݿ������
    {$IFDEF EnableMenuManager}FMenuManager: TMenuManager;{$ENDIF}
    //�˵�������
    {$IFDEF EnableParamManager}FParamsManager: TParameterManager;{$ENDIF}
    //����������
    {$IFDEF EnableChannelManager}FChannelManager: TChannelManager;{$ENDIF}
    //RemObjectsͨ��������
  private
    procedure InnerLog(const nEvent: string);
    //�������ڲ���־
  public
    procedure CallManagersMethod(const nMethodName: string;
      const nObjMethod: Boolean; const nCallback: TManagersCallMethod);
    //ö�����й�����ִ���ض�����
    procedure RegistAll(const nReg: Boolean);
    //ע������
    procedure CheckSupport(const nCallClass,nManagerName: string;
      const nManager: TObject); overload;
    procedure CheckSupport(const nCallClass: string;
      const nManagers: TStringHelper.TStringArray); overload;
    //��֤����������Ƿ�����
    procedure RunAfterApplicationStart();
    procedure RunBeforApplicationHalt();
    //��������������,�����ض���Դ
    procedure GetManagersStatus(const nList: TStrings);
    //��ȡ��Ч�������ĵ�ǰ״̬
    procedure WriteLog(const nClass,nDesc,nEvent: string); overload;
    procedure WriteLog(const nClass: TClass; const nDesc,nEvent: string); overload;
    //��־������δ��Чʱ��¼������־
  end;

var
  gMG: TManagerGroup;
  //ȫ��ʹ��
  
implementation

constructor TSimpleLogger.Create(const nPath,nExt,nField: string;
  const nSyncLock: Boolean);
begin
  FFileExt := Trim(nExt);
  if FFileExt = '' then
    FFileExt := '.log';
  //xxxxx

  FLogField := Trim(nField);
  if FLogField = '' then
    FLogField := #9;
  //xxxxx

  if not DirectoryExists(nPath) then
    ForceDirectories(nPath);
  FWritePath := nPath;

  if nSyncLock then
       FWriteLock := TCrossProcWaitObject.Create()
  else FWriteLock := nil; //for thread or process sync
end;

destructor TSimpleLogger.Destroy;
begin
  FWriteLock.Free;
  inherited;
end;

//Date: 2021-01-04
//Parm: ��־��������;��������;�¼�
//Desc: ����־�ļ���д��nClass.nEvent�¼�
procedure TSimpleLogger.WriteLog(const nClass,nDesc,nEvent: string);
var nStr: string;
    nFile: TextFile;
begin
  if Assigned(FWriteLock) then
    FWriteLock.SyncLockEnter(True);
  //xxxxx
  try
    nStr := FWritePath +  TDateTimeHelper.Date2Str(Now) + FFileExt;
    AssignFile(nFile, nStr);

    if FileExists(nStr) then
         Append(nFile)
    else Rewrite(nFile);

    nStr := FormatDateTime('hh:nn:ss.zzz', Time()) + FLogField +
            Copy(nClass, 1, 32) + FLogField;
    //ʱ��,����

    if nDesc <> '' then
      nStr := nStr + nDesc + FLogField;            //����
    WriteLn(nFile, nStr + nEvent);                 //д���¼�
  finally
    if Assigned(FWriteLock) then
      FWriteLock.SyncLockLeave(True);
    CloseFile(nFile);
  end;
end;

//------------------------------------------------------------------------------
//Date: 2019-05-27
//Parm: ��������;���󷽷�;�ص�����
//Desc: ö�����й���,ִ��nMethodName����
procedure TManagerGroup.CallManagersMethod(const nMethodName: string;
  const nObjMethod: Boolean; const nCallBack: TManagersCallMethod);
var nObj: TObject;
    nCtx: TRttiContext;
    nType: TRttiType;
    nRF: TRttiField;
    nMethod: TRttiMethod;
    nInstance: TRttiInstanceType;
begin
  nCtx := TRttiContext.Create;
  try
    nType := nCtx.GetType(TypeInfo(TManagerGroup));
    for nRF in nType.GetFields do
    begin
      if nRF.FieldType.TypeKind <> tkClass then Continue;
      nInstance := nRF.FieldType.AsInstance;
      nMethod := nInstance.GetMethod(nMethodName);

      if Assigned(nMethod) then
      begin
        if nObjMethod then
        begin
          nObj := nRF.GetValue(@gMG).AsObject;
          if Assigned(nObj) then
            nCallBack(nObj, nInstance, nMethod);
          //object call
        end else
        begin
          nCallBack(nil, nInstance, nMethod);
          //type call
        end;
      end;
    end;
  finally
    nCtx.Free;
  end;
end;

//Date: 2017-03-27
//Parm: �Ƿ�ע��
//Desc: ɨ��Group������Manager,����Manager��ע�᷽��.
procedure TManagerGroup.RegistAll(const nReg: Boolean);
begin
  if not nReg then
  begin
    CallManagersMethod('RunBeforUnregistAllManager', True,
    procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
      const nMethod: TRttiMethod)
    begin
      nMethod.Invoke(nObj, []);
    end);
  end; //ж��ǰִ��

  CallManagersMethod('RegistMe', False,
  procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
    const nMethod: TRttiMethod)
  begin
    nMethod.Invoke(nInstance.MetaclassType, [TValue.From(nReg)]);
  end); //ִ��ע��

  if nReg then
  begin
    CallManagersMethod('RunAfterRegistAllManager', True,
    procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
      const nMethod: TRttiMethod)
    begin
      nMethod.Invoke(nObj, []);
    end);
  end; //ע���ִ��
end;

//Date: 2021-04-11
//Parm: �¼�
//Desc: ��¼�������ڲ��¼�
procedure TManagerGroup.InnerLog(const nEvent: string);
begin
  WriteLog('TManagerGroup', '��������', nEvent);
end;

//Date: 2017-04-18
//Parm: ������;������;����������
//Desc: ��nCallClass��ҪnManager֧��,��nManagerΪnil,�׳��쳣.
procedure TManagerGroup.CheckSupport(const nCallClass,nManagerName: string;
  const nManager: TObject);
var nStr: string;
begin
  if not Assigned(nManager) then
  begin
    nStr := '%s Needs TManagerGroup.%s(nil) Support.';
    nStr := Format(nStr, [nCallClass, nManagerName]);

    InnerLog(nStr);
    raise Exception.Create(nStr);
  end;
end;

//Date: 2017-04-17
//Parm: ������;���������������
//Desc: ���nCallClsss�����nManagers�Ƿ����
procedure TManagerGroup.CheckSupport(const nCallClass: string;
  const nManagers: TStringHelper.TStringArray);
var nStr,nBase: string;
    nBool: Boolean;
    nCtx: TRttiContext;
    nType: TRttiType;
    nRF: TRttiField;    
begin
  nCtx := TRttiContext.Create;
  try
    nType := nCtx.GetType(TypeInfo(TManagerGroup));
    for nBase in nManagers do
    begin
      nBool := False;
      //init flag
      
      for nRF in nType.GetFields do
      begin
        nBool := (nBase = nRF.FieldType.Name) or (nBase = sAllManager);
        if not nBool then Continue;
        
        if nRF.FieldType.TypeKind = tkClass then
        begin
          CheckSupport(nCallClass, nRF.Name, nRF.GetValue(@gMG).AsObject);
          if nBase <> sAllManager then
            Break; 
          //match done  
        end else

        if nBase <> sAllManager then        
        begin
          nStr := '%s: Manager "%s" Is Not Valid Class.';
          nStr := Format(nStr, [nCallClass, nBase]);

          InnerLog(nStr);
          raise Exception.Create(nStr);
        end;  
      end;

      if not nBool then
      begin
        nStr := '%s: Manager "%s" Is Not Exists.';
        nStr := Format(nStr, [nCallClass, nBase]);

        InnerLog(nStr);
        raise Exception.Create(nStr);
      end; //not exits
    end;
  finally
    nCtx.Free;
  end;    
end;

//Date: 2019-04-24
//Parm: �б�
//Desc: ��ȡ��ǰ��Ч�Ĺ�����״̬��Ϣ
procedure TManagerGroup.GetManagersStatus(const nList: TStrings);
begin
  CallManagersMethod('GetStatus', True,
  procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
    const nMethod: TRttiMethod)
  begin
    nMethod.Invoke(nObj, [TValue.From(nList), TValue.From(True)]);
  end);
end;

//Date: 2019-05-27
//Desc: �������������
procedure TManagerGroup.RunAfterApplicationStart;
begin
  CallManagersMethod('RunAfterApplicationStart', True,
  procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
    const nMethod: TRttiMethod)
  begin
    nMethod.Invoke(nObj, []);
  end);
end;

//Date: 2019-05-27
//Desc: �����������ر�
procedure TManagerGroup.RunBeforApplicationHalt;
begin
  CallManagersMethod('RunBeforApplicationHalt', True,
  procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
    const nMethod: TRttiMethod)
  begin
    nMethod.Invoke(nObj, []);
  end);
end;

//Date: 2021-04-09
//Parm: ��־����;��������;�¼�
//Desc: ����־�ļ���д��nClass.nEvent�¼�
procedure TManagerGroup.WriteLog(const nClass: TClass; const nDesc,nEvent: string);
begin
  FSimpleLogger.WriteLog(nClass.ClassName, nDesc, nEvent);
end;

//Date: 2021-04-09
//Parm: ��־��������;��������;�¼�
//Desc: ����־�ļ���д��nClass.nEvent�¼�
procedure TManagerGroup.WriteLog(const nClass, nDesc, nEvent: string);
begin
  FSimpleLogger.WriteLog(nClass, nDesc, nEvent);
end;

initialization
  FillChar(gMG, SizeOf(TManagerGroup), #0);
  gMG.FSimpleLogger := TSimpleLogger.Create(TApplicationHelper.gLogPath, '_S.log');
  gMG.RegistAll(True);
finalization
  gMG.RegistAll(False);
  gMG.FSimpleLogger.Free;
end.
