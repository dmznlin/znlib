{*******************************************************************************
  ����: dmzn@163.com 2019-04-23
  ����: ���ڲ��ģʽ��ģ�������

  ��ע:
  *.��һ��ϵͳ�ɶ��������ģ�����ʱ,����ʹ�ò��������ʵ�ָ���.
  *.����:һ���м����Ҫͬʱ֧��ҵ������Ӳ������,ֻ��Ҫ��д�̳���
    TPlugEventWorkerBase����������,Ȼ������ע�ᵽ���������.
  *.��������Ҳ�ɴ��м���з���,��Ϊ������ҵ���м����Ӳ�������м��.
*******************************************************************************}
unit UMgrPlugs;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, {$IFDEF MSWin}Winapi.Windows,{$ENDIF}
  UBaseObject;

const
  {*plug event*}
  cPlugEvent_InitSystemObject     = $0001;
  cPlugEvent_RunSystemObject      = $0002;
  cPlugEvent_FreeSystemObject     = $0003;
  cPlugEvent_BeforeStartServer    = $0004;
  cPlugEvent_AfterServerStarted   = $0005;
  cPlugEvent_BeforeStopServer     = $0006;
  cPlugEvent_AfterStopServer      = $0007;
  cPlugEvent_BeforeHaltServer     = $0008;

type
  TPlugModuleInfo = record
    FModuleID       : string;    //��ʶ
    FModuleName     : string;    //����
    FModuleAuthor   : string;    //����
    FModuleVersion  : string;    //�汾
    FModuleDesc     : string;    //����
    FModuleFile     : string;    //�ļ�
    FModuleBuildTime: TDateTime; //����ʱ��
  end;

  TPlugModuleList = array of TPlugModuleInfo;
  //ģ����Ϣ�б�

  PPlugMenuItem = ^TPlugMenuItem;
  TPlugMenuItem = record
    FModule     : string;        //ģ���ʶ
    FName       : string;        //�˵���
    FCaption    : string;        //�˵�����
    FFormID     : Integer;       //���ܴ���
    FDefault    : Boolean;       //Ĭ������
  end;

  TPlugMenuItems = array of TPlugMenuItem;
  //ģ��˵��б�

  TPlugEventWorkerBase = class(TObject)
  private
    FWorkerValid: Boolean;
    //��Ч��ʶ
  protected
    procedure InitSystemObject(const nData: Pointer); virtual;
    //����������ʱ��ʼ��
    procedure RunSystemObject(const nData: Pointer); virtual;
    //����������������
    procedure FreeSystemObject(const nData: Pointer); virtual;
    //�������˳�ʱ�ͷ�
    procedure BeforeStartServer(const nData: Pointer); virtual;
    //��������֮ǰ����
    procedure AfterServerStarted(const nData: Pointer); virtual;
    //��������֮�����
    procedure BeforeStopServer(const nData: Pointer); virtual;
    //����ر�֮ǰ����
    procedure AfterStopServer(const nData: Pointer); virtual;
    //����ر�֮�����
    function WhenEventActive(const nEvent: string;
      const nData: Pointer): Boolean; overload; virtual;
    function WhenEventActive(const nEvent: Integer;
      const nData: Pointer): Boolean; overload; virtual;
    //�ⲿ�����ض��¼�
    procedure GetExtendMenu(var nMenus: TPlugMenuItems); virtual;
    //������չ�˵���
  public
    constructor Create;
    destructor Destroy; override;
    //�����ͷ�
    class function ModuleInfo: TPlugModuleInfo; virtual;
    //ģ����Ϣ
  end;

  TPlugManager = class(TManagerBase)
  private
    FWorkers: array of TPlugEventWorkerBase;
    {*��������*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*�ӳ�ִ��*}
    procedure InitSystemObject(const nData: Pointer = nil);
    procedure RunSystemObject(const nData: Pointer = nil);
    procedure FreeSystemObject(const nData: Pointer = nil);
    {*����ҵ��*}
    procedure BeforeStartServer(const nData: Pointer = nil);
    procedure AfterServerStarted(const nData: Pointer = nil);
    procedure BeforeStopServer(const nData: Pointer = nil);
    procedure AfterStopServer(const nData: Pointer = nil);
    {*������ͣ*}
    procedure ActiveEvent(const nEvent: string; const nData: Pointer = nil;
      const nValidWorker: Boolean = True); overload;
    procedure ActiveEvent(const nEvent: Integer; const nData: Pointer = nil;
      const nValidWorker: Boolean = True); overload;
    {*�ض��¼�*}
    procedure AddEventWorker(const nWorker: TPlugEventWorkerBase);
    procedure DeleteWorker(const nModule: string);
    {*��������*}
    function GetMenuItems: TPlugMenuItems;
    {*ģ��˵�*}
    function GetModuleInfo(const nModule: string): TPlugModuleInfo;
    function GetModuleInfoList: TPlugModuleList;
    {*ģ���б�*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*��ȡ״̬*}
  end;

var
  gPlugManager: TPlugManager = nil;
  //ȫ��ʹ��

implementation

uses
  UManagerGroup, ULibFun;

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TPlugManager, '���������', nEvent);
end;

constructor TPlugEventWorkerBase.Create;
begin
  inherited;
  //nothing
end;

destructor TPlugEventWorkerBase.Destroy;
begin
  //nothing
  inherited;
end;

procedure TPlugEventWorkerBase.InitSystemObject;
begin

end;

procedure TPlugEventWorkerBase.RunSystemObject;
begin

end;

procedure TPlugEventWorkerBase.FreeSystemObject;
begin

end;

procedure TPlugEventWorkerBase.BeforeStartServer;
begin

end;

procedure TPlugEventWorkerBase.AfterServerStarted;
begin

end;

procedure TPlugEventWorkerBase.BeforeStopServer;
begin

end;

procedure TPlugEventWorkerBase.AfterStopServer;
begin

end;

procedure TPlugEventWorkerBase.GetExtendMenu(var nMenus: TPlugMenuItems);
begin

end;

function TPlugEventWorkerBase.WhenEventActive(const nEvent: string;
  const nData: Pointer): Boolean;
begin
  Result := False;
end;

function TPlugEventWorkerBase.WhenEventActive(const nEvent: Integer;
  const nData: Pointer): Boolean;
begin
  Result := False;
end;

class function TPlugEventWorkerBase.ModuleInfo: TPlugModuleInfo;
{$IFDEF MSWin}var nBuf: array[0..MAX_PATH-1] of Char;{$ENDIF}
begin
  with Result do
  begin
    FModuleID       := '{0EE5410B-9334-45DE-A186-713C11434392}';
    FModuleName     := 'ͨ�ÿ�ܲ������';
    FModuleAuthor   := 'dmzn@163.com';
    FModuleVersion  := '2019-01-01';
    FModuleDesc     := '���ͨ���̳и���,���Ի�ÿ�ܵĽӿ�.';
    FModuleBuildTime:= TDateTimeHelper.Str2DateTime('2019-01-01 00:01:01');

    {$IFDEF MSWin}
    FModuleFile := Copy(nBuf, 1, GetModuleFileName(HInstance, nBuf, MAX_PATH));
    //module full file name
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------
constructor TPlugManager.Create;
begin
  inherited;
  SetLength(FWorkers, 0);
end;

destructor TPlugManager.Destroy;
begin

  inherited;
end;

//Date: 2019-04-23
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TPlugManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TPlugManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TPlugManager.Create;
    gMG.FPlugManager := gMG.FManagers[nIdx].FManager as TPlugManager;
  end else
  begin
    gMG.FPlugManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gPlugManager := gMG.FPlugManager;
  //����ȫ�ֱ���
end;

procedure TPlugManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TPlugManager', ['TLogManager']);
  //���֧��
end;

procedure TPlugManager.RunBeforUnregistAllManager;
var nIdx: Integer;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
      FreeAndNil(FWorkers[nIdx]);
    SetLength(FWorkers, 0);
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: �¼�����
//Desc: ����nWorker����
procedure TPlugManager.AddEventWorker(const nWorker: TPlugEventWorkerBase);
var nIdx: Integer;
begin
  SyncEnter;
  try
    nIdx := Length(FWorkers);
    SetLength(FWorkers, nIdx + 1);

    nWorker.FWorkerValid := True;
    FWorkers[nIdx] := nWorker;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: ģ���ʶ
//Desc: ɾ��ָ������
procedure TPlugManager.DeleteWorker(const nModule: string);
var nIdx: Integer;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
     if FWorkers[nIdx].ModuleInfo.FModuleID = nModule then
      FWorkers[nIdx].FWorkerValid := False;
    //xxxxx
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Desc: ��ȡ��չ�˵���
function TPlugManager.GetMenuItems: TPlugMenuItems;
var nIdx: Integer;
begin
  SyncEnter;
  try
    SetLength(Result, 0);
    //init

    for nIdx := Low(FWorkers) to High(FWorkers) do
      FWorkers[nIdx].GetExtendMenu(Result);
    //xxxxx
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: ģ���ʶ
//Desc: ��ȡnModule������
function TPlugManager.GetModuleInfo(const nModule: string): TPlugModuleInfo;
var nIdx: Integer;
    nInit: TPlugModuleInfo;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
    begin
      Result := FWorkers[nIdx].ModuleInfo;
      if Result.FModuleID = nModule then Exit;
    end;

    FillChar(nInit, SizeOf(nInit), #0);
    Result := nInit;
    //default
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: ��
//Desc: ��ȡģ���б�
function TPlugManager.GetModuleInfoList: TPlugModuleList;
var nIdx,nInt: Integer;
begin
  SyncEnter;
  try
    SetLength(Result, Length(FWorkers));
    nInt := 0;
    //init

    for nIdx := Low(FWorkers) to High(FWorkers) do
    begin
      Result[nInt] := FWorkers[nIdx].ModuleInfo;
      Inc(nInt);
    end;
  finally
    SyncLeave;
  end;
end;

procedure TPlugManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nIdx,nInt: Integer;
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);
    nInt := 0;

    for nIdx := Low(FWorkers) to High(FWorkers) do
     if FWorkers[nIdx].FWorkerValid then
      Inc(nInt);
    //xxxxx

    if not nFriendly then
    begin
      nList.Add('NumWorker=' + IntToStr(Length(FWorkers)));
      nList.Add('NumWorkerValid=' + IntToStr(nInt));
      Exit;
    end;

    nList.Add(FixData('NumWorker:', IntToStr(Length(FWorkers))));
    nList.Add(FixData('NumWorkerValid:', IntToStr(nInt)));
  finally
    SyncLeave;
  end;
end;

//------------------------------------------------------------------------------
procedure TPlugManager.InitSystemObject(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_InitSystemObject, nData);
end;

procedure TPlugManager.RunSystemObject(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_RunSystemObject, nData);
end;

procedure TPlugManager.FreeSystemObject(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_FreeSystemObject, nData);
end;

procedure TPlugManager.BeforeStartServer(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_BeforeStartServer, nData);
end;

procedure TPlugManager.AfterServerStarted(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_AfterServerStarted, nData);
end;

procedure TPlugManager.BeforeStopServer(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_BeforeStopServer, nData);
end;

procedure TPlugManager.AfterStopServer(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_AfterStopServer, nData);
end;

//Date: 2019-04-23
//Parm: �¼���ʶ
//Desc: ����nEventID������
function Event2Str(const nEventID: Integer): string;
begin
  case nEventID of
   cPlugEvent_InitSystemObject   : Result := 'InitSystemObject';
   cPlugEvent_RunSystemObject    : Result := 'RunSystemObject';
   cPlugEvent_FreeSystemObject   : Result := 'FreeSystemObject';
   cPlugEvent_BeforeStartServer  : Result := 'BeforeStartServer';
   cPlugEvent_AfterServerStarted : Result := 'AfterServerStarted';
   cPlugEvent_BeforeStopServer   : Result := 'BeforeStopServer';
   cPlugEvent_AfterStopServer    : Result := 'AfterStopServer';
   cPlugEvent_BeforeHaltServer   : Result := 'BeforeHaltServer'
   else Result := Format('Unknown Command(%d)', [nEventID]);
  end;
end;

//Date: 2019-04-23
//Parm: �¼�;����;������Ч
//Desc: ִ��nEvent�¼�
procedure TPlugManager.ActiveEvent(const nEvent: string; const nData: Pointer;
  const nValidWorker: Boolean);
var nStr: string;
    nIdx: Integer;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
    begin
      if nValidWorker and (not FWorkers[nIdx].FWorkerValid) then Continue;
      //worker is close

      with FWorkers[nIdx] do
      try
        WhenEventActive(nEvent, nData);
        //default deal
      except
        on nErr: Exception do
        begin
          nStr := 'ģ��[ %s ]ִ��[ %s ]ʱ����,����: %s';
          WriteLog(Format(nStr, [ModuleInfo.FModuleName, nEvent,
            nErr.Message]));
          //xxxxx
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: �¼�;����;������Ч
//Desc: ִ��nEvent�¼�
procedure TPlugManager.ActiveEvent(const nEvent: Integer; const nData: Pointer;
  const nValidWorker: Boolean);
var nStr: string;
    nIdx: Integer;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
    begin
      if nValidWorker and (not FWorkers[nIdx].FWorkerValid) then Continue;
      //worker is close

      with FWorkers[nIdx] do
      try
        if WhenEventActive(nEvent, nData) then Continue;
        //default deal

        case nEvent of
         cPlugEvent_InitSystemObject   : InitSystemObject(nData);
         cPlugEvent_RunSystemObject    : RunSystemObject(nData);
         cPlugEvent_FreeSystemObject   : FreeSystemObject(nData);
         cPlugEvent_BeforeStartServer  : BeforeStartServer(nData);
         cPlugEvent_AfterServerStarted : AfterServerStarted(nData);
         cPlugEvent_BeforeStopServer   : BeforeStopServer(nData);
         cPlugEvent_AfterStopServer    : AfterStopServer(nData);

         cPlugEvent_BeforeHaltServer   : //�����˳�
          begin
            BeforeStopServer(nData);
            AfterStopServer(nData);
            //�رշ���
            FreeSystemObject(nData);
            //�ͷŶ���
          end;
        end;
      except
        on nErr: Exception do
        begin
          nStr := 'ģ��[ %s ]ִ��[ %s ]ʱ����,����: %s';
          WriteLog(Format(nStr, [ModuleInfo.FModuleName, Event2Str(nEvent),
            nErr.Message]));
          //xxxxx
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

end.
