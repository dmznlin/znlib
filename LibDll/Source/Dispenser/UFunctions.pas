{*******************************************************************************
  作者: dmzn@163.com 2019-01-02
  描述: 驱动输出函数
*******************************************************************************}
unit UFunctions;

interface

uses
  Windows, Classes, SysUtils, USysLoger, UMgrTTCEDispenser;

function dispenser_init(nFile: PChar; nDir: PChar): Boolean; stdcall;
procedure dispenser_free; stdcall;
//初始化
procedure dispenser_start; stdcall;
procedure dispenser_stop; stdcall;
//启停驱动
function dispenser_getcard(nID,nCard: PChar; const nWaitFor: Boolean = True;
  const nTimeout: Integer = cDispenser_Wait_Timeout): Boolean; stdcall;
//获取卡号
procedure dispenser_sendout(nID: PChar); stdcall;
//发卡
procedure dispenser_recovery(nID: PChar); stdcall;
//收卡

implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(TDispenserManager, '自助卡机管理器', nEvent);
end;

//Date: 2019-01-02
//Parm: 配置文件;日志路径
//Desc: 初始化驱动配置
function dispenser_init(nFile: PChar; nDir: PChar): Boolean;
begin
  Result := False;
  try
    if not Assigned(gSysLoger) then
      gSysLoger := TSysLoger.Create(nDir);
    gSysLoger.LogSync := False;

    if not Assigned(gDispenserManager) then
      gDispenserManager := TDispenserManager.Create;
    gDispenserManager.LoadConfig(nFile);
    Result := True;
  except
    on nErr: Exception do
    begin
      WriteLog(nErr.Message);
    end;
  end;
end;

//Date: 2019-01-02
//Desc: 释放驱动
procedure dispenser_free;
begin
  FreeAndNil(gDispenserManager);
  FreeAndNil(gSysLoger);
end;

//Date: 2019-01-02
//Desc: 启动
procedure dispenser_start; stdcall;
begin
  gDispenserManager.StartDispensers;
end;

//Date: 2019-01-02
//Desc: 停止
procedure dispenser_stop; stdcall;
begin
  gDispenserManager.StopDispensers;
end;

//Date: 2019-01-02
//Parm: 设备号;是否等待;超时时间
//Desc: 读取nID当前的卡号
function dispenser_getcard(nID,nCard: PChar; const nWaitFor: Boolean;
  const nTimeout: Integer): Boolean;
var nStr,nData: string;
begin
  nData := gDispenserManager.GetCardNo(nID, nStr, nWaitFor, nTimeout);
  Result := nData <> '';

  if Result then
    StrPCopy(nCard, nData);
  if nStr <> '' then WriteLog(nStr);
end;

//Date: 2019-01-02
//Parm: 设备号
//Desc: nID执行发卡
procedure dispenser_sendout(nID: PChar); stdcall;
var nStr: string;
begin
  gDispenserManager.SendCardOut(nID, nStr);
  if nStr <> '' then WriteLog(nStr);
end;

//Date: 2019-01-02
//Parm: 设备号
//Desc: nID执行收卡
procedure dispenser_recovery(nID: PChar);
var nStr: string;
begin
  gDispenserManager.RecoveryCard(nID, nStr);
  if nStr <> '' then WriteLog(nStr);
end;

end.
