{*******************************************************************************
  作者: dmzn@163.com 2021-03-14
  描述: 数据库ADO驱动
*******************************************************************************}
unit UDBDriverADO_V2;

interface

uses
  UDBManager;

type
  TDBDriverADO = class(TDBDriver)
  protected
  public
    class function DriverInfo: TDBDriverInfo; override;
    {*驱动信息*}
  end;

implementation

//Desc: 驱动信息
class function TDBDriverADO.DriverInfo: TDBDriverInfo;
begin
  with Result do
  begin
    DrvName    := 'DBDriver.ADO.V2';
    DrvAuthor  := 'dmzn@163.com';
    DrvVersion := '0.0.1';
  end;
end;

initialization
  TDBManager.RegistDriver(TDBDriverADO);
  //注册驱动
end.
