{*******************************************************************************
  ����: dmzn@163.com 2021-03-14
  ����: ���ݿ�ADO����
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
    {*������Ϣ*}
  end;

implementation

//Desc: ������Ϣ
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
  //ע������
end.
