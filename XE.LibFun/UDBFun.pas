{*******************************************************************************
  ����: dmzn@163.com 2021-05-16
  ����: ���ݿ�ͨ��ҵ��
*******************************************************************************}
unit UDBFun;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles, Data.DB;

type
  TDBCommand = class
  strict private
    class var
      FInitSnowflake: Boolean;
      {*ѩ�����*}
  public
    class procedure Init; static;
    {*��ʼ��*}
    class procedure WriteLog(const nEvent: string); static;
    {*��¼��־*}
    class function ServerNow: TDateTime; static;
    {*������ʱ��*}
    class function SerialID(const nGroup,nObject: string;
      const nUseDate: Boolean = True): string; static;
    {*���б��*}
    class function SnowflakeID: string; static;
    {*ѩ�����*}
  end;

implementation

uses
  UManagerGroup, UDBManager, ULibFun, USnowFlake;

const
  sTable_SerialBase   = 'Sys_SerialBase';            //��������

//Date: 2021-05-16
//Desc: ��ṹ�����������ֵ�
procedure SystemTables(const nList: TList);
begin
  gDBManager.AddTable(sTable_SerialBase, nList).
    AddF('R_ID',        sField_SQLServer_AutoInc,    '��¼���').
    AddF('B_Group',     'varChar(15)',               '����').
    AddF('B_Object',    'varChar(32)',               '����').
    AddF('B_Prefix',    'varChar(25)',               'ǰ׺').
    AddF('B_IDLen',     'Integer',                   '��ų�').
    AddF('B_Base',      'Integer',                   '����').
    AddF('B_Date',      'DateTime',                  '�ο�����');
  //SerialBase
end;

//Date: 2021-05-16
//Desc: ��ʼ��
class procedure TDBCommand.Init;
begin
  FInitSnowflake := False;
  gDBManager.AddTableBuilder(SystemTables);
end;

//Date: 2021-05-16
//Desc: ��¼��־
class procedure TDBCommand.WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TDBCommand, '����ҵ������', nEvent);
end;

//Date: 2021-05-16
//Desc: ��������ǰʱ��
class function TDBCommand.ServerNow: TDateTime;
var nStr: string;
    nDS: TDataSet;
begin
  nDS := nil;
  try
    Result := Now();
    nStr := '';

    case gDBManager.DefaultFit of
     dtMSSQL   : nStr := 'Select ' + sField_SQLServer_Now;
     dtMySQL   : nStr := '';
     dtOracle  : nStr := '';
     dtPostgre : nStr := '';
    end;

    if nStr <> '' then
    begin
      nDS := gDBManager.DBQuery(nStr);
      Result := nDS.Fields[0].AsDateTime;
    end;
  finally
    gDBManager.ReleaseDBQuery(nDS);
  end;
end;

//Date: 2021-05-16
//Parm: ����;����;ʹ������
//Desc: ����nGroup.nObject����Ĵ������б��
class function TDBCommand.SerialID(const nGroup, nObject: string;
  const nUseDate: Boolean): string;
var nStr,nP,nB: string;
    nInt: Integer;
    nConn: TDataSet;
    nCtx: TDBTransContext;
begin
  Result := '';
  nConn := nil;

  with gMG.FDBManager do
  try
    nConn := LockDBQuery();
    nCtx := BeginTrans(nConn);
    try
      nStr := 'Update %s Set B_Base=B_Base+1 ' +
              'Where B_Group=''%s'' And B_Object=''%s''';
      nStr := Format(nStr, [sTable_SerialBase, nGroup, nObject]);
      DBExecute(nStr, nConn);

      nStr := 'Select B_Prefix,B_IDLen,B_Base,B_Date,%s as B_Now From %s ' +
              'Where B_Group=''%s'' And B_Object=''%s''';
      nStr := Format(nStr, [sField_SQLServer_Now, sTable_SerialBase,
              nGroup, nObject]);
      DBQuery(nStr, nConn);

      with nConn,TDateTimeHelper do
      begin
        if RecordCount < 1 then
        begin
          nStr := Format('û��[ %s.%s ]�ı�������.', [nGroup, nObject]);
          raise Exception.Create(nStr);
        end;

        nP := FieldByName('B_Prefix').AsString;
        nB := FieldByName('B_Base').AsString;
        nInt := FieldByName('B_IDLen').AsInteger;

        if nUseDate then //�����ڱ���
        begin
          nStr := Date2Str(FieldByName('B_Date').AsDateTime, False);
          //old date

          if (nStr <> Date2Str(FieldByName('B_Now').AsDateTime, False)) and
             (FieldByName('B_Now').AsDateTime >
              FieldByName('B_Date').AsDateTime) then
          begin
            nStr := 'Update %s Set B_Base=1,B_Date=%s ' +
                    'Where B_Group=''%s'' And B_Object=''%s''';
            nStr := Format(nStr, [sTable_SerialBase, sField_SQLServer_Now,
                    nGroup, nObject]);
            gDBManager.DBExecute(nStr);

            nB := '1';
            nStr := Date2Str(FieldByName('B_Now').AsDateTime, False);
            //now date
          end;

          System.Delete(nStr, 1, 2);
          //yymmdd
          nInt := nInt - Length(nP) - Length(nStr) - Length(nB);
          Result := nP + nStr + StringOfChar('0', nInt) + nB;
        end else
        begin
          nInt := nInt - Length(nP) - Length(nB);
          nStr := StringOfChar('0', nInt);
          Result := nP + nStr + nB;
        end;
      end;

      nCtx.CommitTrans;
    except on nErr: Exception do
      begin
        TDBCommand.WriteLog(nErr.Message);
        nCtx.RollbackTrans;
        raise;
      end;
    end;
  finally
    gMG.FDBManager.ReleaseDBQuery(nConn);
  end;
end;

//Date: 2021-05-16
//Desc: ����ѩ�����к�
class function TDBCommand.SnowflakeID: string;
var nIni: TIniFile;
begin
  if not FInitSnowflake then
  begin
    FInitSnowflake := True;
    nIni := nil;

    if FileExists(TApplicationHelper.gSysConfig) then
    try
      nIni := TIniFile.Create(TApplicationHelper.gSysConfig);
      with IdGenerator do
      begin
        WorkerID := nIni.ReadInteger('Snowflake', 'WorkerID', 1);
        DatacenterId := nIni.ReadInteger('Snowflake', 'CenterID', 1);;
      end;
    finally
      nIni.Free;
    end;
  end;

  try
    Result := IntToHex(IdGenerator.NextId);
    //64-Byte,8-byte,16-Hex
  except
    on nErr: Exception do
    begin
      WriteLog(nErr.Message);
      Result := TDateTimeHelper.DateTimeSerial();
    end;
  end;
end;

initialization
  TDBCommand.Init;
finalization
  //null
end.
