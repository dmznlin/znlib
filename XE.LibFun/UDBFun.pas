{*******************************************************************************
  作者: dmzn@163.com 2021-05-16
  描述: 数据库通用业务
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
      {*雪花序号*}
  public
    class procedure Init; static;
    {*初始化*}
    class procedure WriteLog(const nEvent: string); static;
    {*记录日志*}
    class function ServerNow: TDateTime; static;
    {*服务器时间*}
    class function SerialID(const nGroup,nObject: string;
      const nUseDate: Boolean = True): string; static;
    {*串行编号*}
    class function SnowflakeID: string; static;
    {*雪花编号*}
  end;

implementation

uses
  UManagerGroup, UDBManager, ULibFun, USnowFlake;

const
  sTable_SerialBase   = 'Sys_SerialBase';            //编码种子

//Date: 2021-05-16
//Desc: 表结构描述和数据字典
procedure SystemTables(const nList: TList);
begin
  gDBManager.AddTable(sTable_SerialBase, nList).
    AddF('R_ID',        sField_SQLServer_AutoInc,    '记录编号').
    AddF('B_Group',     'varChar(15)',               '分组').
    AddF('B_Object',    'varChar(32)',               '对象').
    AddF('B_Prefix',    'varChar(25)',               '前缀').
    AddF('B_IDLen',     'Integer',                   '编号长').
    AddF('B_Base',      'Integer',                   '基数').
    AddF('B_Date',      'DateTime',                  '参考日期');
  //SerialBase
end;

//Date: 2021-05-16
//Desc: 初始化
class procedure TDBCommand.Init;
begin
  FInitSnowflake := False;
  gDBManager.AddTableBuilder(SystemTables);
end;

//Date: 2021-05-16
//Desc: 记录日志
class procedure TDBCommand.WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TDBCommand, '常用业务命令', nEvent);
end;

//Date: 2021-05-16
//Desc: 服务器当前时间
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
//Parm: 分组;对象;使用日期
//Desc: 生成nGroup.nObject对象的串行序列编号
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
          nStr := Format('没有[ %s.%s ]的编码配置.', [nGroup, nObject]);
          raise Exception.Create(nStr);
        end;

        nP := FieldByName('B_Prefix').AsString;
        nB := FieldByName('B_Base').AsString;
        nInt := FieldByName('B_IDLen').AsInteger;

        if nUseDate then //按日期编码
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
//Desc: 生成雪花序列号
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
