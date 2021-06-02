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
    const
      sValidBefore      = '2099-12-31';             //��Ч��
      sTable_Users      = 'Sys_Users';              //ϵͳ�û�
      sTable_SerialBase = 'Sys_SerialBase';         //��������

    type
      PUserData = ^TUserData;
      TUserData = record
        FRecordID       : string;                   //��¼���
        FUserID         : string;                   //�û����
        FUserName       : string;                   //�û�����
        FAccount        : string;                   //�û��ʻ�(��¼��)
        FPassword       : string;                   //�û�����
        FEncryptKey     : string;                   //��̬��Կ
        FDynamicPwd     : Boolean;                  //��̬����
        FLangID         : string;                   //���Ա��
        FGroupID        : string;                   //���ڷ���
        FIsAdmin        : Boolean;                  //�ǹ���Ա
        FMail           : string;
        FPhone          : string;                   //��ϵ��ʽ
        FMemo           : string;                   //��ע��Ϣ
        FValidOn        : TDateTime;                //��Ч����
      end;
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
    class function ValidID(const nID: string): Boolean; static;
    {*��Ч��ʶ*}
    class procedure InitUser(var nUser: TUserData); static;
    class function GetUser(const nAccount: string;
      var nUser: TUserData): string; static;
    class function AddUser(const nUser: PUserData): string; static;
    class function DelUser(const nAccount: string): string; static;
    {*�û�����*}
  end;

implementation

uses
  UManagerGroup, UDBManager, ULibFun, USnowFlake;

//Date: 2021-05-16
//Desc: ��ṹ�����������ֵ�
procedure SystemTables(const nList: TList);
begin
  with TSQLBuilder,TEncodeHelper,TApplicationHelper,TDBCommand do
  begin
    gDBManager.AddTable(sTable_Users, nList, dtMSSQL).
      AddF('R_ID',        sField_SQLServer_AutoInc, '��¼���').
      AddF('U_ID',        'varChar(32)',            '�û���ʶ').
      AddF('U_Name',      'varChar(32)',            '�û�����').
      AddF('U_Account',   'varChar(32)',            '��¼�˻�').
      AddF('U_Password',  'varChar(32)',            '��¼����').
      AddF('U_Encrypt',   'varChar(32)',            '��̬��Կ').
      AddF('U_Phone',     'varChar(15)',            '�绰����').
      AddF('U_Mail',      'varChar(50)',            'ͨ������').
      AddF('U_Lang',      'varChar(5)',             '���Ա�ʶ').
      AddF('U_DynPwd',    'Char(1)',                '��̬����', SQM(sFlag_No)).
      AddF('U_Admin',     'Char(1)',                '�ǹ���Ա', SQM(sFlag_No)).
      AddF('U_Group',     'varChar(32)',            '�����ʶ').
      AddF('U_Memo',      'varChar(50)',            '��ע��Ϣ').
      AddF('U_CreateOn',  'DateTime',               '��������').
      AddF('U_ValidOn',   'DateTime',               '��Ч����', SQM(sValidBefore)).
      //for field
      AddI('idx_id',      'Create Index $idx ON $TBS(U_ID,U_Account ASC)').
      AddI('idx_account', 'Create Index $idx ON $TBS(U_Account ASC)').
      //for index
      AddR('admin', MakeSQLByStr([
        SF('U_ID',        TDBCommand.SnowflakeID()),
        SF('U_Name',      '����Ա'),
        SF('U_Account',   'admin'),
        SF('U_Password',  Encode_3DES('admin', sDefaultKey)),
        SF('U_Encrypt',   Encode_3DES(sDefaultKey, sDefaultKey)),
        SF('U_Lang',      'cn'),
        SF('U_Mail',      'dmzn@163.com'),
        SF('U_DynPwd',    'Y'),
        SF('U_Admin',     'Y'),
        SF('U_CreateOn',  sField_SQLServer_Now, sfVal)
      ], sTable_Users));
    //Users

    gDBManager.AddTable(sTable_SerialBase, nList, dtMSSQL).
      AddF('R_ID',        sField_SQLServer_AutoInc,    '��¼���').
      AddF('B_Group',     'varChar(15)',               '����').
      AddF('B_Object',    'varChar(32)',               '����').
      AddF('B_Prefix',    'varChar(25)',               'ǰ׺').
      AddF('B_IDLen',     'Integer',                   '��ų�').
      AddF('B_Base',      'Integer',                   '����').
      AddF('B_Date',      'DateTime',                  '�ο�����');
    //SerialBase
  end;
end;

//Date: 2021-05-16
//Desc: ��ʼ��
class procedure TDBCommand.Init;
begin
  FInitSnowflake := False;
  gMG.RunAfterRegistAllManager( procedure
    begin gDBManager.AddTableBuilder(SystemTables); end);
  //delay run
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

//Date: 2021-05-23
//Parm: ��ʶ
//Desc: ��֤nID�Ƿ�Ϸ���Ч
class function TDBCommand.ValidID(const nID: string): Boolean;
var nIdx: Integer;
begin
  for nIdx := Length(nID) downto 1 do
  begin
    Result := nID[nIdx] in ['0'..'9', 'a'..'z', 'A'..'Z', '_', '@'];
    if not Result then Exit;
  end;
end;

//Date: 2021-05-23
//Parm: �û�����
//Desc: ��ʼ��nData����
class procedure TDBCommand.InitUser(var nUser: TUserData);
var nInit: TUserData;
begin
  FillChar(nInit, SizeOf(TUserData), #0);
  nUser := nInit;
  nUser.FValidOn := TDateTimeHelper.Str2Date(sValidBefore);
end;

//Date: 2021-05-23
//Parm: �û�����
//Desc: ����û�
class function TDBCommand.AddUser(const nUser: PUserData): string;
var nStr: string;
    nQuery: TDataSet;
begin
  if nUser.FAccount = '' then
  begin
    Result := '�û��ʺ�(Account)����Ϊ��';
    Exit;
  end;

  if not ValidID(nUser.FAccount) then
  begin
    Result := '�û��ʻ�(Account)��ʹ��: ����,��ĸ,�»���';
    Exit;
  end;

  Result := '';
  nQuery := nil;
  try
    nStr := 'Select R_ID, U_Name From %s Where U_Account=''%s''';
    nStr := Format(nStr, [sTable_Users, nUser.FAccount]);
    nQuery := gMG.FDBManager.DBQuery(nStr);

    if nQuery.RecordCount > 0 then
    begin
      Result := Format('�˺�[ %s ]�ѱ��û�[ %s.%s ]ʹ��', [nUser.FAccount,
        nQuery.FieldByName('R_ID').AsString,
        nQuery.FieldByName('U_Name').AsString]);
      Exit;
    end;

    if nUser.FUserID = '' then
      nUser.FUserID := SnowflakeID();
    //xxxxx

    if nUser.FPassword = '' then
      nUser.FPassword := '123';
    //xxxxx

    if nUser.FLangID = '' then
      nUser.FLangID := 'cn';
    //xxxxx

    with TSQLBuilder,TEncodeHelper,TApplicationHelper do
    nStr := MakeSQLByStr([
      SF('U_ID',       nUser.FUserID),
      SF('U_Name',     nUser.FUserName),
      SF('U_Account',  nUser.FAccount),
      SF('U_Password', Encode_3DES(nUser.FPassword, sDefaultKey)),
      SF('U_Encrypt',  Encode_3DES(nUser.FEncryptKey, sDefaultKey)),
      SF('U_Phone',    nUser.FPhone),
      SF('U_Mail',     nUser.FMail),
      SF('U_Lang',     nUser.FLangID),
      SF('U_Group',    nUser.FGroupID),
      SF('U_Memo',     nUser.FMemo),
      SF('U_CreateOn', sField_SQLServer_Now, sfVal),
      SF('U_ValidOn',  TDateTimeHelper.Date2Str(nUser.FValidOn)),

      SF_IF([SF('U_DynPwd', sFlag_Yes),
             SF('U_DynPwd', sFlag_No)], nUser.FDynamicPwd),
      SF_IF([SF('U_Admin', sFlag_Yes),
             SF('U_Admin', sFlag_No)], nUser.FIsAdmin)
      ], sTable_Users);
    //insert sql

    gMG.FDBManager.DBExecute(nStr, nQuery);
  finally
    gMG.FDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2021-05-23
//Parm: �û��ʻ�
//Desc: ɾ��nAccount�ʻ����û�
class function TDBCommand.DelUser(const nAccount: string): string;
var nStr: string;
begin
  if not ValidID(nAccount) then
  begin
    Result := Format('�û��ʻ�[ %s ]��Ч', [nAccount]);
    Exit;
  end;

  Result := '';
  nStr := 'Delete From %s Where U_Account=''%s''';
  nStr := Format(nStr, [sTable_Users, nAccount]);
  gMG.FDBManager.DBExecute(nStr);
end;

//Date: 2021-05-23
//Parm: �û��ʻ�
//Desc: ��ȡnID��Ӧ���˺���Ϣ
class function TDBCommand.GetUser(const nAccount: string;
  var nUser: TUserData): string;
var nStr: string;
    nQuery: TDataSet;
begin
  if not ValidID(nAccount) then
  begin
    Result := Format('�û��ʻ�[ %s ]��Ч', [nAccount]);
    Exit;
  end;

  nQuery := nil;
  try
    nStr := 'Select * From %s Where U_Account=''%s''';
    nStr := Format(nStr, [sTable_Users, nAccount]);
    nQuery := gMG.FDBManager.DBQuery(nStr);

    with nQuery,TEncodeHelper,TApplicationHelper do
    begin
      if RecordCount < 1 then
      begin
        Result := Format('�û��ʻ�[ %s ]������', [nAccount]);
        Exit;
      end;

      if RecordCount > 1 then
      begin
        Result := Format('�û��ʻ�[ %s ]�ѽ���(�ظ�)', [nAccount]);
        Exit;
      end;

      with nUser do
      begin
        FRecordID     := FieldByName('R_ID').AsString;
        FUserID       := FieldByName('U_ID').AsString;
        FUserName     := FieldByName('U_Name').AsString;
        FAccount      := FieldByName('U_Account').AsString;
        FPassword     := FieldByName('U_Password').AsString;
        FEncryptKey   := FieldByName('U_Encrypt').AsString;
        FDynamicPwd   := FieldByName('U_DynPwd').AsString = sFlag_Yes;
        FLangID       := FieldByName('U_Lang').AsString;
        FGroupID      := FieldByName('U_Group').AsString;
        FValidOn      := FieldByName('U_ValidOn').AsDateTime;
        FIsAdmin      := FieldByName('U_Admin').AsString =  sFlag_Yes;
        FMail         := FieldByName('U_Mail').AsString;
        FPhone        := FieldByName('U_Phone').AsString;
        FMemo         := FieldByName('U_Memo').AsString;

        FPassword     := Decode_3DES(FPassword, sDefaultKey);
        FEncryptKey   := Decode_3DES(FEncryptKey, sDefaultKey);
      end;
    end;
  finally
    gMG.FDBManager.ReleaseDBQuery(nQuery);
  end;
end;

initialization
  TDBCommand.Init;
finalization
  //null
end.
