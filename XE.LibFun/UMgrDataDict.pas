{*******************************************************************************
  ����: dmzn@163.com 2021-06-08
  ����: �����ֵ������

  ��ע:
  &.�����ֵ���Ҫ���ڳ�ʼ��ListView,cxGrid�����ݱ��,�ֵ������ά����һ����֮
    ��ص���������.
  &.�ֵ����������: ����ģ��,ģ���¶��ʵ��,ÿ��ʵ���Ӧһ��������.
  &.�ֵ������ʹ��ProgID����,����ʶ��ǰ����ʵ���������ĳ���.
  &.�ֵ����������ݿ����,���ü�����,�������Ỻ��,����ÿ��ʵ������ֻ����һ��.
  &.��ȡʱ����LoadEntity,���ɹ����ʵ��ᱻ����,ֱ�Ӷ�ȡActiveEntity�Ϳ�����.
*******************************************************************************}
unit UMgrDataDict;

interface

uses
  System.Classes, System.SysUtils, Data.DB, UBaseObject;

type
  TDictFormatStyle = (fsNone, fsFixed, fsSQL, fsCheckBox);
  //��ʽ����ʽ: �̶�����,���ݿ�����

  PDictFormatItem = ^TDictFormatItem;
  TDictFormatItem = record
    FStyle    : TDictFormatStyle;                       //��ʽ
    FData     : string;                                 //����
    FFormat   : string;                                 //��ʽ��
    FExtMemo  : string;                                 //��չ����
  end;

  PDictDBItem = ^TDictDBItem;
  TDictDBItem = record
    FTable    : string;                                 //����
    FField    : string;                                 //�ֶ�
    FIsKey    : Boolean;                                //����

    FType     : TFieldType;                             //��������
    FWidth    : integer;                                //�ֶο��
    FDecimal  : integer;                                //С��λ
  end;

  TDictFooterKind = (fkNone, fkSum, fkMin, fkMax, fkCount, fkAverage);
  //ͳ������: ��,�ϼ�,��С,���,��Ŀ,ƽ��ֵ
  TDictFooterPosition = (fpNone, fpFooter, fpGroup, fpAll);
  //�ϼ�λ��: ҳ��,����,���߶���

  PDictGroupFooter = ^TDictGroupFooter;
  TDictGroupFooter = record
    FDisplay  : string;                                 //��ʾ�ı�
    FFormat   : string;                                 //��ʽ��
    FKind     : TDictFooterKind;                        //�ϼ�����
    FPosition : TDictFooterPosition;                    //�ϼ�λ��
  end;

  PDictItem = ^TDictItem;
  TDictItem = record
    FRecord   : integer;                                //��¼��
    FTitle    : string;                                 //����
    FAlign    : TAlignment;                             //����
    FWidth    : integer;                                //���
    FIndex    : integer;                                //˳��
    FVisible  : Boolean;                                //�ɼ�
    FDBItem   : TDictDBItem;                            //���ݿ�
    FFormat   : TDictFormatItem;                        //��ʽ��
    FFooter   : TDictGroupFooter;                       //ҳ�źϼ�
  end;
  TDictItems = array of TDictItem;

  PEntityItem = ^TEntityItem;
  TEntityItem = record
  private
    FEntity   : string;                                 //ʵ����
    FName     : string;                                 //ʵ������
    FLang     : string;                                 //���Ա�ʶ
    FItems    : TDictItems;                             //�ֵ�����(PDictItem)
  public
    function AddDict(const nTitle,nField: string): PEntityItem;
    {*����ֵ���*}
  end;

  TDcitItemBuilder = procedure (const nList: TList);
  //for external-system fill entity.items info

  TDataDictManager = class(TManagerBase)
  public
    const
      sTable_DataDict = 'Sys_DataDict';                     //�����ֵ�
  private
    FBuilders: array of TDcitItemBuilder;
    {*�ֵ�������Ϣ*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*�ӳ�ִ��*}
    procedure AddDictBuilder(const nBuilder: TDcitItemBuilder);
    function AddEntity(const nEntity,nName: string; nLang: string=''): PEntityItem;
    {*�������*}
    function BuilItemSQL(const nItem: PEntityItem): string;
    procedure InitItem(const nItem: PEntityItem);
    procedure AddItem(const nItem: PEntityItem);
    procedure DelItem(const nItem: PEntityItem);
    {*�ֵ���*}
    procedure GetEntity(const nEntity: string;
      const nData: PEntityItem; nLang: string = '');
    {*�ֵ�����*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*��ȡ״̬*}
  end;

var
  gDataDictManager: TDataDictManager = nil;
  //ȫ��ʹ��
  
implementation

uses
  UManagerGroup, UDBManager, ULibFun;

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TDataDictManager, '�ֵ������', nEvent);
end;

//Desc: ��ӹ����������
procedure AddDataDictTables(const nList: TList);
begin
  with gDBManager,TSQLBuilder,TDataDictManager do
  begin
    AddTable(sTable_DataDict, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '��¼��ʶ').
      AddF('D_Entity',      'varchar(32)',            '����ʵ��').
      AddF('D_Title',       'varchar(32)',            '���ݱ���').
      AddF('D_Align',       'smallint',               '�������').
      AddF('D_Width',       'integer',                '������').
      AddF('D_Index',       'integer',                '����˳��').
      AddF('D_Visible',     'smallint',               '�Ƿ�ɼ�').
      AddF('D_LangID',      'varchar(5)',             '���Ա�ʶ').
      //normal
      AddF('D_DBTable',     'varchar(32)',            '������').
      AddF('D_DBField',     'varchar(32)',            '�ֶ���').
      AddF('D_DBIsKey',     'smallint',               '�Ƿ�����').
      AddF('D_DBType',      'smallint',               '��������').
      AddF('D_DBWidth',     'smallint',               '�ֶο��').
      AddF('D_DBDecimal',   'smallint',               'С��λ').
      //database
      AddF('D_FmtStyle',    'smallint',               '��ʽ����ʽ').
      AddF('D_FmtData',     'varchar(200)',           '��ʽ������').
      AddF('D_FmtFormat',   'varchar(100)',           '��ʽ������').
      AddF('D_FmtExtMemo',  'varchar(100)',           '��ʽ����չ').
      //format
      AddF('D_FteDisplay',  'varChar(50)',            'ͳ����ʾ�ı�').
      AddF('D_FteFormat',   'varChar(50)',            'ͳ�Ƹ�ʽ��').
      AddF('D_FteKind',     'smallint',               'ͳ������').
      AddF('D_FtePositon',  'smallint',               'ͳ����ʾλ��').
      //footer
      AddI('idx_entity', 'CREATE INDEX $IDX ON $TBS(D_Entity ASC,D_LangID ASC)');
      //for index;
  end;
end;

function TEntityItem.AddDict(const nTitle, nField: string): PEntityItem;
begin

end;

//------------------------------------------------------------------------------
constructor TDataDictManager.Create;
begin
  inherited;
  //
end;

destructor TDataDictManager.Destroy;
begin
  //call RunBeforUnregistAllManager
  inherited;
end;

//Date: 2021-06-08
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TDataDictManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TDataDictManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TDataDictManager.Create;
    gMG.FDataDictManager := gMG.FManagers[nIdx].FManager as TDataDictManager;
  end else
  begin
    gMG.FDataDictManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gDataDictManager := gMG.FDataDictManager;
  //����ȫ�ֱ���
end;

procedure TDataDictManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TDataDictManager', ['TDBManager']);
  //���֧��
  gDBManager.AddTableBuilder(AddDataDictTables);
end;

procedure TDataDictManager.RunBeforUnregistAllManager;
begin

end;

procedure TDataDictManager.AddDictBuilder(const nBuilder: TDcitItemBuilder);
begin

end;

function TDataDictManager.AddEntity(const nEntity, nName: string;
  nLang: string): PEntityItem;
begin

end;

function TDataDictManager.BuilItemSQL(const nItem: PEntityItem): string;
begin

end;

procedure TDataDictManager.AddItem(const nItem: PEntityItem);
begin

end;

procedure TDataDictManager.DelItem(const nItem: PEntityItem);
begin

end;

//Date: 2021-06-08
//Parm: �б�;�Ƿ��Ѻ���ʾ
//Desc: ��������״̬���ݴ���nList
procedure TDataDictManager.GetEntity(const nEntity: string;
  const nData: PEntityItem; nLang: string);
begin

end;

procedure TDataDictManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
begin
    with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      //nList.Add('NumAll=' + FStatus.FNumAll.ToString);
      Exit;
    end;

    //nList.Add(FixData('NumAll:', FStatus.FNumAll));
  finally
    SyncLeave;
  end;
end;

procedure TDataDictManager.InitItem(const nItem: PEntityItem);
begin

end;

end.
