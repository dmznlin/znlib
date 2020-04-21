{*******************************************************************************
  ����: dmzn@163.com 2020-04-21
  ����: �˵�������

  ��ע:
  *.������֧��.
  *.������ά���˵���,��֤ϵͳ�˵�����.ԭ��ʹ�ò˵��༭���ķ���,�ᵼ�²������.
  *.����ProgID: �������ֲ˵�������ϵͳ.
  *.ʵ��Entity: ��������ͬϵͳ�ڵĲ�ͬ�˵�,�������˵��Ϳ�ݲ˵�.
*******************************************************************************}
unit UMenuManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, UBaseObject;

type
  TMenuItemType = (mtProg, mtEntity, mtParent, mtItem, mtAssist);
  //menu item
const
  cMenuItemType: array[TMenuItemType] of string = ('����', 'ʵ��',
    '���˵�', '�˵���', '������');
  //menu item desc

type
  TMenuData = record
    FName         : string;                              //����
    FData         : string;                              //����
    FFlag         : string;                              //��ʶ,��:LangID
  end;
  TMenuDatas = array of TMenuData;                       //�����б�

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    FType         : TMenuItemType;                       //�˵�����
    FProgID       : string;                              //�����ʶ
    FEntity       : string;                              //ʵ���ʶ
    FMenuID       : string;                              //�˵���ʶ
    FPMenu        : string;                              //�ϼ��˵�
    FTitle        : TMenuDatas;                          //�˵�����
    FImgIndex     : integer;                             //ͼ������
    FFlag         : string;                              //���Ӳ���(�»���..)
    FAction       : string;                              //�˵�����
    FFilter       : string;                              //��������
    FNewOrder     : Single;                              //��������
    FPopedom      : string;                              //Ȩ����
    FSubMenu      : TList;                               //�Ӳ˵��б�
  end;

  TMenuManager = class(TManagerBase)
  private

  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    {*�ӳ�ִ��*}
  end;

var
  gMenuManager: TMenuManager = nil;
  //ȫ��ʹ��

implementation

uses
  ULibFun, UManagerGroup;

const
  sTableMenu = 'Sys_Menu';

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TMenuManager, '�˵�������', nEvent);
end;

//------------------------------------------------------------------------------
constructor TMenuManager.Create;
begin

end;

destructor TMenuManager.Destroy;
begin

  inherited;
end;

//Date: 2020-04-21
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TMenuManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TMenuManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TMenuManager.Create;
    gMG.FMenuManager := gMG.FManagers[nIdx].FManager as TMenuManager;
  end else
  begin
    gMG.FMenuManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gMenuManager := gMG.FMenuManager;
  //����ȫ�ֱ���
end;

//Desc: ��ӹ����������
procedure AddMenuTables(const nList: TList);
begin
  gMG.FDBManager.AddTable(sTableMenu, nList).
  AddF('M_MenuID',      'varchar(15)',        '�˵���ʶ').
  AddF('M_ProgID',      'varchar(15)',        '�����ʶ').
  AddF('M_Entity',      'varchar(15)',        'ʵ���ʶ').
  AddF('M_PMenu',       'varchar(15)',        '�ϼ��˵�').
  AddF('M_Title',       'varchar(50)',        '�˵�����').
  AddF('M_ImgIndex',    'integer default 0',  'ͼ������', '0').
  AddF('M_Flag',        'varchar(20)',        '���Ӳ���').
  AddF('M_Action',      'varchar(100)',       '�˵�����').
  AddF('M_Filter',      'varchar(100)',       '��������').
  AddF('M_Popedom',     'varchar(36)',        'Ȩ����').
  AddF('M_NewOrder',    'float default -1',   '��������', '-1');
end;

procedure TMenuManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TMenuManager', ['TDBManager']);
  //���֧��
  gMG.FDBManager.AddSystemData(AddMenuTables);
end;

end.
