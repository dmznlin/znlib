{*******************************************************************************
  ����: dmzn@163.com 2017-03-21
  ����: ʹ�Ѿ������Ķ�������ظ�ʹ�õĶ����

  ��ע: 
  *.�̰߳�ȫ.
  *.TObjectPoolManager.Lock��Release�������ʹ��.
*******************************************************************************}
unit UObjectPool;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, UBaseObject, ULibFun;

type
  TObjectNewOne = reference to function(var nData: Pointer): TObject;
  TObjectFreeOne = reference to procedure(const nObject: TObject;
    const nData: Pointer);
  //���󴴽��ͷ�
  TObjectResetOne = reference to procedure(const nObject: TObject;
    const nData: Pointer);
  //������������

  PObjectPoolItem = ^TObjectPoolItem;
  TObjectPoolItem = record
    FObject  : TObject;                 //����
    FData    : Pointer;                 //����
    FUsed    : Boolean;                 //ʹ����
    FUsedNum : Cardinal;                //ʹ�ü���
  public
    procedure Init(const nObject: TObject = nil);
    {*��ʼ��*}
  end;

  PObjectPoolClass = ^TObjectPoolClass;
  TObjectPoolClass = record
    FClass      : TClass;               //����
    FNewOne     : TObjectNewOne;        //����
    FFreeOne    : TObjectFreeOne;       //�ͷ�
    FResetOne   : TObjectResetOne;      //����

    FNumLocked  : Integer;              //������
    FNumLockAll : Int64;                //�������
    FItems      : TList;                //�����б�
  public
    procedure Init(const nClass: TClass = nil);
    {*��ʼ��*}
  end;

  TObjectLockFilter = reference to function(const nObject: TObject;
    const nData: Pointer; var nTimes: Integer; const nUsed: Boolean): Boolean;
  //����ʱɸѡ

  TObjectPoolManager = class(TManagerBase)
  private
    FPool: array of TObjectPoolClass;
    //�����
    FNumLocked: Integer;
    FNumLockAll: Int64;
    //��������
    FSrvClosed: Integer;
    //����ر� 
  protected
    procedure ClearPool(const nFree: Boolean);
    //������Դ
    function FindPool(const nClass: TClass): Integer;
    //��������
  public
    constructor Create;
    destructor Destroy; override;
    //�����ͷ�
    class procedure RegistMe(const nReg: Boolean); override;
    //ע�������
    function IsExists(const nClass: TClass): Boolean;
    //�Ƿ�ע��
    function NewClass(const nClass: TClass; const nNew: TObjectNewOne;
      const nFree: TObjectFreeOne = nil;
      const nReset: TObjectResetOne = nil;
      const nOnlyOnce: Boolean = True): Integer;
    procedure NewNormalClass;
    //ע������
    function Lock(const nClass: TClass; nNew: TObjectNewOne = nil;
      const nData: PPointer = nil;
      const nFilter: TObjectLockFilter = nil; nFilterAll: Boolean = False;
      const nLockNil: Boolean = False): TObject;
    procedure Release(const nObject: TObject; const nReset: Boolean = True);
    //�����ͷ�
    function GetData(const nClass: TClass; const nObj: TObject): Pointer;
    function SetData(const nClass: TClass; const nObj: TObject;
      const nData: Pointer): Boolean;
    //��չ����
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    function GetHealth(const nList: TStrings = nil): TObjectHealth; override;
    //��ȡ״̬
  end;

var
  gObjectPoolManager: TObjectPoolManager = nil;
  //ȫ��ʹ��
  
implementation

uses
  UManagerGroup;
  
const
  cYes  = $0002;
  cNo   = $0005;

//Date: 2021-07-12
//Parm: ����
//Desc: ��ʼ������ض���
procedure TObjectPoolItem.Init(const nObject: TObject);
var nInit: TObjectPoolItem;
begin
  FillChar(nInit, SizeOf(TObjectPoolItem), #0);
  Self := nInit;

  if Assigned(nObject) then
    FObject := nObject;
  //xxxxx
end;

//Date: 2021-07-12
//Parm: ������
//Desc: ��ʼ���������
procedure TObjectPoolClass.Init(const nClass: TClass);
var nInit: TObjectPoolClass;
begin
  FillChar(nInit, SizeOf(TObjectPoolClass), #0);
  Self := nInit;

  if Assigned(nClass) then
    FClass := nClass;
  //xxxxx
end;

//------------------------------------------------------------------------------
constructor TObjectPoolManager.Create;
begin
  inherited;
  FNumLocked := 0;
  FSrvClosed := cNo;
  
  NewNormalClass;
  //reg normal 
end;

destructor TObjectPoolManager.Destroy;
var nInit: Int64;
    nList: TStrings;
begin
  SyncEnter;
  FSrvClosed := cYes; //set close flag  
  SyncLeave;

  if FNumLocked > 0 then
  begin
    nList := nil;
    nInit := TDateTimeHelper.GetTickCount(); //init

    while FNumLocked > 0 do
    begin
      Sleep(1);
      //wait for relese

      if (nList = nil) and
         (TDateTimeHelper.GetTickCountDiff(nInit) > 10 * 1000) then //10s
      try
        nList := TStringList.Create;
        GetStatus(nList);
        //list objects status

        gMG.WriteLog(TObjectPoolManager, '���������',
          '����û����ȫ�ͷ�,��������:' + #13#10 + nList.Text);
        Break;
      finally
        nList.Free;
      end;
    end;
  end;
    
  ClearPool(True);
  inherited;
end;

//Date: 2017-03-23
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TObjectPoolManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TObjectPoolManager);
  if nReg then
  begin     
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TObjectPoolManager.Create;
    gMG.FObjectPool := gMG.FManagers[nIdx].FManager as TObjectPoolManager;
  end else
  begin
    gMG.FObjectPool := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;
end;

//Desc: ��������
procedure TObjectPoolManager.ClearPool(const nFree: Boolean);
var nIdx,i: Integer;
    nItem: PObjectPoolItem;
begin
  for nIdx := Low(FPool) to High(FPool) do
  with FPool[nIdx] do
  begin
    if Assigned(FItems) then
    begin
      for i := FItems.Count - 1 downto 0 do
      begin
        nItem := FItems[i];
        if Assigned(FFreeOne) then
        begin
          FFreeOne(nItem.FObject, nItem.FData);
          nItem.FObject := nil;
          nItem.FData := nil;
        end else
        begin
          FreeAndNil(nItem.FObject);
          //free default
        end;

        Dispose(nItem);
        FItems.Delete(i);
      end;

      FreeAndNil(FItems);
    end;
  end;

  if nFree then  
    SetLength(FPool, 0);
  //clear all
end;

//Date: 2017-03-23
//Parm: ���� 
//Desc: ����nClass�ڶ�����е�λ��
function TObjectPoolManager.FindPool(const nClass: TClass): Integer;
var nIdx: Integer;
begin
  Result := -1;

  for nIdx := Low(FPool) to High(FPool) do
  if FPool[nIdx].FClass = nClass then
  begin
    Result := nIdx;
    Exit;
  end;
end;

//Date: 2019-01-22
//Parm: ����
//Desc: ���nClass�Ƿ���ע��
function TObjectPoolManager.IsExists(const nClass: TClass): Boolean;
begin
  SyncEnter;
  try
    Result := not (FindPool(nClass) < 0);
  finally
    SyncLeave;
  end;
end;

//Date: 2017-03-23
//Parm: ����;��������;�ͷŷ���;���÷���;������,�Ƿ񸲸�
//Desc: ע��nClass�ൽ�����
function TObjectPoolManager.NewClass(const nClass: TClass;
  const nNew: TObjectNewOne; const nFree: TObjectFreeOne;
  const nReset: TObjectResetOne; const nOnlyOnce: Boolean): Integer;
begin
  SyncEnter;
  try
    Result := FindPool(nClass);
    if Result < 0 then
    begin
      Result := Length(FPool);
      SetLength(FPool, Result + 1);
      FPool[Result].Init();
    end else

    if nOnlyOnce then
    begin
      Exit;
      //��������ע��,Ĭ�ϲ�����
    end;

    with FPool[Result] do
    begin
      FClass := nClass;
      FNewOne := nNew;
      FFreeOne := nFree;
      FResetOne := nReset;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2017-03-23
//Desc: ע�᳣���� 
procedure TObjectPoolManager.NewNormalClass;
var nNewOne: TObjectNewOne;
    nResetOne: TObjectResetOne;
begin
  nNewOne :=
   function(var nData: Pointer):TObject begin Result := TStringList.Create; end;
  //create

  nResetOne := procedure(const nObject: TObject; const nData: Pointer)
    begin TStringList(nObject).Clear; end;
  //reset

  NewClass(TStrings, nNewOne, nil, nResetOne);
  NewClass(TStringList, nNewOne, nil, nResetOne);

  //----------------------------------------------------------------------------
  nNewOne :=
   function(var nData: Pointer):TObject begin Result := TList.Create; end;
  //xxxxx

  nResetOne := procedure(const nObject: TObject; const nData: Pointer)
    begin TList(nObject).Clear; end;
  //reset
  NewClass(TList, nNewOne, nil, nResetOne);
end;

//Date: 2017-03-23
//Parm: ��������;��������;��չ����;ɸѡ����;ɸѡȫ������;������
//Desc: ����nClass�Ķ���ָ��
function TObjectPoolManager.Lock(const nClass:TClass; nNew:TObjectNewOne;
  const nData: PPointer; const nFilter: TObjectLockFilter; nFilterAll: Boolean;
  const nLockNil: Boolean): TObject;
var nIdx,i,nVal,nRepeat,nTimes: Integer;
    nItem: PObjectPoolItem;
begin
  SyncEnter;
  try    
    Result := nil;
    if FSrvClosed = cYes then
      raise Exception.Create(ClassName + ': Not Support "Lock" When Closing.');
    //pool will close

    nIdx := FindPool(nClass);
    if (not Assigned(nNew)) and ((nIdx < 0) or 
       (not Assigned(FPool[nIdx].FNewOne))) then
      raise Exception.Create(ClassName + ': Lock Object Need "Create" Method.');
    //xxxxx

    if nFilterAll and (not Assigned(nFilter)) then
      nFilterAll := False;
    //reset filterAll when no filter

    if nIdx < 0 then
      nIdx := NewClass(nClass, nNew);
    //xxxxx

    with FPool[nIdx] do
    begin
      if not Assigned(FItems) then
        FItems := TList.Create;
      //xxxxx

      if not Assigned(nNew) then
        nNew := FNewOne;
      //xxxxx

      nTimes := 0;
      nRepeat := 1;
      //default once

      while nRepeat > nTimes do
      begin
        Inc(nTimes);
        //counter

        for i := FItems.Count - 1 downto 0 do
        begin
          nItem := FItems[i];
          nVal := nTimes;

          if (nFilterAll or (not nItem.FUsed)) and ((not Assigned(nFilter)) or
              nFilter(nItem.FObject, nItem.FData, nVal, nItem.FUsed)) then
          begin
            Result := nItem.FObject;
            if nItem.FUsed then
            begin
              Inc(nItem.FUsedNum);
              //���¼���
            end else
            begin
              nItem.FUsed := True;
              nItem.FUsedNum := 1;
            end;

            if Assigned(nData) then
              nData^ := nItem.FData;
            Break;
          end;

          if nVal > nTimes then
           nRepeat := nVal;
          //������ѯ����
        end;

        if Assigned(Result) then Break;
        //����ѯ���
      end;

      if not (Assigned(Result) or nLockNil) then
      begin
        New(nItem);
        FItems.Add(nItem);
        nItem.Init();
        
        nItem.FObject := nNew(nItem.FData);
        Result := nItem.FObject;
        nItem.FUsed := True;
        nItem.FUsedNum := 1;

        if Assigned(nData) then
          nData^ := nItem.FData;
        //xxxxx
      end;
    end;

    if Assigned(Result) then
    begin
      Inc(FPool[nIdx].FNumLocked);
      Inc(Self.FNumLocked);
      //inc counter

      if FPool[nIdx].FNumLockAll < High(Int64) then
           Inc(FPool[nIdx].FNumLockAll)
      else FPool[nIdx].FNumLockAll := 0;

      if Self.FNumLockAll < High(Int64) then
           Inc(Self.FNumLockAll)
      else Self.FNumLockAll := 0;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2017-03-23
//Parm: ����;����
//Desc: �ͷŶ���
procedure TObjectPoolManager.Release(const nObject: TObject;
  const nReset: Boolean);
var nIdx,i: Integer;
    nItem: PObjectPoolItem; 
begin
  if not Assigned(nObject) then Exit;
  //nothing
  
  SyncEnter;
  try     
    for nIdx := Low(FPool) to High(FPool) do
    with FPool[nIdx] do
    begin
      if not (nObject is FClass) then Continue;
      //not match
            
      if Assigned(FItems) then         
      begin       
        for i := FItems.Count - 1 downto 0 do
        begin
          nItem := FItems[i];
          if nItem.FObject = nObject then
          begin
            Dec(FPool[nIdx].FNumLocked);
            Dec(Self.FNumLocked);

            if nItem.FUsedNum > 0 then
              Dec(nItem.FUsedNum);
            //dec counter

            if nItem.FUsedNum < 1 then
            begin
              nItem.FUsed := False;
              if nReset and Assigned(FResetOne) then
                FResetOne(nItem.FObject, nItem.FData);
              //reset
            end;

            Exit;
          end;
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2018-05-31
//Parm: ��������;����ʵ��
//Desc: ��ȡnClass.nObj����չ����
function TObjectPoolManager.GetData(const nClass: TClass;
  const nObj: TObject): Pointer;
var nIdx,i: Integer;
    nItem: PObjectPoolItem;
begin
  SyncEnter;
  try
    Result := nil;
    nIdx := FindPool(nClass);
    if nIdx < 0 then Exit;

    with FPool[nIdx] do
    begin
      for i := FItems.Count - 1 downto 0 do
      begin
        nItem := FItems[i];
        if nItem.FObject = nObj then
        begin
          Result := nItem.FData;
          Break;
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2018-05-31
//Parm: ��������;����ʵ��;��չ����
//Desc: ����nClass.nObj����չ����
function TObjectPoolManager.SetData(const nClass: TClass; const nObj: TObject;
  const nData: Pointer): Boolean;
var nIdx,i: Integer;
    nItem: PObjectPoolItem;
begin
  SyncEnter;
  try
    Result := False;
    nIdx := FindPool(nClass);
    if nIdx < 0 then Exit;

    with FPool[nIdx] do
    begin
      for i := FItems.Count - 1 downto 0 do
      begin
        nItem := FItems[i];
        if nItem.FObject = nObj then
        begin
          nItem.FData := nData;
          Result := True;
          Break;
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2017-04-15
//Parm: �б�;�Ƿ��Ѻ���ʾ
//Desc: ��������״̬���ݴ���nList��
procedure TObjectPoolManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nIdx,nLen: Integer;    
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);
    
    if not nFriendly then
    begin
      nList.Add('NumPool=' + Length(FPool).ToString);
      nList.Add('NumLocked=' +  FNumLocked.ToString);
      nList.Add('NumLockAll=' + FNumLockAll.ToString);
      Exit;
    end;
                           
    nList.Add(FixData('NumPool:', Length(FPool)));
    nList.Add(FixData('NumLocked:', FNumLocked));
    nList.Add(FixData('NumLockAll:', FNumLockAll));
                                  
    for nIdx := Low(FPool) to High(FPool) do
    with FPool[nIdx] do
    begin           
      if Assigned(FItems) then
           nLen := FItems.Count
      else nLen := 0;
      
      nList.Add('');
      nList.Add(FixData(FClass.ClassName + '.NumAll:', nLen));
      nList.Add(FixData(FClass.ClassName + '.NumLocked:', FNumLocked));
      nList.Add(FixData(FClass.ClassName + '.NumLockAll:', FNumLockAll));
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2017-04-16
//Desc: ��ȡ������������
function TObjectPoolManager.GetHealth(const nList: TStrings): TObjectHealth;
var nStr: string;
begin
  SyncEnter;
  try
    Result := hlNormal;
    if (FNumLocked >= 1000) and (Result < hlLow) then
    begin
      if Assigned(nList) then
      begin
        nStr := '����������[NumLocked: %d]����,�ȴ��ͷ�.';
        nList.Add(Format(nStr, [FNumLocked]));
      end;

      Result := hlLow;
    end;

    if (FNumLocked >= 5000) and (Result < hlBad) then
    begin
      if Assigned(nList) then
      begin
        nStr := '����������[NumLocked: %d]�ﵽ����ֵ,�����ͷ��߼�.';
        nList.Add(Format(nStr, [FNumLocked]));
      end;

      Result := hlBad;
    end;    
  finally
    SyncLeave;
  end;
end;

initialization
  //nothing
finalization
  //nothing
end.
