{*******************************************************************************
  ����: dmzn@163.com 2018-05-03
  ����: �м������ͨ��������
*******************************************************************************}
unit UMgrChannel;

{$I LibFun.inc}
interface

uses
  Classes, SysUtils, SyncObjs, uROClient, uROBinMessage, uROSOAPMessage,
  {$IFDEF MSWin}Windows, uROWinInetHttpChannel,{$ELSE}
  uRONetHttpClientChannel,{$ENDIF}{$IFDEF RO_v90}uROMessage,{$ENDIF}
  UBaseObject;

type
  TChannelMsgType = (mtBin, mtSoap);
  //��Ϣ����

  PChannelItem = ^TChannelItem;
  TChannelItem = record
    FUsed: Boolean;                //�Ƿ�ռ��
    FType: Integer;                //ͨ������
    FChannel: IUnknown;            //ͨ������

    FMsg: TROMessage;              //��Ϣ����
    {$IFDEF MSWin}
    FHttp: TROWinInetHTTPChannel;  //ͨ������
    {$ELSE}
    FHttp: TRONetHttpClientChannel;//ͨ������
    {$ENDIF}
  end;

  TChannelManager = class(TManagerBase)
  private
    FChannels: TList;
    //ͨ���б�
    FMaxCount: Integer;
    //ͨ����ֵ
    FNumLocked: Integer;
    //��������
    FFreeing: Integer;
    FClearing: Integer;
    //����״̬
  protected
    function GetCount: Integer;
    procedure SetChannelMax(const nValue: Integer);
    //���Դ���
  public
    constructor Create;
    destructor Destroy; override;
    //�����ͷ�
    class procedure RegistMe(const nReg: Boolean); override;
    //ע�������
    function LockChannel(const nType: Integer = -1;
     const nMsgType: TChannelMsgType = mtBin): PChannelItem;
    procedure ReleaseChannel(const nChannel: PChannelItem);
    //ͨ������
    procedure ClearChannel;
    //����ͨ��
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    //��ȡ״̬
    property ChannelCount: Integer read GetCount;
    property ChannelMax: Integer read FMaxCount write SetChannelMax;
    //�������
  end;

var
  gChannelManager: TChannelManager = nil;
  //ȫ��ʹ��

implementation

uses
  UManagerGroup;

const
  cYes  = $0002;
  cNo   = $0005;

constructor TChannelManager.Create;
begin
  inherited;
  FMaxCount := 5;
  FNumLocked := 0;

  FFreeing := cNo;
  FClearing := cNo;
  FChannels := TList.Create;
end;

destructor TChannelManager.Destroy;
begin
  {$IFDEF MSWin}
  InterlockedExchange(FFreeing, cYes);
  {$ELSE}
  FFreeing := cYes;
  {$ENDIF}
  ClearChannel;

  FChannels.Free;
  inherited;
end;

//Date: 2018-05-03
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TChannelManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TChannelManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TChannelManager.Create;
    gMG.FChannelManager := gMG.FManagers[nIdx].FManager as TChannelManager;
  end else
  begin
    gMG.FChannelManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gChannelManager := gMG.FChannelManager;
  //����ȫ�ֱ���
end;

//Desc: ����ͨ������
procedure TChannelManager.ClearChannel;
var nIdx: Integer;
    nItem: PChannelItem;
begin
  {$IFDEF MSWin}
  InterlockedExchange(FClearing, cYes);
  {$ELSE}
  FClearing := cYes;
  {$ENDIF} //set clear flag

  SyncEnter;
  try
    if FNumLocked > 0 then
    try
      SyncLeave;
      while FNumLocked > 0 do
        Sleep(1);
      //wait for relese
    finally
      SyncEnter;
    end;

    for nIdx:=FChannels.Count - 1 downto 0 do
    begin
      nItem := FChannels[nIdx];
      FChannels.Delete(nIdx);

      with nItem^ do
      begin
        if Assigned(FHttp) then FreeAndNil(FHttp);
        if Assigned(FMsg) then FreeAndNil(FMsg);

        if Assigned(FChannel) then FChannel := nil;
        Dispose(nItem);
      end;
    end;
  finally
    {$IFDEF MSWin}
    InterlockedExchange(FClearing, cNo);
    {$ELSE}
    FClearing := cNo;
    {$ENDIF}
    SyncLeave;
  end;
end;

//Desc: ͨ������
function TChannelManager.GetCount: Integer;
begin
  SyncEnter;
  Result := FChannels.Count;
  SyncLeave;
end;

//Desc: ���ͨ����
procedure TChannelManager.SetChannelMax(const nValue: Integer);
begin
  SyncEnter;
  FMaxCount := nValue;
  SyncLeave;
end;

//Desc: ����ͨ��
function TChannelManager.LockChannel(const nType: Integer;
 const nMsgType: TChannelMsgType): PChannelItem;
var nIdx,nFit: Integer;
    nItem: PChannelItem;
begin
  Result := nil; 
  if FFreeing = cYes then Exit;
  if FClearing = cYes then Exit;

  SyncEnter;
  try
    if FFreeing = cYes then Exit;
    if FClearing = cYes then Exit;
    nFit := -1;

    for nIdx:=0 to FChannels.Count - 1 do
    begin
      nItem := FChannels[nIdx];
      if nItem.FUsed then Continue;

      with nItem^ do
      begin
        if (nType > -1) and (FType = nType) then
        begin
          Result := nItem;
          Exit;
        end;

        if nFit < 0 then
          nFit := nIdx;
        //first idle

        if nType < 0 then
          Break;
        //no check type
      end;
    end;

    if FChannels.Count < FMaxCount then
    begin
      New(nItem);
      FChannels.Add(nItem);

      with nItem^ do
      begin
        FType := nType;
        FChannel := nil;
        {$IFDEF MSWin}
        FHttp := TROWinInetHTTPChannel.Create(nil);
        {$ELSE}
        FHttp := TRONetHttpClientChannel.Create(nil);
        {$ENDIF}

        case nMsgType of
         mtBin: FMsg := TROBinMessage.Create;
         mtSoap: FMsg := TROSOAPMessage.Create;
        end;
      end;

      Result := nItem;
      Exit;
    end;

    if nFit > -1 then
    begin
      Result := FChannels[nFit];
      Result.FType := nType;
      Result.FChannel := nil;
    end;
  finally
    if Assigned(Result) then
    begin
      Result.FUsed := True;
      Inc(FNumLocked);
    end;
    SyncLeave;
  end;
end;

//Desc: �ͷ�ͨ��
procedure TChannelManager.ReleaseChannel(const nChannel: PChannelItem);
begin
  if Assigned(nChannel) then
  begin
    SyncEnter;
    try
      nChannel.FUsed := False;
      Dec(FNumLocked);
    finally
      SyncLeave;
    end;
  end;
end;

procedure TChannelManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      nList.Add('MaxCount=' + IntToStr(FMaxCount));
      nList.Add('ChannelCount=' +  IntToStr(FChannels.Count));
      nList.Add('ChannelLocked=' + IntToStr(FNumLocked));
      Exit;
    end;

    nList.Add(FixData('MaxCount:', FMaxCount));
    nList.Add(FixData('ChannelCount:', FChannels.Count));
    nList.Add(FixData('ChannelLocked:', FNumLocked));
  finally
    SyncLeave;
  end;
end;

end.
