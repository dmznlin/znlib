unit UModbusRegisterList;

{==============================================================================}

{$I ModLink.inc}

{==============================================================================}

interface

{==============================================================================}

uses
  Contnrs;

{==============================================================================}

type
  TModbusRegister = class
  private
    FAddress: Word;
    FMaxValue: Word;
    FMinValue: Word;
    FReadable: Boolean;
    FValue: Word;
    FWritable: Boolean;
  public
    constructor Create;
    property Address: Word read FAddress write FAddress;
    property MaxValue: Word read FMaxValue write FMaxValue;
    property MinValue: Word read FMinValue write FMinValue;
    property Readable: Boolean read FReadable write FReadable;
    property Value: Word read FValue write FValue;
    property Writable: Boolean read FWritable write FWritable;
  end;

  TModbusRegisterList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TModbusRegister;
    procedure SetItem(Index: Integer; ARegister: TModbusRegister);
  public
    function Add(ARegister: TModbusRegister): Integer;
    function Remove(ARegister: TModbusRegister): Integer;
    function IndexOf(ARegister: TModbusRegister): Integer;
    property Items[Index: Integer]: TModbusRegister read GetItem write SetItem; default;
  end;

{==============================================================================}

implementation

{==============================================================================}
{ TModbusRegister                                                              }
{==============================================================================}

constructor TModbusRegister.Create;
begin
  FMaxValue := High(FMaxValue);
end;

{==============================================================================}
{ TModbusRegisterList                                                          }
{==============================================================================}

function TModbusRegisterList.Add(ARegister: TModbusRegister): Integer;
begin
  Result := inherited Add(ARegister);
end;

{==============================================================================}

function TModbusRegisterList.GetItem(Index: Integer): TModbusRegister;
begin
  Result := TModbusRegister(inherited Items[Index]);
end;

{==============================================================================}

function TModbusRegisterList.IndexOf(ARegister: TModbusRegister): Integer;
begin
  Result := inherited IndexOf(ARegister);
end;

{==============================================================================}

function TModbusRegisterList.Remove(ARegister: TModbusRegister): Integer;
begin
  Result := inherited Remove(ARegister);
end;

{==============================================================================}

procedure TModbusRegisterList.SetItem(Index: Integer;
  ARegister: TModbusRegister);
begin
  inherited Items[Index] := ARegister;
end;

{==============================================================================}

end.
