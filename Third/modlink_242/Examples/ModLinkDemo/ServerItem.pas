unit ServerItem;

{$I ModLink.inc}

interface

//--------------------------------------------------------------------------------------------------

uses
  { Delphi } Classes;

//--------------------------------------------------------------------------------------------------

type
  TItemKind = (
    ikCoil,
    ikDiscreteInput,
    ikHoldingRegister,
    ikInputRegister
  );

  PServerItem = ^TServerItem;
  TServerItem = packed record
    Addr: Word;
    Kind: TItemKind;
    Value,
    MinValue,
    MaxValue: Word;
    Writeable: Boolean;
  end;

//--------------------------------------------------------------------------------------------------

function CreateServerItem(
  aAddr: Word;
  aKind: TItemKind;
  aValue,
  aMinValue,
  aMaxValue: Word;
  aWriteable: Boolean): PServerItem;

procedure DestroyServerItem(aServerItem: PServerItem);
procedure LoadServerItem(aServerItem: PServerItem; aStream: TStream);
procedure SaveServerItem(aServerItem: PServerItem; aStream: TStream);

//--------------------------------------------------------------------------------------------------

implementation

uses
  { Windows } Windows;

//--------------------------------------------------------------------------------------------------

function CreateServerItem(
  aAddr: Word;
  aKind: TItemKind;
  aValue,
  aMinValue,
  aMaxValue: Word;
  aWriteable: Boolean): PServerItem;
begin
  New(Result);
  ZeroMemory(Result, SizeOf(TServerItem));
  with Result^ do
  begin
    Addr := aAddr;
    Kind := aKind;
    Value := aValue;
    MinValue := aMinValue;
    MaxValue := aMaxValue;
    Writeable := aWriteable;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure DestroyServerItem(aServerItem: PServerItem);
begin
  if Assigned(aServerItem) then
    Dispose(aServerItem);
end;

//--------------------------------------------------------------------------------------------------

procedure LoadServerItem(aServerItem: PServerItem; aStream: TStream);
begin
  aStream.ReadBuffer(aServerItem^, SizeOf(TServerItem));
end;

//--------------------------------------------------------------------------------------------------

procedure SaveServerItem(aServerItem: PServerItem; aStream: TStream);
begin
  aStream.WriteBuffer(aServerItem^, SizeOf(TServerItem));
end;

//--------------------------------------------------------------------------------------------------

end.
