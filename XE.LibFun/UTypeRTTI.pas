{*******************************************************************************
  作者: dmzn@163.com 2017-02-20
  描述: 使用运行时序列化类和对象 
*******************************************************************************}
unit UTypeRTTI;

{$I LibFun.inc}
interface

uses
  System.Classes, System.Rtti, System.SysUtils, System.TypInfo, ULibFun;

type
  TRecordSerializer<T> = class
  strict private     
    class procedure MakeData(const nPrefix,nField,nVal: string;
      const nList: TStrings);
    class function MakePrefix(const nFirst,nNext: string): string;
    //格式化内容
    class procedure EncodeArray(const nCtx: TRttiContext; 
      const nInstance: Pointer; const nField: TRTTIArrayType; 
      const nList: TStrings; const nPrefix: string = '');
    //序列化数组
    class procedure EncodeEnum(const nCtx: TRttiContext; 
      const nInstance: Pointer; const nField: TRttiField; 
      const nList: TStrings; const nPrefix: string = '');
    //序列化枚举
    class procedure EncodeField(const nCtx: TRttiContext; 
      const nInstance: Pointer; const nField: TRttiType;
      const nList: TStrings; const nPrefix: string = '');
    //序列化Field
  public
    const
      sNoSuport = 'not support type.';

    class function Encode(const nRecord: T): string;
    //序列化Record
    class procedure Decode(const nRecord: T; const nData: string);
    //反序列化Record 
  end;

implementation

//Date: 2017-03-15
//Parm: 一级;二级 
//Desc: 构建nFirst.nNext结构的多级前缀
class function TRecordSerializer<T>.MakePrefix(const nFirst,
  nNext: string): string;
begin
  if nFirst = '' then
       Result := nNext
  else Result := nFirst + '.' + nNext;
end;

//Date: 2017-03-15
//Parm: 前缀,字段,值;是否分割 
//Desc: 构建nPrefix.nField=nVal的内容
class procedure TRecordSerializer<T>.MakeData(const nPrefix,nField,nVal: string; 
  const nList: TStrings);
begin
  if nPrefix = '' then
       nList.Add(nField + '=' + nVal)
  else nList.Add(nPrefix + '.' + nField + '=' + nVal)
end;

//Date: 2017-03-15
//Parm: 实例;数组字段;前缀
//Desc: 读取nField数组中的值
class procedure TRecordSerializer<T>.EncodeArray(const nCtx: TRttiContext; 
  const nInstance: Pointer; const nField: TRTTIArrayType; 
  const nList: TStrings; const nPrefix: string);
var nIdx: Integer;
    nAryIx: array of integer;
    nMinIx: array of integer;
    nMaxIx: array of integer;
    nOType: TRttiOrdinalType;
begin 
  SetLength(nAryIx, nField.DimensionCount);
  SetLength(nMinIx, nField.DimensionCount);
  SetLength(nMaxIx, nField.DimensionCount);

  for nIdx:=0 to nField.DimensionCount-1 do
  begin
    nOType := nField.Dimensions[nIdx] as TRttiOrdinalType;
    if nOType = nil then
    begin
      MakeData(nPrefix, nField.Name, sNoSuport, nList);
      Exit;
    end;
    
    nAryIx[nIdx] := nOType.MinValue;
    nMinIx[nIdx] := nOType.MinValue;
    nMaxIx[nIdx] := nOType.MaxValue;
  end;

  for nIdx := 0 to nField.TotalElementCount do
  begin

  end;  
    
            {

        for i := 0 to LStaticArray.TotalElementCount - 1 do
        begin
          Serialize(Name + IxToString(LArrayIx),
            GetValue( PByte(thing.GetReferenceToRawData) +
              LStaticArray.ElementType.TypeSize * i,
              LStaticArray.ElementType),
            sList,false);
          IncIx(LArrayIx, LArrayMinIx, LArrayMaxIx);
        end;
      }  
end;

//Date: 2017-03-17
//Parm: 实例;枚举字段;前缀
//Desc: 读取nField字段的值
class procedure TRecordSerializer<T>.EncodeEnum(const nCtx: TRttiContext;
  const nInstance: Pointer; const nField: TRttiField; const nList: TStrings;
  const nPrefix: string);
var nIdx: Integer;
    nVal: TValue;
begin
  nVal := nField.GetValue(nInstance);
  MakeData(nPrefix, nField.Name, nVal.ToString, nList);
end;

//Date: 2017-03-15
//Parm: 实例;记录类型;前缀
//Desc: 读取nField中每项的名称和值
class procedure TRecordSerializer<T>.EncodeField(const nCtx: TRttiContext;
  const nInstance: Pointer; const nField: TRttiType; 
  const nList: TStrings; const nPrefix: string);
var nVal: TValue;
    nRF: TRttiField;
begin
  for nRF in nField.GetFields do
  begin
    if nRF.FieldType = nil then
    begin
      MakeData(nPrefix, nRF.Name, sNoSuport, nList);
      Continue;
    end;
    
    {
     Rtti information is insufficient for static array members of records.
     Link: http://qc.embarcadero.com/wc/qcmain.aspx?d=78560
     Comments: Anonymous types don't have symbols and don't have full RTTI.
    }
  
    nVal := nRF.GetValue(nInstance);
    //field value
    
    case nRF.FieldType.TypeKind of
     tkRecord:
      begin   
        Continue;
        //do later
      end;
     tkInt64:
      begin
        MakeData(nPrefix, nRF.Name, IntToStr(nVal.AsInt64), nList);
        //int64
      end;
     tkInteger:
      begin
        MakeData(nPrefix, nRF.Name, IntToStr(nVal.AsInteger), nList);
        //integer
      end;
      
     tkFloat:
      with TDateTimeHelper do
      begin
        if nVal.TypeInfo = TypeInfo(TDate) then
        begin
          MakeData(nPrefix, nRF.Name, Date2Str(nVal.AsExtended), nList);
          //date
        end else
        
        if nVal.TypeInfo = TypeInfo(TTime) then
        begin
          MakeData(nPrefix, nRF.Name, Time2Str(nVal.AsExtended), nList);
          //time
        end else

        if nVal.TypeInfo = TypeInfo(TDateTime) then
        begin
          MakeData(nPrefix, nRF.Name, DateTime2Str(nVal.AsExtended), nList);
          //datetime
        end else         
        begin
          MakeData(nPrefix, nRF.Name, FloatToStr(nVal.AsExtended), nList);
          //float
        end;
      end;       
     tkArray: 
      begin
        EncodeArray(nCtx, nInstance, nRF.FieldType as TRTTIArrayType, nList,
                    MakePrefix(nPrefix, nRF.Name));
        //array
      end;
     tkClass:
      begin    
        MakeData(nPrefix, nRF.Name, 'no support', nList);
      end;
     tkSet: 
      begin
        MakeData(nPrefix, nRF.Name, nVal.ToString, nList);
        //set
      end;
     tkEnumeration:
      begin
        MakeData(nPrefix, nRF.Name, nVal.ToString, nList);
        //enumeration
      end;
            
     tkChar,
     tkWChar,
     tkString,
     tkLString,
     tkWString,
     tkUString:
      begin
        MakeData(nPrefix, nRF.Name, nVal.ToString, nList);
        //string
      end else
      begin
        MakeData(nPrefix, nRF.Name, sNoSuport, nList);
        //unsupport
      end;
    end; 
  end;

  for nRF in nField.GetFields do
  begin
    if Assigned(nRF.FieldType) and (nRF.FieldType.TypeKind = tkRecord) then   
      EncodeField(nCtx, nInstance, 
                  nRF.FieldType, nList, MakePrefix(nPrefix, nRF.Name));
    //record
  end;
end;

//Date: 2017-03-14
//Parm: 记录实例;命名前缀
//Desc: 序列化nRecord为字符串
class function TRecordSerializer<T>.Encode(const nRecord: T): string;
var nCtx: TRttiContext;
    nType: TRttiType;
    nList: TStrings; 
begin    
  nList := TStringList.Create;
  nCtx := TRttiContext.Create;
  try
    nType := nCtx.GetType(TypeInfo(T));
    if nType.TypeKind <> tkRecord then
      raise Exception.Create('TRecordSerializer only support Record Type.');
    //xxxxx
    
    EncodeField(nCtx, @nRecord, nType, nList);
    Result := nList.Text;
  finally
    nList.Free;
    nCtx.Free;
  end;
end;

//Date: 2017-03-14
//Parm: 记录;序列化数据
//Desc: 将nData赋值给nRecord结构
class procedure TRecordSerializer<T>.Decode(const nRecord: T;
  const nData: string);
begin

end;

end.
