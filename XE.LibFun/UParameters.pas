{*******************************************************************************
  作者: dmzn@163.com 2019-02-22
  描述: 参数配置管理器

  备注:
  *.参数文档结构:
    <description>参数结构描述</description>
    <parameters>参数数据</parameters>
  *.
*******************************************************************************}
unit UParameters;

interface

uses
  System.Classes, System.SysUtils, NativeXml, UBaseObject;

type
  TParamFieldType = (ptSub, ptAttribute);
  //类型: 属性节点;子节点

  TParamFieldValue = (pdString, pdInt, pdFloat);
  //数据类型: 字符;整数;浮点

  TParamDataEncode = (peNone, peBase64);
  //数据编码: 无;Base64

  PParamFieldData = ^TParamFieldData;
  TParamFieldData = record
    FName     : string;                       //数据名称
    FValue    : string;                       //取值
    FEncode   : TParamDataEncode;             //编码方式
  end;
  TParamFieldDatas = array of TParamFieldData;

  PParamFieldItem = ^TParamFieldItem;
  TParamFieldItem = record
    FName     : string;                       //参数项名
    FDesc     : string;                       //项描述
    FFrom     : string;                       //若不为空,则取其它参数项
    FData     : string;                       //固定值
    FType     : TParamFieldType;              //项类型(存为XML时体现)
    FValue    : TParamFieldValue;             //值类型
    FValueS   : string;                       //字符串
    FValueI   : Integer;                      //整数值
    FValueF   : Double;                       //浮点值
  end;
  TParamFieldItems = array of TParamFieldItem;

  PParamItem = ^TParamItem;
  TParamItem = record
    FID       : string;                       //参数标识
    FName     : string;                       //参数名称
    FRoot     : string;                       //节点名(存为XML时体现)
    FDetail   : string;                       //子节点(存为XML时体现)
    FEnable   : Boolean;                      //是否有效
    FFields   : TParamFieldItems;             //参数明细项
  end;
  TParamItems = array of TParamItem;

  TParameterManager = class(TManagerBase)
  protected
    procedure ParseTemplate(const nXML: TNativeXml);
    {*解析模板*}
    procedure CombineParameterWithTemplate(const nParam,nTemplate: Integer);
    {*合并数据*}
  public
    Parameters: TParamItems;
    Template: TParamItems;
    TemplateData: TParamFieldDatas;
    {*模板&参数*}
    constructor Create;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册管理器*}
    procedure LoadTemplate(const nData: string);
    procedure LoadTemplateFromFile(const nFile: string);
    {*载入模板*}
    function FindTemplate(const nID: string): Integer;
    function FindTemplateData(const nName: string): Integer;
    function FindParameters(const nID: string): Integer;
    {*检索数据*}
    procedure LoadParameters(const nData: string);
    procedure LoadParametersFromFile(const nFile: string);
    procedure SaveParametersToFile(const nFile: string);
    {*读写参数*}
    procedure AddParamItem(const nID,nName,nTemplete: string);
    procedure DelParamItem(const nID: string);
    {*增减参数*}
  end;

implementation

uses
  UManagerGroup, ULibFun;

constructor TParameterManager.Create;
begin
  inherited;
  SetLength(Parameters, 0);
  SetLength(Template, 0);
  SetLength(TemplateData, 0);
end;

//Date: 2019-02-25
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TParameterManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TParameterManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TParameterManager.Create;
    gMG.FParameterManager := gMG.FManagers[nIdx].FManager as TParameterManager;
  end else
  begin
    gMG.FParameterManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;
end;

//Date: 2019-02-25
//Parm: 模板数据
//Desc: 解析模板数据
procedure TParameterManager.LoadTemplate(const nData: string);
var nXML: TNativeXml;
    nStream: TStringStream;
begin
  nXML := nil;
  nStream := nil;
  try
    nStream := TStringStream.Create;
    nStream.WriteString(nData);

    nXML := TNativeXml.Create(nil);
    nXML.LoadFromStream(nStream);
    ParseTemplate(nXML);
  finally
    nStream.Free;
    nXML.Free;
  end;
end;

//Date: 2019-02-25
//Parm: 模板文件
//Desc: 载入模板文件
procedure TParameterManager.LoadTemplateFromFile(const nFile: string);
var nXML: TNativeXml;
begin
  nXML := nil;
  try
    nXML := TNativeXml.Create(nil);
    nXML.LoadFromFile(nFile);
    ParseTemplate(nXML);
  finally
    nXML.Free;
  end;
end;

//Date: 2019-02-25
//Parm: XML对象
//Desc: 解析模板数据
procedure TParameterManager.ParseTemplate(const nXML: TNativeXml);
var nStr: string;
    nIdxA,nIdxB,nIntA,nIntB: Integer;
    nRoot,nNode,nField: TXmlNode;
begin
  SetLength(Template, 0);
  SetLength(TemplateData, 0);
  nRoot := nXML.Root.NodeByNameR('Params');

  for nIdxA := 0 to nRoot.NodeCount-1 do
  begin
    nNode := nRoot.Nodes[nIdxA];
    if CompareText(string(nNode.Name), 'ParamItem') <> 0 then Continue;

    nIntA := Length(Template);
    SetLength(Template, nIntA + 1);
    with Template[nIntA] do
    begin
      SetLength(FFields, 0);
      FID     := string(nNode.AttributeValueByName['Name']);
      FName   := string(nNode.AttributeValueByName['DESC']);
      FRoot   := FID;
      FDetail := string(nNode.AttributeValueByName['Detail']);

      for nIdxB := 0 to nNode.NodeCount-1 do
      begin
        nField := nNode.Nodes[nIdxB];
        if CompareText(string(nField.Name), 'Field') <> 0 then Continue;

        nIntB := Length(FFields);
        SetLength(FFields, nIntB + 1);
        with FFields[nIntB] do
        begin
          FName := string(nField.AttributeValueByName['Name']);
          FDesc := string(nField.AttributeValueByName['Desc']);
          FFrom := string(nField.AttributeValueByName['From']);
          FData := string(nField.AttributeValueByName['Data']);

          nStr := string(nField.AttributeValueByName['Type']);
          if CompareText(nStr, 'attribution') = 0 then
               FType := ptAttribute
          else FType := ptSub;

          nStr := string(nField.AttributeValueByName['Value']);
          if CompareText(nStr, 'integer') = 0 then
            FValue := pdInt
          else if CompareText(nStr, 'float') = 0 then
            FValue := pdFloat
          else FValue := pdString;
        end;
      end;
    end;
  end;

  nRoot := nXML.Root.NodeByNameR('FieldData');
  for nIdxA := nRoot.NodeCount-1 downto 0 do
  begin
    nNode := nRoot.Nodes[nIdxA];
    nIntA := Length(TemplateData);
    SetLength(TemplateData, nIntA + 1);

    with TemplateData[nIntA] do
    begin
      FName := string(nNode.AttributeValueByName['Name']);
      FValue := nNode.ValueUnicode;

      nStr := string(nNode.AttributeValueByName['Encode']);
      if CompareText(nStr, 'Base64') = 0 then
           FEncode := peBase64
      else FEncode := peNone;
    end;
  end;
end;

//Date: 2019-02-28
//Parm: 模板标识
//Desc: 检索模板
function TParameterManager.FindTemplate(const nID: string): Integer;
var nIdx: Integer;
begin
  Result := -1;
  for nIdx := Low(Template) to High(Template) do
   if CompareText(nID, Template[nIdx].FID) = 0 then
   begin
     Result := nIdx;
     Break;
   end;
end;

//Date: 2019-03-01
//Parm: 模板数据
//Desc: 检索模板数据
function TParameterManager.FindTemplateData(const nName: string): Integer;
var nIdx: Integer;
begin
  Result := -1;
  for nIdx := Low(TemplateData) to High(TemplateData) do
   if CompareText(nName, TemplateData[nIdx].FName) = 0 then
   begin
     Result := nIdx;
     Break;
   end;
end;

//Date: 2019-02-28
//Parm: 数据标识
//Desc: 检索数据
function TParameterManager.FindParameters(const nID: string): Integer;
var nIdx: Integer;
begin
  Result := -1;
  for nIdx := Low(Parameters) to High(Parameters) do
   if CompareText(nID, Parameters[nIdx].FID) = 0 then
   begin
     Result := nIdx;
     Break;
   end;
end;

procedure TParameterManager.LoadParameters(const nData: string);
begin

end;

procedure TParameterManager.LoadParametersFromFile(const nFile: string);
begin

end;

procedure TParameterManager.SaveParametersToFile(const nFile: string);
begin

end;

//Date: 2019-02-28
//Parm: 标识;名称;模板标识
//Desc: 添加参数项
procedure TParameterManager.AddParamItem(const nID, nName, nTemplete: string);
var nIdx,nInt: Integer;
begin
  nIdx := FindTemplate(nTemplete);
  if nIdx < 0 then
    raise Exception.Create(Format('节点类型为[ %s ]不存在.', [nTemplete]));
  //xxxxx

  nInt := FindParameters(nID);
  if nInt < 0 then
  begin
    nInt := Length(Parameters);
    SetLength(Parameters, nInt + 1);
  end else
  begin
    if Parameters[nInt].FRoot <> Template[nIdx].FRoot then
      raise Exception.Create(Format('节点[ %s ]已存在.', [nID]));
    //xxxxx
  end;

  with Parameters[nInt] do
  begin
    FID := nID;
    FName := nName;
    FEnable := True;
    FRoot := Template[nIdx].FRoot;
    FDetail := Template[nIdx].FDetail;
  end;

  CombineParameterWithTemplate(nInt, nIdx);
  //合并模板数据
end;

//Date: 2019-02-28
//Parm: 数据索引;模板索引
//Desc: 依据nTemplate生成nParam的数据
procedure TParameterManager.CombineParameterWithTemplate(const nParam,
  nTemplate: Integer);
var i,nIdx: Integer;
    nFields: TParamFieldItems;
begin
  with Parameters[nParam] do
  begin
    nFields := Template[nTemplate].FFields;
    for nIdx := Low(nFields) to High(nFields) do
     for i := Low(FFields) to High(FFields) do
      if FFields[i].FName = nFields[nIdx].FName then
      begin
        nFields[nIdx] := FFields[i];
        Break;
      end;
    //xxxxx

    FFields := nFields;
  end;
end;

//Date: 2019-03-01
//Parm: 标识
//Desc: 删除nID参数
procedure TParameterManager.DelParamItem(const nID: string);
var nIdx: Integer;
begin
  nIdx := FindParameters(nID);
  if nIdx >= 0 then
    Parameters[nIdx].FEnable := False;
  //xxxxx
end;

end.
