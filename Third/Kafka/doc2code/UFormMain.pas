{*******************************************************************************
  作者: dmzn@163.com 2022-04-01
  描述: 将kafka文档转换为开发代码
*******************************************************************************}
unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Vcl.Mask;

type
  TfFormMain = class(TForm)
    Edit1: TMemo;
    Panel1: TPanel;
    Edit2: TMemo;
    Splitter1: TSplitter;
    Btn1: TButton;
    Edit3: TLabeledEdit;
    Edit4: TLabeledEdit;
    Check1: TCheckBox;
    Check2: TCheckBox;
    procedure Btn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FListA,FListB: TStrings;
    procedure ParseCDoc();
    procedure ParseJAVADoc();
  public
    { Public declarations }
  end;

var
  fFormMain: TfFormMain;

implementation

{$R *.dfm}

procedure TfFormMain.FormCreate(Sender: TObject);
begin
  FListA := TStringList.Create;
  FListB := TStringList.Create;

  Edit1.Text := '*.JAVA文档: https://kafka.apache.org/23/javadoc/org/' +
  'apache/kafka/clients/producer/ProducerConfig.html' + #13#10#13#10 +
  '*.C库文档: https://github.com/edenhill/librdkafka.git/trunk/CONFIGURATION.MD';
end;

procedure TfFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FListA.Free;
  FListB.Free;
end;

procedure TfFormMain.Btn1Click(Sender: TObject);
begin
  if Edit1.Lines.Count < 1 then
    raise Exception.Create('没有需要解析的文档');
  //xxxxx

  try
    if Check2.Checked then
         ParseCDoc()
    else ParseJAVADoc();
  except
    on nErr: Exception do
    begin
      Edit2.Text := nErr.Message;
    end;
  end;
end;

//------------------------------------------------------------------------------
//Date: 2022-04-02
//Desc: 解析C库文档
{* 文档格式如下
Property                                 | C/P |
-----------------------------------------|-----|
builtin.features                         |  *  |
client.id                                |  *  |
metadata.broker.list                     |  *  |
*}
procedure TfFormMain.ParseCDoc;
const
  KeyTag1 = '|  *  |'; //both
  KeyTag2 = '|  P  |'; //Producer
  KeyTag3 = '|  C  |'; //Consumer
  KeyNew  = ':::';
  KeyLen  = Length(KeyNew);
var nStr,nKey,nKeys,nType,nTypes: string;
    nIdx,nPos,nLen,nKeyNum,nLineWidth: Integer;
begin
  FListA.Clear;
  FListB.Clear;
  nLen := StrToInt(Edit3.Text); //空格对齐个数
  nLineWidth := StrToInt(Edit4.Text); //单行字符数

  Edit2.Text := '{*Global configuration properties*}';
  nKeyNum := 0;
  nKeys := '';
  nTypes := '';

  for nIdx:=0 to Edit1.Lines.Count-1 do
  begin
    nStr := Trim(Edit1.Lines[nIdx]);
    if nStr = '' then Continue;
    nType := '';

    nPos := Pos(KeyTag1, nStr);
    if nPos > 1 then nType := 'B';
    //find tag

    if nType = '' then
    begin
      nPos := Pos(KeyTag2, nStr);
      if nPos > 1 then nType := 'P';
    end;

    if nType = '' then
    begin
      nPos := Pos(KeyTag3, nStr);
      if nPos > 1 then nType := 'C';
    end;

    if nType = '' then Continue;
    //no tag
    if nTypes <> '' then
    begin
      if Length(nTypes + ', ' + nType) > nLineWidth then //超长截断
      begin
        FListA.Add(nTypes + ',');
        nTypes := '  ' + nType;
      end else nTypes := nTypes + ', ' + nType;
    end else nTypes := nType;

    nStr := Trim(Copy(nStr, 1, nPos - 1));
    nKey := UpperCase(StringReplace(nStr, '.', '_', [rfReplaceAll, rfIgnoreCase]));
    //key name

    Inc(nKeyNum);
    if nKeys <> '' then
    begin
      if Length(nKeys + ', ' + nKey) > nLineWidth then //超长截断
      begin
        FListB.Add(nKeys + ',');
        nKeys := '  ' + nKey;
      end else nKeys := nKeys + ', ' + nKey;
    end else nKeys := nKey;

    nKey := nKey + StringOfChar(' ', nLen - Length(nKey));
    Edit2.Lines.Add(nKey + '= ''' + nStr + ''';');
  end;

  if nKeys <> '' then
    FListB.Add(nKeys);
  nKeys := Format('ConfigKeys: array[1..%d] of string = (%s);', [nKeyNum, FListB.Text]);

  if nTypes <> '' then
    FListA.Add(nTypes);
  nTypes := Format('ConfigTypes: array[1..%d] of TKKConfigType = (%s);', [nKeyNum, FListA.Text]);

  Edit2.Lines.Add('');
  Edit2.Lines.Add(nKeys);
  Edit2.Lines.Add('');
  Edit2.Lines.Add(nTypes);
  Edit2.Lines.Add('{*Configuration properties*}');
end;

//Date: 2022-04-01
//Desc: 解析文档
{* 文档格式如下:
  static String	ACKS_CONFIG
  acks
  static String	BATCH_SIZE_CONFIG
  batch.size
*}
procedure TfFormMain.ParseJAVADoc();
const
  KeyTag1 = 'static String';
  KeyTag2 = 'static final String';
  KeyNew  = ':::';
  KeyLen  = Length(KeyNew);

var nStr,nKey: string;
    nIsKey: Boolean;
    nIdx,nPos,nLen,nKeyNum,nLineWidth: Integer;
begin
  FListA.Text := StringReplace(Edit1.Text, KeyTag1, KeyNew,
    [rfReplaceAll, rfIgnoreCase]);
  FListA.Text := StringReplace(Edit1.Text, KeyTag2, KeyNew,
    [rfReplaceAll, rfIgnoreCase]);
  //change tag

  for nIdx:=FListA.Count-1 downto 0 do
  begin
    if Trim(FListA[nIdx]) = '' then
    begin
      FListA.Delete(nIdx);
      Continue;
    end; //删除空行

    if Check1.Checked and (Pos(KeyNew, FListA[nIdx]) > 0) and
       (nIdx < FListA.Count-1) then
    begin
      FListA[nIdx+1] := FListA[nIdx] + FListA[nIdx+1];
      FListA.Delete(nIdx);
      Continue;
    end; //与下一行合并
  end;

  FListB.Clear;
  nLen := StrToInt(Edit3.Text); //空格对齐个数
  nLineWidth := StrToInt(Edit4.Text); //单行字符数

  Edit2.Text := '{*org.apache.kafka.clients.producer.ProducerConfig*}';
  nKeyNum := 0;
  nKey := '';
  nStr := '';

  for nIdx:=0 to FListA.Count-1 do
  begin
    nPos := Pos(KeyNew, FListA[nIdx]);
    //find key
    nIsKey := nPos > 0;
    
    if nIsKey then
    begin
      nStr := Trim(Copy(FListA[nIdx], nPos + KeyLen, MaxInt));
      //get key
    end else

    if nStr <> '' then
    begin
      if not nIsKey then
      begin
        Inc(nKeyNum);
        if nKey <> '' then
        begin
          if Length(nKey + ', ' + nStr) > nLineWidth then //超长截断
          begin
            FListB.Add(nKey + ',');
            nKey := '  ' + nStr;
          end else nKey := nKey + ', ' + nStr;
        end else nKey := nStr;

        nStr := nStr + StringOfChar(' ', nLen - Length(nStr));
        Edit2.Lines.Add(nStr + '= ''' + Trim(FListA[nIdx]) + ''';');
      end;

      nStr := '';
    end;
  end;

  if nKey <> '' then
    FListB.Add(nKey);
  nKey := Format('ConfigKeys: array[1..%d] of string = (%s);', [nKeyNum, FListB.Text]);

  Edit2.Lines.Add('');
  Edit2.Lines.Add(nKey);
  Edit2.Lines.Add('{*The producer configuration keys*}');
end;

end.
